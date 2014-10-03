{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE RankNTypes                 #-}

module PsychTest where

import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Monad.RWS.Class
import Control.Monad.RWS.Strict
import Control.Monad.Trans.State

import Data.Maybe
import Data.Monoid
import Data.Text


newtype Trial p s m a = Trial { runTrial :: Monad m => RWST p () s m (Maybe a) }

instance Monad m => Functor (Trial p s m) where
  fmap f ma = Trial (RWST (\p0 s0 -> do
                              (a, s, ()) <- runRWST (runTrial ma) p0 s0
                              return (fmap f a, s, ())))

------------------------------------------------------------------------------
instance Monad m => Applicative (Trial p s m) where
  pure a    = Trial ( RWST (\_ s -> return (Just a, s, ())))
  (Trial f')  <*> (Trial a') =
    Trial $ RWST (\p0 s0 -> do
                     (f'',_ ,()) <- runRWST f' p0 s0
                     (a'',sa,()) <- runRWST a' p0 s0
                     return (f'' <*> a'', sa, ()))

------------------------------------------------------------------------------
instance (Monad m) => Monad (Trial p s m) where
  return           = pure
  (Trial a) >>= f = Trial $ RWST (\p0 s0 -> do
                                     (a'',s, ()) <- runRWST a p0 s0
                                     case fmap f a'' of
                                       Just (Trial b) -> runRWST b p0 s
                                       Nothing -> return (Nothing,s,()))

------------------------------------------------------------------------------
newtype TestResults s a = TestResults { getResults :: [(s,a)] }
                        deriving (Monoid)

------------------------------------------------------------------------------
instance Functor (TestResults s) where
  fmap f (TestResults rs) = TestResults (fmap (\(x,y) -> (x, f y)) rs)


------------------------------------------------------------------------------
runTrials :: Monad m => p -> s -> [Trial p s m a] -> m (TestResults s a)
runTrials p s trials = foldM f (s, TestResults []) trials >>= return . snd
  where f (foldS,  res) (Trial t) = do
           (r', s', ()) <- runRWST t p foldS
           case r' of
             Nothing -> return (s', res)
             Just r  -> return (s', res <> TestResults [(s,r)])

------------------------------------------------------------------------------
class Test t where
  type ParamsType  t
  type StateType   t
  type ResultsType t
  initialState :: StateType t




{- Not yet :) Let's specailize on gloss for now (or SDL?)
------------------------------------------------------------------------------
class TestBackend b r where
  type RenderResult :: b -> r
  renderStimulus    :: (Stimulus s) => s -> IO r
  stepBackend       :: 
-}
