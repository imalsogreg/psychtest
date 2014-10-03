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


{-
-- Try 1 :: Trial is a RWST, where the writer is a (State,Result) assoc list
-- Ran into problems because (m a) >>= (a -> m b) -> m b was trying to make
-- an assoc list like [(s,a),(s,b)] - heterogeneous!

newtype Trial p s m a = Trial {
  runTr :: Monad m => RWST p (TestResults s a) s m a
  }

instance Monad m => Functor (Trial p s m) where
  fmap f ma = Trial (RWST (\p0 s0 -> do
                              (a, s, w) <- runRWST (runTr ma) p0 s0
                              return (f a, s, fmap f w)))

instance Monad m => Applicative (Trial p s m) where
  pure a    = Trial ( RWST (\_ s -> return (a, s, TestResults [(s,a)])))
--  (Trial f')  <*> (Trial a') = let toF = runRWST f'
--                                   toA = runRWST a'

instance (Monad m) => Monad (Trial p s m) where
  return           = pure
  a'@(Trial a) >>= f = Trial $ RWST (\p0 s0 -> do
                                      (a'',s,w) <- runRWST a p0 s0
                                      let (Trial b) = f a''
                                      (b'',s',w') <- runRWST b p0 s
                                      return (b'',s', w <> w')
                                  )

newtype TestResults s a = TestResults { runResults :: [(s,a)] }
                        deriving (Monoid)


-}


newtype Trial p s m a = Trial { runTrial :: Monad m => RWST p () s m (Maybe a) }

instance Monad m => Functor (Trial p s m) where
  fmap f ma = Trial (RWST (\p0 s0 -> do
                              (a, s, ()) <- runRWST (runTrial ma) p0 s0
                              return (fmap f a, s, ())))

instance Monad m => Applicative (Trial p s m) where
  pure a    = Trial ( RWST (\_ s -> return (Just a, s, ())))
  (Trial f')  <*> (Trial a') = Trial $ RWST (\p0 s0 -> do
                                                (f'',_ ,()) <- runRWST f' p0 s0
                                                (a'',sa,()) <- runRWST a' p0 s0
                                                return (f'' <*> a'', sa, ()))

instance (Monad m) => Monad (Trial p s m) where
  return           = pure
  (Trial a) >>= f = Trial $ RWST (\p0 s0 -> do
                                     (a'',s, ()) <- runRWST a p0 s0
                                     case fmap f a'' of
                                       Just (Trial b) -> runRWST b p0 s
                                       Nothing -> return (Nothing,s,())
                                 )



newtype TestResults s a = TestResults { runResults :: [(s,a)] }
                        deriving (Monoid)


instance Functor (TestResults s) where
  fmap f (TestResults rs) = TestResults (fmap (\(x,y) -> (x, f y)) rs)


------------------------------------------------------------------------------
class Test t where
  type ParamsType  t
  type StateType   t
  type ResultsType t
  initialState :: StateType t
  runTest ::
    Monad m => [Trial (ParamsType t) (StateType t) m (Maybe (ResultsType t))]
            -> m (TestResults (StateType t) (ResultsType t))



{- Not yet :) Let's run in gloss for now (or SDL?)
------------------------------------------------------------------------------
class TestBackend b r where
  type RenderResult :: b -> r
  renderStimulus    :: (Stimulus s) => s -> IO r
  stepBackend       :: 
-}


{-
------------------------------------------------------------------------------
data Test r = Test { testName    :: Text            -- Maybe not this way
                   , testPhases  :: [TestPhase]
                   , testResults :: IO r
                   }
-}
