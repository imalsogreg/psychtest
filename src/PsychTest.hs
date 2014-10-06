{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module PsychTest where

------------------------------------------------------------------------------
import Control.Applicative
--import Control.Monad.Trans.Reader
--import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Monad.RWS.Class
import Control.Monad.RWS.Strict
--import Control.Monad.Trans.State
------------------------------------------------------------------------------
import Data.Maybe
import Data.Monoid
import Data.Text


------------------------------------------------------------------------------
newtype Trial p s m a = Trial {
  getTrial :: Monad m => RWST p () s m (Maybe a)
  }


------------------------------------------------------------------------------
newtype TestResults s a = TestResults { getResults :: [(s,a)] }
                        deriving (Monoid)


------------------------------------------------------------------------------
runTrials :: Monad m => p -> s -> [Trial p s m a] -> m (TestResults s a)
runTrials p s trials = foldM f (s, TestResults []) trials >>= return . snd
  where f (foldS,  res) (Trial t :: Trial p s m a) = do
           (r', s', ()) <- runRWST t p foldS
--           let s'' = betweenTrials r' s' :: StateType (Trial p s m a)
           case r' of
             Nothing -> return (s', res)
             Just r  -> return (s', res <> TestResults [(s,r)])


------------------------------------------------------------------------------
class Test t where
  type ParamsType  t
  type StateType   t
  type ResultsType t
  initialState  :: StateType t


------------------------------------------------------------------------------
instance Monad m => Functor (Trial p s m) where
  fmap f (Trial a') = Trial (RWST (\p0 s0 -> do
                              (a, s, ()) <- runRWST a' p0 s0
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

instance (MonadIO m) => MonadIO (Trial p s m) where
  liftIO ma = Trial $ RWST (\_ s0 -> do
                               a <- liftIO ma
                               return (Just a, s0, ()))


instance (Monad m) => MonadReader p (Trial p s m) where
  ask = Trial $ RWST (\p0 s0 -> return (Just p0, s0, ()))
  local f (Trial a) = Trial $ RWST (\p0 s0 -> do
                                       (a', s', ()) <- runRWST a (f p0) s0
                                       return (a', s', ()))


instance MonadTrans (Trial p s) where
  lift ma =  Trial (RWST (\_ s0 -> ma >>= \a -> return (Just a, s0, ())))

instance MonadWriter () m => MonadWriter () (Trial p s m) where
  tell = undefined
  listen = undefined
  pass = undefined

instance Monad m => MonadState s (Trial p s m) where
  get   = Trial (RWST (\_ s0 -> return (Just s0, s0, ())))
  put s = Trial (RWST (\_ _  -> return (Just (),  s, ())))

instance MonadRWS p () s m => MonadRWS p () s (Trial p s m)
  
  
------------------------------------------------------------------------------
instance Functor (TestResults s) where
  fmap f (TestResults rs) = TestResults (fmap (\(x,y) -> (x, f y)) rs)



{- Not yet :) Let's specailize on gloss for now (or SDL?)
------------------------------------------------------------------------------
class TestBackend b r where
  type RenderResult :: b -> r
  renderStimulus    :: (Stimulus s) => s -> IO r
  stepBackend       :: 
-}
