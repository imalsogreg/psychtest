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
import Control.Monad.Trans.Writer
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

{- -- Try2. So close! Except that I couldn't implement mzero for Trial,
   -- because where do I get the initial state? And I have no a to give
   -- to 'return'
newtype Trial p s m a = Trial { runTrial :: Monad m => RWST p (TestResults s a) s m a }

instance Monad m => Functor (Trial p s m) where
  fmap f ma = Trial (RWST (\p0 s0 -> do
                              (a, s, w) <- runRWST (runTrial ma) p0 s0
                              return (f a, s, TestResults [(s,f a)])))

instance Monad m => Applicative (Trial p s m) where
  pure a    = Trial ( RWST (\_ s -> return (a, s, TestResults [(s,a)])))
--  (Trial f')  <*> (Trial a') = let toF = runRWST f'
--                                   toA = runRWST a'

instance (Monad m) => Monad (Trial p s m) where
  return           = pure
  a'@(Trial a) >>= f = Trial $ RWST (\p0 s0 -> do
                                      (a'',s, _) <- runRWST a p0 s0
                                      let (Trial b) = f a''
                                      runRWST b p0 s
                                    )

instance (Monad m) => MonadPlus (Trial p s m) where
  mzero                     = return (Nothing, s0, [])
  mplus (Trial a) (Trial b) = Trial $ RWST (\p0 s0 -> do
                                               (a'',s ,w ) <- runRWST a p0 s0
                                               (b'',s',w') <- runRWST b p0 s
                                               return (b'', s', w <> TestResults [(s',b'')]))



newtype TestResults s a = TestResults { runResults :: [(s,a)] }
                        deriving (Monoid)


instance Functor (TestResults s) where
  fmap f (TestResults rs) = TestResults (fmap (\(x,y) -> (x, f y)) rs)
-}




class Test t where
  type ParamsType  t
  type StateType   t
  type ResultsType t



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
