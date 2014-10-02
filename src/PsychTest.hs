{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PsychTest (
  Test(..),
  Trial,
  runTest
  ) where

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

newtype Trial p s m r = Trial {
  runTr :: ReaderT p (StateT s  m) r
  } deriving (Monad, Functor, Applicative, MonadIO, MonadReader p, MonadState s)

newtype Trial2 p s m r = Trial2 {
  runTr2 :: RWST p (TestResults s r) s m r
  }

newtype TestResults s r = TestResults { runResults :: [(s,r)] }



runTest :: Trial p s m r -> p -> s -> m (r,s)
runTest k p s = runStateT (runReaderT (runTr k) p) s

class Test t where
  type ParamsType  t
  type StateType   t
  type ResultsType t
--  runTest  :: Monoid (ResultsType t) => ParamsType t -> t -> IO (ResultsType t)
--  runTrial :: (TrialM (StateType t) (ParamsType t) IO) (ResultsType t)

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
