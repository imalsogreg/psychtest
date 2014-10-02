{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module PsychTest (
  Test(..)
  ) where

import Data.Monoid
import Data.Text

class Test t where
  type StateType   t
  type ParamsType  t
  type ResultsType t
  runTest  :: Monoid (ResultsType t) => ParamsType t -> t -> IO (ResultsType t)
  genTrial :: Monoid (ResultsType t) => ParamsType t -> t -> IO (ResultsType t)

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
