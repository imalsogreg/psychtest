{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PsychTest.DualNBack where

------------------------------------------------------------------------------
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Monoid
import           Data.Time
import qualified Data.Vector as V
------------------------------------------------------------------------------
import PsychTest

data TrialState = WaitingHello
                | InterTrial
                | Presenting      [ (TilePos, Sound) ]
                | WaitingResponse [ (TilePos, Sound) ]
                | ReportingResult

data TilePos = TilePos (Int,Int)

data Sound = SoundA
           | SoundB
           | SoundC
           | SoundD
           | SoundE
           | SoundF
           | SoundG
           | SoundH
           | SoundI

data Params = Params String

data DualNBackTest = DualNBackTest (ReaderT Params (StateT TrialState IO) Results)
  

instance Test DualNBackTest where
  type StateType   DualNBackTest = TrialState
  type ParamsType  DualNBackTest = Params
  type ResultsType DualNBackTest = Results

newtype Results = Results (Sum Int)
                deriving (Monoid)

mkTrial = undefined

{-
trials :: Trial Results
trials = do
  welcomeScreen `awaiting` Click
-}

data Trial r = Trial { runTrial :: IO r }


awaiting :: Stimulus -> PassCondition -> IO Results
awaiting s Click = undefined

welcomeScreen :: PassCondition -> Stimulus
welcomeScreen c = Stimulus

data Stimulus = Stimulus

data PassCondition = TimeElapsed
                   | Click

data TrialResults = TrialResults { 
    responseTime :: Double
  , trialCorrect :: Bool
  }
                    

