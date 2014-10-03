{-# LANGUAGE TypeFamilies #-}

module PsychTest.ReactionTime where

import Control.Concurrent
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Time
import PsychTest

data ReactionTime = ReactionTime
data Params       = Params { subjectName  :: String }
data Result       = Result { reactionTime :: Double }
data State        = State  { trialInd     :: Int    }

{-
reactionTest = do
  welcome
  replicate 10 reactionTrial


runTrial :: Trial Params State IO Result
runTrial = do
  (Params n) <- ask
  dt <- liftIO $ do
    putStrLn $ "Wait for it, " ++ n ++ "..."
    threadDelay 500000
    putStrLn $ "Go!"
    t0 <- getCurrentTime
    getChar
    tNow <-getCurrentTime
    return . realToFrac $ diffUTCTime tNow t0
  return $ Result dt
  
-}  
