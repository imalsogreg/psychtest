{-# LANGUAGE TypeFamilies #-}

module PsychTest.ReactionTime where

import Control.Concurrent
import Control.Monad.RWS.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class
import Data.Time
import PsychTest

data ReactionTime = ReactionTime
data Params       = Params { subjectName  :: String }
data Result       = Result { reactionTime :: Double }
data State        = State  { trialNum     :: Int    }

--type RTTrial = Trial Params State IO Result

instance Test ReactionTime where
  type ParamsType  ReactionTime = Params
  type StateType   ReactionTime = State
  type ResultsType ReactionTime = Result
  initialState = State 0


trials :: [Trial Params State IO Result]
trials = take 10 $ repeat timeReaction

timeReaction :: Trial Params State IO Result
timeReaction = do
  (Result x) <- return (Result 1) :: Trial Params State IO Result
  y <- ask
  liftIO $ print (x,subjectName y)
  r <- lift . liftIO $ do
    print "Wait for it..."
    threadDelay 1000000
    putStrLn "Go!"
    t0 <- getCurrentTime
    _ <- getChar
    tNow <- getCurrentTime
    return . Result . realToFrac $ diffUTCTime tNow t0
  (State n) <- get
  put (State $ n + 1)
  return r

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
