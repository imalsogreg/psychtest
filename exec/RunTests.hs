module Main where

import PsychTest
import PsychTest.DualNBack

main :: IO ()
main = do
  (genTrial :: Params -> DualNBackTest -> IO Results) undefined undefined
  return ()
