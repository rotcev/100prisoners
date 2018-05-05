module Main where

import           System.Random.Shuffle

type Found = Bool

type Prisoner = Int

type RemainingAttempts = Int

type Cabinet = [Int]

followNumbers ::
     Cabinet
  -> Prisoner
  -> Prisoner
  -> RemainingAttempts
  -> (RemainingAttempts, Found)
followNumbers _ _ _ 0 = (0, False)
followNumbers cabinet prisoner target remainingAttempts =
  if prisoner == target
    then (remainingAttempts, True)
    else followNumbers
           cabinet
           prisoner
           (cabinet !! target)
           (remainingAttempts - 1)

findNumber :: Cabinet -> Prisoner -> (RemainingAttempts, Found)
findNumber cabinet prisoner =
  let first = cabinet !! prisoner
  in if prisoner == first
       then (49, True)
       else followNumbers cabinet prisoner (cabinet !! first) 49

solve :: Cabinet -> (Found, [(RemainingAttempts, Found)])
solve [] = undefined
solve cabinet =
  let prisoners = [0 .. 99]
      results = fmap (findNumber cabinet) prisoners
      allSurvive = all (\(_, r) -> r) results
  in (allSurvive, results)

main :: IO ()
main = do
  cabinet <- shuffleM [0 .. 99]
  print $ "Solving for " ++ show cabinet
  print . solve $ cabinet
