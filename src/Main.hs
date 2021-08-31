{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Foldable ( Foldable(foldl'), minimumBy )
import Data.Semigroup ( Max(Max, getMax) )
import qualified Data.Sequence as S
import Optics ((^.))
import System.Random (newStdGen)

import Sim

main :: IO ()
main = do
    (_, stateYellow) <- yellowSim
    let meanTimeYellow = meanTime (stateYellow ^. _waitTimes)
        maxTimeYellow = maxTime (stateYellow ^. _waitTimes)
    putStrLn $ "Mean wait time (yellow customers): " ++ show meanTimeYellow
    putStrLn $ "Maximum wait time (yellow customers): " ++ show maxTimeYellow
    (queuesRed, stateRed) <- redSim
    let lengthsRed = map length queuesRed
        meanTimeRed = meanTime (stateRed ^. _waitTimes)
        maxTimeRed = maxTime (stateRed ^. _waitTimes)
    putStrLn $ "Mean queue length (red customers): " ++ show (meanLength lengthsRed)
    putStrLn $ "Maximum queue length (red customers): " ++ show (maxLength lengthsRed)
    (_, stateBlue) <- blueSim
    let meanTimeBlue = meanTime (stateBlue ^. _waitTimes)
        maxTimeBlue = maxTime (stateBlue ^. _waitTimes)
    putStrLn $ "Least absolute difference between maximum and mean wait times: "
            ++ show (minimumBy (\x y -> snd x `compare` snd y)
                            [ ("Yellow", maxTimeYellow - meanTimeYellow)
                            , ("Red", maxTimeRed - meanTimeRed)
                            , ("Blue", maxTimeBlue - meanTimeBlue)
                            ])
    putStrLn $ "Least relative difference between maximum and mean wait times: "
                ++ show (minimumBy (\x y -> snd x `compare` snd y)
                                [ ("Yellow", (maxTimeYellow - meanTimeYellow) / maxTimeYellow)
                                , ("Red", (maxTimeRed - meanTimeRed) / maxTimeRed)
                                , ("Blue", (maxTimeBlue - meanTimeBlue) / maxTimeBlue)
                                ])

yellowSim = runSim serviceYellow

redSim = runSim serviceRed

blueSim = runSim serviceBlue

runSim serviceParameters = do
    g1 <- newStdGen
    let state0 = SimState g1 S.empty 0 []
    return (simulation config state0 (replicate 10000 1.0))
  where
    config = SimConfig (ArrivalParameters 100) serviceParameters

meanTime times = sum times / fromIntegral (length times)

maxTime times = getMax $ foldl' (<>) 0 (map Max times)

meanLength lengths = fromIntegral (sum lengths) / fromIntegral (length lengths)

maxLength lengths = getMax $ foldl' (<>) 0 (map Max lengths)