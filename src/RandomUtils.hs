module RandomUtils where

import System.Random
import Data.List (delete)

-- randomDoubleList gen amount = take amount (randoms gen :: [Double])

-- randomIntegerList gen amount = take amount (randoms gen :: [Integer])

initRandomGen = mkStdGen

randomNum :: (Num a, Random a, RandomGen b) => b -> (a, b)
randomNum = random

randomInRange :: (Num a, Random a, RandomGen g) => a -> a -> g -> (a, g)
randomInRange a b = randomR (a, b)

randomDoubleInRange a b = uniformR (a :: Double, b :: Double)

pickRandomFromList list gen =
    let
        (index, newGen) = randomInRange 0 (length list - 1) gen
    in
        (list !! index, newGen)

shuffleList list gen =
    let 
        (shuffledList, _, finalGen) = foldl (\(buildList, oldList, currGen) x ->
            let
                (element, nextGen) = pickRandomFromList oldList currGen
                newOldList = delete element oldList
                newBuildList = element:buildList
            in
                (newBuildList, newOldList, nextGen)) ([], list, gen) list
    in
        (shuffledList, finalGen)

bernoulliExperiment gen prob =
    let
        (num, nextGen) = randomDoubleInRange 0 1 gen
    in
        (num < prob, nextGen)