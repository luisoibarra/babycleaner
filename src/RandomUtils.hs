module RandomUtils where

import System.Random

-- randomDoubleList gen amount = take amount (randoms gen :: [Double])

-- randomIntegerList gen amount = take amount (randoms gen :: [Integer])

initRandomGen = mkStdGen

randomNum :: (Num a, Random a, RandomGen b) => b -> (a, b)
randomNum = random

randomInRange :: (Num a, Random a, RandomGen g) => a -> a -> g -> (a, g)
randomInRange a b = randomR (a, b)

pickRandomFromList list gen =
    let
        (index, newGen) = randomInRange 0 (length list - 1) gen
    in
        (list !! index, newGen)