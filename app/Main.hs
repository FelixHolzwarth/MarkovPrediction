module Main where

import Numeric.LinearAlgebra
import Data.Function (on)
import Data.List (minimumBy)

import Lib

type MarcovChain = Vector R -> Vector R

type State = (Int,Int)

type Window = Int

type Steps = Int

type Data t = t State

type States t = t State

classify :: Steps -> [Double] -> [Int]
classify steps ds = map (roundToNearest . (* 100) . (/ maxValue)) ds
  where
    maxValue = maximum ds

    increments = statePercents steps

    roundToNearest :: Double -> Int
    roundToNearest d = case dropWhile (\(_,y) -> fromIntegral y <= d) $ zip increments (tail increments) of
                          ((x,y):_) -> if d - fromIntegral x >= fromIntegral y - d then y else x
                          _         -> last increments

makeMarcovChainFromData :: Foldable t => Window -> Steps -> Data t -> MarcovChain
makeMarcovChainFromData = undefined

stateProduct :: Window -> Steps -> States []
stateProduct winSize steps = [(i,j) | i <- statePercents steps, j <- [1..winSize]]


statePercents :: Steps -> [Int]
statePercents steps = [0,stepSize..200]
  where
    stepSize = round $ 200 / fromIntegral steps

predict :: MarcovChain -> Vector R -> Vector R
predict = ($)

main :: IO ()
main = print "Crypto"
