{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Utils where
import Debug.Trace (trace)


moveDirs = [(0,1),(1,0),(-1,0),(0,-1)]
gridDirs = [
        (0,1),(1,0),(-1,0),(0,-1),
        (1,1),(-1,1),(-1,-1),(1,-1),
        (0,0)
    ]

positionsNextTo :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
positionsNextTo dirs (posX, posY) = [ (posX + dX, posY + dY) | (dX, dY) <- dirs ]

positionsNextToMoving = positionsNextTo moveDirs

positionsNextToGrid = positionsNextTo gridDirs

nextTo (posX, posY) (posX2, posY2) = any (\(dX, dY) -> posX == posX2 + dX && posY == posY2 + dY) moveDirs

neighbors (posX, posY) = filter ( `nextTo` (posX, posY))

samePositionCells cell cells getPosition = filter (\x -> getPosition cell == getPosition x) cells

connectedNodes :: [(Int, Int)] -> Bool
connectedNodes nodes = length nodes == length (bfs nodes)


{-
Returns a list mapping node distance to node for reachable nodes
>>> bfs [(0,1),(0,0),(1,1)]
[(1,(1,1)),(1,(0,0)),(0,(0,1))]
-}
bfs [] = []
bfs (node:nodes) = innerBfs (0, node) nodes [] []

innerBfs currentNodeTuple unseenNodes enqueuedNodes visitedNodes = let
    (currentHeight, currentNode) = currentNodeTuple
    currentNeighbors = neighbors currentNode unseenNodes
    newUnseenNodes = filter (`notElem` currentNeighbors) unseenNodes
    newQueue = enqueuedNodes ++ map (\x  -> (currentHeight + 1, x)) currentNeighbors
    newVisitedNodes = currentNodeTuple:visitedNodes
    in
        if newQueue == [] then
            newVisitedNodes
        else
            innerBfs (head newQueue) newUnseenNodes (tail newQueue) newVisitedNodes

