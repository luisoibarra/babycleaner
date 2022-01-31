{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Utils where


dirs = [(0,1),(1,0),(-1,0),(0,-1)]

positionsNextTo (posX, posY) = [ (posX + dX, posY + dY) | (dX, dY) <- dirs ]

nextTo (posX, posY) (posX2, posY2) = any (\(dX, dY) -> posX == posX2 + dX && posY == posY2 + dY) dirs

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

        