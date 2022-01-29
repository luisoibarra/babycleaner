module Utils where


dirs = [(0,1),(1,0),(-1,0),(0,-1)]

nextTo (posX, posY) (posX2, posY2) = any (\(dX, dY) -> posX == posX2 + dX && posY == posY2 + dY) dirs

neighbors (posX, posY) = filter ( `nextTo` (posX, posY))
-- samePosition (posX, posY) cells = filter (== cells) cells

connectedCells [] = True
connectedCells (x:cells) = True -- TODO <- NOT DONE

-- bfs [] = []
-- bfs (node:nodes) = bfs (0, node) nodes [] []

innerBfs currentNodeTuple unseenNodes enqueuedNodes visitedNodes = let
    (currentHeight, currentNode) = currentNodeTuple
    currentNeighbors = neighbors currentNode unseenNodes
    newUnseenNodes = filter (`notElem` currentNeighbors) unseenNodes
    newQueue = enqueuedNodes ++ map (\x  -> (currentHeight + 1, x)) currentNeighbors
    in
        if newQueue == [] then
            visitedNodes
        else
            innerBfs (head newQueue) newUnseenNodes newQueue (currentNodeTuple:visitedNodes)

        