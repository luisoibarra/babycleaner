module BehaviorUtils where


import Environment
import Agent
import RandomUtils
import Data.List (delete)
import Debug.Trace

-- Brook Architecture --
{-
The first predicate that is true, the corresponding action function is applied, 
returning a tuple of the env and the actions to perform.

The only changes in the environment should be in the randomGenerator. Any other changes should be 
reflected in the action list
-}
brookAgent beh env agent = foldl
    (\(accEnv, actions) (pred, actionFun) ->
        if null actions then
            if pred env agent then
                actionFun env agent
            else (accEnv, [])
        else (accEnv, actions)) (env, []) beh

-- UTILS -- 


inDirt = inAgentType Dirt
inBaby = inAgentType Baby
inPlaypen = inAgentType Playpen


getAgentsInPosition x y = filter (\a -> getAgentPos a == (x, y))

getAgentsAgentType agentType = filter (\a -> getAgentType a == agentType)

getSamePositionAndAgentType (x,y) agentType agents =
    let
        samePosition = getAgentsInPosition x y agents
        sameAgentType = getAgentsAgentType agentType samePosition
    in
        sameAgentType

-- If agent is in the same position of an agentType agent
inAgentType agentType env agent =
    let
        position = getAgentPos agent
        agents = getEnvAgents env
    in
        not (null (getSamePositionAndAgentType position agentType agents))

holededByRobot env agent =
    let
        agentId = getAgentId agent
        pos = getAgentPos agent
        agents = getEnvAgents env
        samePositionRobots = getSamePositionAndAgentType pos Robot agents
        robotStates = map getAgentState samePositionRobots
    in
        any (\state ->
                case state of
                    RobotState {holdingAgents=_holdingAgents} -> agentId `elem` _holdingAgents
                    _ -> False) robotStates


-- Move Utils --

-- Returns if a given agent can move to pos. (WITHOUT PUSHING)
canMoveNoPushing env agent pos = validEnvPosition env pos &&
    let
        (posX, posY) = pos
        agentType = getAgentType agent
        agents = getEnvAgents env
        agentTypes = map getAgentType $ getAgentsInPosition posX posY agents
    in case agentType of
         -- Obstacle can't be with other agent 
         Obstacle -> null agentTypes
         -- Playpen can't move or be moved
         Playpen -> False
         -- Dirt can't move or be moved
         Dirt -> False
         -- Baby can be with other babies
         Baby -> all (== Baby) agentTypes
         -- Robot can be with other babies, on dirt and in a playpen
         Robot -> all (\a -> a `elem` [Dirt, Baby, Playpen]) agentTypes

getAllMoveablePositions env validAgents agentPos =
    let
        Env {height=_height, width=_width} = env
        -- Mapping agentList to (cellPosition, agentList)
        currentNeighborsAgents = neighborsAgents env agentPos
        -- All empty or occupied by only validAgents positions
        possibleMovePositions = [pos | (pos, agents) <- currentNeighborsAgents,
                -- Empty
                null agents ||
                -- Occuppied by only validAgents 
                all (\a -> let agentType = getAgentType a in agentType `elem` validAgents) agents
            ]
    in
        possibleMovePositions

moveRawAgent env agent pos =
    let
        newAgent = changeAgentPos agent pos
        envAgentPosUpdated = updateAgentInEnv env agent newAgent
    in
        envAgentPosUpdated

moveObstacleRecursive env agent pos =
    let
        (posX, posY) = pos
        (posX2, posY2) = getAgentPos agent
        (dX, dY) = (posX - posX2, posY - posY2)
        envAgents = getEnvAgents env
        obstaclesInPosition = filter (agentTypeIs Obstacle) $ getAgentsInPosition posX posY envAgents
    in
        if canMoveNoPushing env agent pos then
            (True, moveRawAgent env agent pos)
        else if null obstaclesInPosition then
            (False, env)
        else let
                obstacle = head obstaclesInPosition
                newPosition = (posX + dX, posY + dY)
                (moved, newEnv) = moveObstacleRecursive env obstacle newPosition
            in
                if moved then
                    (True, moveRawAgent newEnv agent pos)
                else
                    (False, newEnv)

{-
Returns a list of tuple containing in the first space the found agent and in the second space
the current optimal path to it. -> [(Agent, [(Int, Int)])]
-}
agentBfs allowedAgentsInPath env agent = -- TODO
    let
        pos = getAgentPos agent
        agents = getEnvAgents env
    in
        innerAgentBfs allowedAgentsInPath env pos [pos] [] [] [(agent, [])]

innerAgentBfs allowedAgentsInPath env currentPos visitedPositions pendingPositions currentPath currentAgentsDistanceInfo =
    let
        (posX, posY) = currentPos
        Env {height=_height, width=_width, agents=_agents} = env
        
        allValidToVisitPositions = getAllMoveablePositions env allowedAgentsInPath currentPos
        notSeenPositions = map (\pos -> (pos, currentPath ++ [pos])) $ filter (`notElem` visitedPositions) allValidToVisitPositions
        allPendingPositions = pendingPositions ++ notSeenPositions

        agentsInCurrentPosition = getAgentsInPosition posX posY _agents
        newCurrentAgentsDistanceInfo = currentAgentsDistanceInfo ++ map (\a -> (a, currentPath)) agentsInCurrentPosition

        (newCurrentPos, newCurrentPath) = head allPendingPositions
        newPendingPositions = tail allPendingPositions
    
        newVisitedPositions = currentPos:visitedPositions
    in
        if null allPendingPositions then
            newCurrentAgentsDistanceInfo
        else
            innerAgentBfs allowedAgentsInPath env newCurrentPos newVisitedPositions newPendingPositions newCurrentPath newCurrentAgentsDistanceInfo


-- Add Dirt Utils --

-- Returns a list mapping positions to if it's dirty or not, and also the final generator
addDirtToPositions randGen positions maxAmountOfDirt prob =
    let
        (shuffledPositions, nextGen) = shuffleList positions randGen
        dirts = foldl (\(listDirtPos, currGen) pos ->
            let
                (isDirtBernoulli, nextCurrGen) = bernoulliExperiment currGen prob
                currentDirts = length $ [ isDirt | (isDirt, _) <- listDirtPos, isDirt]
                newList = (isDirtBernoulli && currentDirts < maxAmountOfDirt, pos):listDirtPos
            in
                (newList, nextCurrGen))

                ([], nextGen) shuffledPositions
    in
        ([pos | (dirty, pos) <- fst dirts, dirty], snd dirts)


-- Pick up Utils --

getAgentPicked agent =
    let
        state = getAgentState agent
    in case state of
        RobotState {holdingAgents=_holdingAgents} -> _holdingAgents
        _ -> []

isAlreadyPicked env agent =
    let
        allPicked = [x | ag <- getEnvAgents env, x <- getAgentPicked ag]
        agentId = getAgentId agent
    in
        agentId `elem` allPicked

pickUpAgent agent agentToPick =
    let
        RobotState {holdingAgents=_holdingAgents} = getAgentState agent
        toPickId = getAgentId agentToPick
        newHoldingAgents = if toPickId `elem` _holdingAgents then _holdingAgents else toPickId:_holdingAgents
        newState = RobotState {holdingAgents=newHoldingAgents}
        newAgent = changeAgentState agent newState
    in
        newAgent

dropAgent agent agentToDrop =
    let
        RobotState {holdingAgents=_holdingAgents} = getAgentState agent
        toDropId = getAgentId agentToDrop
        newHoldingAgents = if toDropId `notElem` _holdingAgents then _holdingAgents else delete toDropId _holdingAgents
        newState = RobotState {holdingAgents=newHoldingAgents}
        newAgent = changeAgentState agent newState
    in
        newAgent
        