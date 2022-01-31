module BehaviorUtils where


import Environment
import Agent

-- Brook Architecture --

brookAgent beh env agent = foldl
    (\acc (pred, actionFun) ->
        if null acc then
            if pred env agent then
                actionFun env agent
            else []
        else acc) [] beh

-- UTILS -- 

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




getAllMoveablePositions env validAgents agent =
    let
        Env {height=_height, width=_width} = env
        agentPos = getAgentPos agent
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

-- Returns a new env with all movement changes made. Assumes that the inputs are correct
moveAgent env agent pos =
    let
        agentType = getAgentType agent
    in case agentType of
        Baby -> snd $ moveObstacleRecursive env agent pos
        Robot -> moveRawAgent env agent pos
        _ -> error $ "Agent " ++ show agent ++ " can't move"

moveRawAgent env agent pos =
    let
        newAgent = changeAgentPos agent pos
        envAgentPosUpdated = updateAgentEnv env agent newAgent
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