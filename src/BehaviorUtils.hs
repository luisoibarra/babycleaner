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