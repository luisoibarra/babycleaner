module BehaviorRobot where

import Agent
import BehaviorUtils
import Environment
import Debug.Trace

-- Brook Behavior--

robotBrookBehavior = [ 

    -- TESTING LOGIC

    -- (inDirt, \env agent -> (env, [Clean {agent=agent}])),
    -- (\env agent -> not (inPlaypen env agent) && isHoldingSomething agent, testGetMoveRobotActions),
    -- (\env agent -> inPlaypen env agent && isHoldingSomething agent, \env agent -> (env, [LeaveBaby {agent=agent}])),
    -- (inBaby, \env agent -> (env, [PickBaby {agent=agent}])) -- Warning Cycle

    
    -- BASIC LOGIC
    -- Holding a baby
    (\env agent -> isHoldingSomething agent, takeBabyToPlaypen),
    -- Not holding something
    -- Free babies
    (\env agent -> (not . null) (babiesFree env) , searchBaby),
    -- Not holding something, no Free babies
    -- Dirty environment
    (\env agent -> isEnvDirty env, cleanDirt),

    -- Not holding something, no Free babies, no dirty cells
    (\env agent -> True, \env agent -> (env, [DoNothing {agent=agent}]))
    ]

-- Get Actions --

isEnvDirty env = any (\x -> getAgentType x == Dirt) $ getEnvAgents env

isEmptyPlaypen env agent = all (\x -> getAgentType x == Playpen) (getEnvPositionAgent env $ getAgentPos agent)

isDirty env agent = all (\x -> getAgentType x == Dirt) (getEnvPositionAgent env $ getAgentPos agent)

isFreeBaby env agent = not (holededByRobot env agent || inPlaypen env agent) && getAgentType agent == Baby

babiesFree env =
    let
        babies = filter (\a -> getAgentType a == Baby) $ getEnvAgents env
    in
        filter (isFreeBaby env) babies

-- Assumes that agent is holding a baby  
takeBabyToPlaypen env agent
    | inPlaypen env agent = (env, [LeaveBaby {agent=agent}])
    | otherwise =
        let
            path = goToNearest isEmptyPlaypen env agent
            pathLength = length path
        in
            (env,
            if pathLength == 0 then
                [
                    DoNothing {agent=agent}
                ]
            else if pathLength == 1 then
                [
                    Move {agent=agent, destination=path !! 0},
                    LeaveBaby {agent=agent}
                ]
            else if pathLength == 2 then
                [
                    Move {agent=agent, destination=path !! 0},
                    Move {agent=agent, destination=path !! 1},
                    LeaveBaby {agent=agent}
                ]
            else if pathLength >= 3 then
                [
                    Move {agent=agent, destination=path !! 0},
                    Move {agent=agent, destination=path !! 1}
                ]
            else error "Invalid pathLength")

-- Assumes that agent is not holding a baby and that there are free babies
searchBaby env agent 
    | inBaby env agent = (env, [PickBaby {agent=agent}])
    | otherwise =
        let
            path = goToNearest isFreeBaby env agent
            pathLength = length path
        in
            (env,
            if pathLength == 0 then
                [
                    DoNothing {agent=agent}
                ]
            else if pathLength == 1 then
                [
                    Move {agent=agent, destination=path !! 0},
                    PickBaby {agent=agent}
                ]
            else if pathLength >= 2 then
                [
                    Move {agent=agent, destination=path !! 0}
                ]
            else error "Invalid pathLength")

-- Assumes that agent is not holding a baby and that there aren't free babies and that there are dirty cells
cleanDirt env agent 
    | inDirt env agent = (env, [Clean {agent=agent}])
    | otherwise =
        let
            path = goToNearest isDirty env agent
            pathLength = length path
        in
            (env,
            if pathLength == 0 then
                [
                    DoNothing {agent=agent}
                ]
            else if pathLength == 1 then
                [
                    Move {agent=agent, destination=path !! 0},
                    Clean {agent=agent}
                ]
            else if pathLength >= 2 then
                [
                    Move {agent=agent, destination=path !! 0}
                ]
            else error "Invalid pathLength")

testGetMoveRobotActions env agent =
    let
        (posX, posY) = getAgentPos agent
    in
        (env, [Move {agent=agent, destination=(posX -1, posY)}])

-- Apply Action to Env --

applyMoveRobotActionToEnv env action =
    let
        Move {agent=_agent, destination=_destination} = action
        agents = getEnvAgents env
        agentsPicked = getAgentPicked _agent
        holdingAgentList = filter (\agent -> getAgentId agent `elem` agentsPicked) agents
    in
        foldl (\accEnv ag -> moveRawAgent accEnv ag _destination) env $ _agent : holdingAgentList


applyCleanRobotActionToEnv env action =
    let
        agent = getAgentFromAction action
        pos = getAgentPos agent
        agents = getEnvAgents env
        dirt = getSamePositionAndAgentType pos Dirt agents
    in
        foldl deleteAgentFromEnv env dirt

applyPickBabyRobotActionToEnv env action =
    let
        agent = getAgentFromAction action
        pos = getAgentPos agent
        agents = getEnvAgents env
        babies = getSamePositionAndAgentType pos Baby agents
        notPickedBabies = filter (not . isAlreadyPicked env) babies
        pickedBabyAgent = if null notPickedBabies then agent else pickUpAgent agent (head notPickedBabies)
    in
        updateAgentInEnv env agent pickedBabyAgent


applyLeaveBabyRobotActionToEnv env action =
    let
        agent = getAgentFromAction action
        pos = getAgentPos agent
        agents = getEnvAgents env
        babies = getSamePositionAndAgentType pos Baby agents
        pickedBabies = filter (isAlreadyPicked env) babies
        dropedBabyAgent = if null pickedBabies then agent else dropAgent agent (head pickedBabies)
    in
        updateAgentInEnv env agent dropedBabyAgent
