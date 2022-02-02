module BehaviorRobot where

import Agent
import BehaviorUtils
import Environment

-- Brook Behavior--

robotBrookBehavior = [ -- TODO
    (inDirt, \env agent -> (env, [Clean {agent=agent}])),
    (\env agent -> not (inPlaypen env agent) && isHoldingSomething agent, getMoveRobotActions),
    (\env agent -> inPlaypen env agent && isHoldingSomething agent, \env agent -> (env, [LeaveBaby {agent=agent}])),
    (inBaby, \env agent -> (env, [PickBaby {agent=agent}])) -- Warning Cycle
    ]

-- Get Actions --

getMoveRobotActions env agent = -- TODO
    let
        (posX, posY) = getAgentPos agent
    in
        (env, [Move {agent=agent, destination=(posX -1, posY)}])

-- Apply Action to Env --

applyMoveRobotActionToEnv env action =
    let
        Move {agent=_agent, destination=_destination} = action
        pos = getAgentPos _agent
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
