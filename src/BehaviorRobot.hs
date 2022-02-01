module BehaviorRobot where

import Agent
import BehaviorUtils
import Environment

-- Brook Behavior--

robotBrookBehavior = [ -- TODO
    (inDirt, \env agent -> [Clean {agent=agent}]),
    (\env agent -> inPlaypen env agent && isHoldingSomething agent, \env agent -> [LeaveBaby {agent=agent}]),
    (inBaby, \env agent -> [PickBaby {agent=agent}])-- Warning Cycle
    ]

-- Apply Action to Env --

applyMoveRobotActionToEnv env action = env -- TODO Remember moving the picked up babies


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
        updateAgentEnv env agent pickedBabyAgent


applyLeaveBabyRobotActionToEnv env action = 
    let
        agent = getAgentFromAction action
        pos = getAgentPos agent
        agents = getEnvAgents env
        babies = getSamePositionAndAgentType pos Baby agents
        pickedBabies = filter (isAlreadyPicked env) babies
        dropedBabyAgent = if null pickedBabies then agent else dropAgent agent (head pickedBabies)
    in
        updateAgentEnv env agent dropedBabyAgent
