module AgentEnv where

import Agent
import Environment
import BehaviorUtils
import BehaviorBaby
import BehaviorRobot

-- GET AGENT ACTIONS --

getAgentActions env agent =
    let
        currentType = getAgentType agent
    in case currentType of
        Obstacle -> getDoNothingAction env agent
        Playpen  -> getDoNothingAction env agent
        Dirt -> getDoNothingAction env agent
        Baby -> getBabyAction env agent
        Robot -> getRobotAction env agent

-- AGENT TYPE GET ACTIONS --

getDoNothingAction env agent = [DoNothing agent]

getRobotAction = brookAgent robotBrookBehavior 

getBabyAction = brookAgent babyBrookBehavior

-- APPLY ACTION TO ENV --

getEnvFromAction env action =
    case action of
        DoNothing {} -> applyDoNothingActionToEnv env action
        Move {} -> applyMoveActionToEnv env action
        Clean {} -> applyCleanActionToEnv env action
        PickBaby {} -> applyPickBabyActionToEnv env action
        LeaveBaby {} -> applyLeaveBabyActionToEnv env action
        CreateDirt {} -> applyCreateDirtActionToEnv env action

applyDoNothingActionToEnv env action = env

applyMoveActionToEnv env action =
    let
        agent = getAgentFromAction action
        agentType = getAgentType agent
    in case agentType of
        Baby -> applyMoveBabyActionToEnv env action
        Robot -> applyMoveRobotActionToEnv env action
        _ -> error $ show agentType ++ " can't move"

applyCleanActionToEnv env action = 
    let
        agent = getAgentFromAction action
        agentType = getAgentType agent
    in case agentType of
        Robot -> applyCleanRobotActionToEnv env action
        _ -> error $ show agentType ++ " can't clean"

applyPickBabyActionToEnv env action =
    let
        agent = getAgentFromAction action
        agentType = getAgentType agent
    in case agentType of
        Robot -> applyPickBabyRobotActionToEnv env action
        _ -> error $ show agentType ++ " can't pick a baby"


applyLeaveBabyActionToEnv env action = 
    let
        agent = getAgentFromAction action
        agentType = getAgentType agent
    in case agentType of
        Robot -> applyLeaveBabyRobotActionToEnv env action
        _ -> error $ show agentType ++ " can't leave a baby"

applyCreateDirtActionToEnv env action = 
    let
        agent = getAgentFromAction action
        agentType = getAgentType agent
    in case agentType of
        Baby -> applyCreateDirtBabyActionToEnv env action
        _ -> error $ show agentType ++ " can't pick create dirt"

-- RANDOM ENV CHANGE --

randomlyChangeEnv env = env -- TODO