module AgentEnv where

import Agent
import Environment

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

getRobotAction env robotAgent = [DoNothing robotAgent] -- TODO

getBabyAction env babyAgent = [DoNothing babyAgent] -- TODO

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

applyMoveActionToEnv env action = env -- TODO

applyCleanActionToEnv env action = env -- TODO

applyPickBabyActionToEnv env action = env -- TODO

applyLeaveBabyActionToEnv env action = env -- TODO

applyCreateDirtActionToEnv env action = env -- TODO

-- RANDOM ENV CHANGE --

randomlyChangeEnv env = env -- TODO