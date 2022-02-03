module AgentEnv where

import Agent
import Environment
import BehaviorUtils
import BehaviorBaby
import BehaviorRobot
import Data.List (sortOn, delete)
import RandomUtils

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

getDoNothingAction env agent = (env, [DoNothing agent])

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
        agent = getEnvAgentFromAction env action
        agentType = getAgentType agent
    in case agentType of
        Baby -> applyMoveBabyActionToEnv env action
        Robot -> applyMoveRobotActionToEnv env action
        _ -> error $ show agentType ++ " can't move"

applyCleanActionToEnv env action = 
    let
        agent = getEnvAgentFromAction env action
        agentType = getAgentType agent
    in case agentType of
        Robot -> applyCleanRobotActionToEnv env action
        _ -> error $ show agentType ++ " can't clean"

applyPickBabyActionToEnv env action =
    let
        agent = getEnvAgentFromAction env action
        agentType = getAgentType agent
    in case agentType of
        Robot -> applyPickBabyRobotActionToEnv env action
        _ -> error $ show agentType ++ " can't pick a baby"


applyLeaveBabyActionToEnv env action = 
    let
        agent = getEnvAgentFromAction env action
        agentType = getAgentType agent
    in case agentType of
        Robot -> applyLeaveBabyRobotActionToEnv env action
        _ -> error $ show agentType ++ " can't leave a baby"

applyCreateDirtActionToEnv env action = 
    let
        agent = getEnvAgentFromAction env action
        agentType = getAgentType agent
    in case agentType of
        Baby -> applyCreateDirtBabyActionToEnv env action
        _ -> error $ show agentType ++ " can't pick create dirt"

-- RANDOM ENV CHANGE --

randomlyChangeEnv env = 
    let 
        _allPositions = [(x,y) | x <- [0.._width-1], y <- [0.._height-1]]
        (initPlaypenPosition, newEnv) = pickRandomFromListEnv env _allPositions
        (newEnv2, newPlaypen) = buildRandomPlaypen newEnv initPlaypenPosition
        allPositions = filter (\x -> x `notElem` map getAgentPos newPlaypen) _allPositions

        Env {
            height=_height, 
            width=_width, 
            randGen=_randGen, 
            currentTurn=_currentTurn, 
            shuffleTurnAmount=_shuffleTurnAmount, 
            currentIdPointer=_currentIdPointer, 
            agents=_agents
        } = newEnv2

        sortedAgents = sortOn getAgentType $ filter (\x -> getAgentType x /= Playpen) _agents

        (finalGen, _, newAgentsNoPlaypen) = foldl addAgentToRandomPosition (_randGen, allPositions, []) sortedAgents
        finalEnv = Env {
                height=_height, 
                width=_width, 
                randGen=finalGen, 
                currentTurn=_currentTurn, 
                shuffleTurnAmount=_shuffleTurnAmount, 
                currentIdPointer=_currentIdPointer, 
                agents=newAgentsNoPlaypen ++ newPlaypen
            }
    in
        if _currentTurn `mod` _shuffleTurnAmount == 0 then
            finalEnv
        else
            env
    
addAgentToRandomPosition (currentGen, [], currAgentList) _ = (currentGen, [], currAgentList)
addAgentToRandomPosition (currentGen, availablePositions, currAgentList) agentToAdd = 
    let
        (selectedPosition, finalGenerator) = pickRandomFromList availablePositions currentGen
        newAvailablePositions = delete selectedPosition availablePositions
        newAgent = resetAgentStateAndChangePos agentToAdd selectedPosition
        newCurrentAgentList = newAgent:currAgentList
    in
        (finalGenerator, newAvailablePositions, newCurrentAgentList)

buildRandomPlaypen env initialPos = 
    let
        Env {height=_height, width=_width, randGen=_randGen, agents=_agents} = env
        neighbors = validNeighborsPosition _height _width initialPos
        (pos, nextGen) = pickRandomFromList neighbors _randGen
        allPlaypens = getAgentsAgentType Playpen _agents
        (_, newPlaypens, finalRandGen, _) = foldl expandPlaypen (env, [], nextGen, pos) allPlaypens
    in
        (changeEnvRandGen env finalRandGen, newPlaypens)

expandPlaypen (env, agents, randGen, position) agent = 
    let
        Env {height=_height, width=_width} = env
        validPositions = filter (\pos -> pos `notElem` map getAgentPos agents) $ validNeighborsPosition _height _width position
        (selectedPosition, finalRandGen) = pickRandomFromList validPositions randGen
        newAgent = resetAgentStateAndChangePos agent selectedPosition 

    in
        (env, newAgent:agents, finalRandGen, selectedPosition)

