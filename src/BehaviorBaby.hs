module BehaviorBaby where
import Environment

import BehaviorUtils
import Agent

-- Brook Behavior--

babyBrookBehavior = [
        -- DoNothing When? -> In Playpen | When been holded by a Robot
        (inPlaypen, \env agent -> (env, [DoNothing {agent = agent}])),
        (holededByRobot, \env agent -> (env, [DoNothing {agent = agent}])),
        (\env agent -> True, getMoveBabyActions)
    ]

-- Get Actions --

getMoveBabyActions env agent =
    let
        position = getAgentPos agent
        -- Can move only if it is empty or there are only obstacles
        possibleMovePositions = getAllMoveablePositions env (isAllAgentType [Obstacle]) agent position
        (movePos, nextRandEnv) = pickRandomFromListEnv env possibleMovePositions
    in  if null possibleMovePositions then
            (env, [DoNothing {agent=agent}, CreateDirt {agent = agent}])
        else
            (nextRandEnv, [Move {agent = agent, destination = movePos}, CreateDirt {agent = agent}])

-- Apply Action to Env --

applyMoveBabyActionToEnv env action =
    let
        Move {agent=agent, destination=movePos} = action
    in  
        snd $ moveObstacleRecursive env agent movePos

-- Appear dirt probability
dirtCreationProb = 0.25

getMaxDirtAmount :: Int -> Int
getMaxDirtAmount babiesAmount
    | babiesAmount <= 0 = 0
    | babiesAmount == 1 = 1
    | babiesAmount == 2 = 3
    | babiesAmount >= 3 = 6
    | otherwise = error "Invalid babiesAmount range"


applyCreateDirtBabyActionToEnv env action = 
    let
        agent = getEnvAgentFromAction env action
        pos = getAgentPos agent
        Env {
            height=_height, 
            width=_width, 
            randGen=_randGen, 
            currentTurn=_currentTurn, 
            shuffleTurnAmount=_shuffleTurnAmount, 
            currentIdPointer=_currentIdPointer, 
            agents=_agents
        } = env
        gridValidPositions = gridNeighborsPositions _height _width pos
        babyAmount = length $ foldl (\acc currPos -> acc ++ getSamePositionAndAgentType currPos Baby _agents ) [] gridValidPositions
        maxDirtAmount = getMaxDirtAmount babyAmount
        -- Get all empty positions around pos
        emptyPositions = [p | p <- gridValidPositions, null $ getEnvPositionAgent env p]
        (dirtPositions, dirtRandGen) = addDirtToPositions _randGen emptyPositions maxDirtAmount dirtCreationProb
        initialEnv = Env {
                height=_height, 
                width=_width, 
                randGen=dirtRandGen, 
                currentTurn=_currentTurn, 
                shuffleTurnAmount=_shuffleTurnAmount, 
                currentIdPointer=_currentIdPointer, 
                agents=_agents
            }
    in
        addNewAgentsToEnv initialEnv 
            [
                Agent { 
                    agentType=Dirt, 
                    posX=posX, 
                    posY=posY, 
                    agentId=0, 
                    state=EmptyState } 
                    | (posX, posY) <- dirtPositions
            ]
