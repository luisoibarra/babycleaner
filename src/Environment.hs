module Environment where
import Agent
import Utils
import System.Random
import Text.Printf
import RandomUtils
import Debug.Trace

data Env s = Env {
    height :: Int,
    width :: Int,
    randGen :: StdGen,
    currentTurn :: Int,
    shuffleTurnAmount :: Int,
    currentIdPointer :: Int,
    agents :: [Agent]
}

instance Show (Env s) where {
  show env = let Env {
      height=_height,
      width=_width,
      randGen=_randGen,
      currentTurn=_currentTurn,
      shuffleTurnAmount=_shuffleTurnAmount,
      agents=_agents
    } = env
    in
        printf "Dimensions (%d, %d)\nTurn %d\nShuffle %d\n\n%s" _height _width _currentTurn _shuffleTurnAmount (showBoard _height _width _agents)
}

getEnvTurn env = let Env { currentTurn = turn } = env in turn
getEnvAgents env = let Env { agents = agents } = env in agents
getEnvGen env = let Env { randGen = gen } = env in gen
getEnvAgentId env id =
    let
        agents = getEnvAgents env
        agentWithId = filter (\x -> id == getAgentId x) agents
    in  if null agentWithId then
            let
                invalidId = trace (concat ["Invalid id:", show id, "Possibles:", show $ map getAgentId agents]) id
            in
                error $ show invalidId ++ "Invalid agent id. Environment doesn't contain any agent with specified id"
        else if length agentWithId >= 2 then
            error "Multiple agents with the same Id"
        else
            head agentWithId

isValidEnv env =
    let
        Env {
            height = height,
            width = width,
            currentTurn = currentTurn,
            shuffleTurnAmount = shuffleTurnAmount,
            agents = agents
        } = env
        playpens = filter (agentTypeIs Playpen) agents
        babies = filter (agentTypeIs Baby) agents
    in
        -- Minimun height and width
        height >= 2 && width >= 2 &&
        -- Playpen amount is equal to the Baby amount
        length playpens == length babies &&
        -- Valid Playpen
        connectedNodes (map getAgentPos playpens) &&
        -- Cells Valid
        all (\x -> validCell (samePositionCells x agents getAgentPos)) agents &&
        -- Valid Positions
        all (validEnvPosition env . getAgentPos) agents

validCell [] = True
validCell agents =
    let
        babies = filter (agentTypeIs Baby) agents
        obstacles = filter (agentTypeIs Obstacle) agents
        playpens = filter (agentTypeIs Playpen) agents
        robots = filter (agentTypeIs Robot) agents
        dirts = filter (agentTypeIs Dirt) agents
    in
        -- Zero or one instance for any AgentType in one position 
        all (\x -> length x <= 1) [babies, obstacles, playpens, robots, dirts] &&
        -- A baby and a playpen can be in the same position
        length (babies ++ playpens) <= 2 &&
        -- A baby and a robot can be in the same position
        length (babies ++ robots) <= 2

-- Returns if pos is a valid position in env
validEnvPosition env pos =
    let
        Env {height=_height, width=_width} = env
    in
        validPosition _height _width pos

-- Returns if pos is a valid position in a height X width matrix  
validPosition height width (posX, posY) = posX >= 0 && posY >= 0 && posX < width && posY < height

-- Returns all valid pos's neighboring positions in a height X width matrix
validNeighborsPosition height width pos = filter (validPosition height width) (positionsNextToMoving pos)

gridNeighborsPositions height width pos = filter (validPosition height width) (positionsNextToGrid pos)


-- Returns a agent list grouped by position  
neighborsAgents env pos =
    let
        neighborsPos = filter (validEnvPosition env) (positionsNextToMoving pos)
    in
        filter (validEnvPosition env . fst) [ (neighPos, getEnvPositionAgent env neighPos) | neighPos <- neighborsPos ]


-- Returns all agents in given position
getEnvPositionAgent env pos = filter (\a -> getAgentPos a == pos) $ getEnvAgents env

changeEnvRandGen env newRandGen =
    let
        Env {
            height=_height,
            width=_width,
            currentTurn=_currentTurn,
            shuffleTurnAmount=_shuffleTurnAmount,
            currentIdPointer=_currentIdPointer,
            agents=_agents
        } = env
    in
        Env {
            height=_height,
            width=_width,
            randGen = newRandGen,
            currentTurn=_currentTurn,
            shuffleTurnAmount=_shuffleTurnAmount,
            currentIdPointer = _currentIdPointer,
            agents=_agents
        }


addOneToTurnEnv env =
    let
        Env {
            height=_height,
            width=_width,
            randGen = _randGen,
            currentTurn=_currentTurn,
            shuffleTurnAmount=_shuffleTurnAmount,
            currentIdPointer=_currentIdPointer,
            agents=_agents
        } = env
    in
        Env {
            height=_height,
            width=_width,
            randGen = _randGen,
            currentTurn=_currentTurn + 1,
            shuffleTurnAmount=_shuffleTurnAmount,
            currentIdPointer = _currentIdPointer,
            agents=_agents
        }

pickRandomFromListEnv env pickList =
    let
        Env {
                height=_height,
                width=_width,
                randGen=_randGen,
                currentTurn=_currentTurn,
                currentIdPointer = _currentIdPointer,
                shuffleTurnAmount=_shuffleTurnAmount,
                agents=_agents
            } = env
        (element, nextGen) = pickRandomFromList pickList _randGen
        nextEnv = Env {
                height=_height,
                width=_width,
                randGen=nextGen,
                currentTurn=_currentTurn,
                currentIdPointer = _currentIdPointer,
                shuffleTurnAmount=_shuffleTurnAmount,
                agents=_agents
            }
    in
        (element, nextEnv)

updateAgentInEnv env oldAgent newAgent =
    let
        deleteAgentEnv = deleteAgentFromEnv env oldAgent
        Env {
                height=_height,
                width=_width,
                randGen=_randGen,
                currentTurn=_currentTurn,
                shuffleTurnAmount=_shuffleTurnAmount,
                currentIdPointer = _currentIdPointer,
                agents=_agents
            } = deleteAgentEnv
        newAgents = newAgent:_agents
    in
        Env {
                height=_height,
                width=_width,
                randGen=_randGen,
                currentTurn=_currentTurn,
                shuffleTurnAmount=_shuffleTurnAmount,
                currentIdPointer = _currentIdPointer,
                agents=newAgents
            }

deleteAgentFromEnv env deleteAgent =
    let
        Env {
                height=_height,
                width=_width,
                randGen=_randGen,
                currentTurn=_currentTurn,
                shuffleTurnAmount=_shuffleTurnAmount,
                currentIdPointer = _currentIdPointer,
                agents=_agents
            } = env
        newAgents = [a | a <- _agents, getAgentId a /= getAgentId deleteAgent]
    in
        Env {
                height=_height,
                width=_width,
                randGen=_randGen,
                currentTurn=_currentTurn,
                shuffleTurnAmount=_shuffleTurnAmount,
                currentIdPointer = _currentIdPointer,
                agents=newAgents
            }

-- Adds newAgent to the env updating the id for consistency
addNewAgentToEnv env newAgent = let
    Env {
            height=_height,
            width=_width,
            randGen=_randGen,
            currentTurn=_currentTurn,
            shuffleTurnAmount=_shuffleTurnAmount,
            currentIdPointer=_currentIdPointer,
            agents=_agents
        } = env
    Agent {
            agentType=_agentType,
            posX=_posX,
            posY=_posY,
            agentId=_agentId,
            state=_state
        } = newAgent
    newPointer = _currentIdPointer+1
    toAddAgent = Agent {agentType=_agentType, posX=_posX, posY=_posY, agentId=newPointer, state=_state}
    in Env {
            height=_height,
            width=_width,
            randGen=_randGen,
            currentTurn=_currentTurn,
            shuffleTurnAmount=_shuffleTurnAmount,
            currentIdPointer=newPointer,
            agents=toAddAgent:_agents
        }

addNewAgentsToEnv :: Foldable t => Env s -> t Agent -> Env s
addNewAgentsToEnv = foldl addNewAgentToEnv


getEnvAgentFromAction env action =
    let
        oldActionAgent = getAgentFromAction action
        agent = getEnvAgentId env $ getAgentId oldActionAgent
    in
        agent

getEnvAgentTypeAgents env agentType =
    let
        allAgents = getEnvAgents env
    in
        filter (\a -> getAgentType a == agentType) allAgents


showBoard height width agents =
    let
        horizontalDelimiter = "  -" ++ concat (replicate height "----")
        verticalDelimiter = "|"
    in
        "    " ++ concat [show i ++ "   " | i <- [0..width-1]] ++ "\n" ++
        horizontalDelimiter ++ "\n" ++
        -- Row
        concat [
            (show y ++ " " ++ verticalDelimiter ++ concat [showAgent x y agents ++ verticalDelimiter | x <- [0..width-1]])
            ++ "\n"
            ++ horizontalDelimiter ++ "\n"
            | y <- [height-1,height-2..0]]