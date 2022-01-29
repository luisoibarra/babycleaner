module Environment where
import Agent
import Utils
import System.Random
import Text.Printf

data Env s = Env {
    height :: Int,
    width :: Int,
    randGen :: StdGen,
    currentTurn :: Int,
    shuffleTurnAmount :: Int,
    agents :: [Agent s]
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
        all (\x -> validCell (samePositionCells x agents getAgentPos)) agents

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

addOneToTurnEnv env = 
    let
        Env {
            height=_height, 
            width=_width, 
            randGen = _randGen,
            currentTurn=_currentTurn, 
            shuffleTurnAmount=_shuffleTurnAmount, 
            agents=_agents
        } = env
    in 
        Env {
            height=_height, 
            width=_width, 
            randGen = _randGen,
            currentTurn=_currentTurn + 1, 
            shuffleTurnAmount=_shuffleTurnAmount, 
            agents=_agents
        }
