module Environment where
import Agent
import Utils

data Env = Env {
    height :: Int,
    width :: Int,
    currentTurn :: Int,
    shuffleTurnAmount :: Int,
    agents :: [Agent]
}

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
        -- Playpen amount is equal to the Baby amount
        length playpens == length babies &&
        -- Valid Playpen
        connectedCells (map getAgentPos playpens) -- <- MAKING this funtion
