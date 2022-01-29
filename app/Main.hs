module Main where
import Control.Exception (assert)
import Environment
import Agent
import Utils
import AgentEnv
import RandomUtils

-- Simulation Initial State --

-- Simulation's max turn
maxTurn = 10

randomSeed = 10
initialGen = initRandomGen randomSeed

-- Simulation's initial state
initState = Env {
    height = 5,
    width = 5,
    randGen = initialGen,
    currentTurn = 0,
    shuffleTurnAmount = 10,
    agents = [
        Agent {
            posX = 0,
            posY = 0,
            agentType = Robot,
            state = RobotState {
                holdingBaby = False
            }
        },
        Agent {
            posX = 2,
            posY = 0,
            agentType = Dirt,
            state = EmptyState
        },
        Agent {
            posX = 2,
            posY = 2,
            agentType = Baby,
            state = EmptyState
        },
        Agent {
            posX = 5,
            posY = 2,
            agentType = Baby,
            state = EmptyState
        },
        Agent {
            posX = 2,
            posY = 3,
            agentType = Playpen,
            state = EmptyState
        },
        Agent {
            posX = 3,
            posY = 3,
            agentType = Playpen,
            state = EmptyState
        },
        Agent {
            posX = 4,
            posY = 2,
            agentType = Obstacle,
            state = EmptyState
        }
    ]
}

main = do
    putStrLn "Begining simulation"
    print $ beginSimulation initState
    putStrLn "Simulation end"

beginSimulation initialState =
    if isValidEnv initialState then
        simulationLoop initialState
    else
        error "Invalid initial state"

simulationLoop env = if getEnvTurn env >= maxTurn then env else 
    let
        -- Agent Interaction with Env
        envAfterAgents = doAgentInteraction env
        -- Natural Env Change
        envAfterNaturalChange = doEnvNaturalChange envAfterAgents
    in
        simulationLoop envAfterNaturalChange

doAgentInteraction env = 
    let 
        Env {
                agents=_agents
            } = env
        afterAgentsInteraction = interactAllAgent env _agents 
    in
        afterAgentsInteraction

{-
All agents interact with the environment

cenv = env
for a in agents:
    for action in actions(a):
        cenv = modEnv(cenv, action)
return cenv

-}
interactAllAgent env [] = env
interactAllAgent env agents = 
    foldl (\currentEnv agent -> foldl getEnvFromAction currentEnv (getAgentActions currentEnv agent)) env agents

doEnvNaturalChange env =  
    let
        turnChangedEnv = addOneToTurnEnv env
        randomChangedEnv = randomlyChangeEnv turnChangedEnv
    in 
        randomChangedEnv
