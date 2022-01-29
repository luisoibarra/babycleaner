module Main where
import Control.Exception (assert)
import Environment
import Agent
import Utils

main = do
    putStrLn "Here"
    print (nextTo (0,1) (0,2))

-- Simulation Initial State
initState = Env {
    height = 10,
    width = 10,
    currentTurn = 0,
    shuffleTurnAmount = 10,
    agents = [
        Agent {
            posX = 0,
            posY = 0,
            agentType = Robot
        },
        Agent {
            posX = 2,
            posY = 0,
            agentType = Dirt
        },
        Agent {
            posX = 2,
            posY = 2,
            agentType = Baby
        },
        Agent {
            posX = 5,
            posY = 2,
            agentType = Baby
        },
        Agent {
            posX = 2,
            posY = 3,
            agentType = Playpen
        },
        Agent {
            posX = 3,
            posY = 3,
            agentType = Playpen
        },
        Agent {
            posX = 4,
            posY = 2,
            agentType = Obstacle
        }
    ]
}

beginSimulation initialState =
    if isValidEnv initialState then
        simulationLoop initialState
    else
        error "Invalid initial state"

simulationLoop env = let
    -- Agent Interaction with Env
    envAfterAgents = doAgentInteraction env
    -- Natural Env Change
    envAfterNaturalChange = doEnvNaturalChange envAfterAgents
    in
        simulationLoop envAfterNaturalChange

doAgentInteraction env = env -- TODO

doEnvNaturalChange env = env -- TODO
