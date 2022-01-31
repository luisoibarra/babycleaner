module Main where

import Control.Exception (assert)
import Environment
import Agent
import Utils
import AgentEnv
import RandomUtils
import Debug.Trace
import InitialStates

-- Simulation Initial State --

-- Simulation's max turn
maxTurn = 10

-- RandomGen Configuration
randomSeed = 10
initialGen = initRandomGen randomSeed

-- Simulation's initial state  -- TODO Create a random initial env generator
-- initState = initState0 initialGen
initState = initStateBabyTest initialGen

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
        trace (show env) simulationLoop envAfterNaturalChange

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
