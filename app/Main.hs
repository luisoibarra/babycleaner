module Main where

import Environment
import Agent
import Utils
import AgentEnv
import RandomUtils
import Debug.Trace
import InitialStates
import BehaviorUtils
import Data.List

-- Simulation Initial State --

-- Simulation's max turn
maxTurn = 10

-- RandomGen Configuration
randomSeed = 10
randomInitialBabiesPlaypens = 1
randomInitialRobots = 1
randomInitialObstacles = 0
randomInitialDirt = 0
defaultHeight = 2
defaultWidth = 2
defaultShuffleTurn = 10

-- Simulation's initial state  -- TODO Create a random initial env generator
-- initialGen = initRandomGen randomSeed
-- initState = initState0 initialGen
-- initState = initStateBabyTest initialGen
-- initState = initStateRobotTest initialGen
initState = getRandomInitialEnv randomInitialBabiesPlaypens randomInitialRobots randomInitialDirt randomInitialObstacles defaultHeight defaultWidth defaultShuffleTurn randomSeed

main = do
    -- print $ agentBfs agentCanPassThrough initState Agent { agentType=Robot, posX=3, posY=2, agentId=1, state=EmptyState }
    putStrLn "Begining simulation"
    -- print $ simulationLoop initState
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
    (afterEnv, actions) = getActions(cenv, a):
    cenv = afterEnv
    for a in actions:
        cenv = modEnv(cenv, action)
return cenv
-}
interactAllAgent env [] = env
interactAllAgent env agents =
    let 
        -- First goes the Robots then Babies and so on
        orderedAgents = sortOn (inverseOrderAgentType . getAgentType) agents
    in
        foldl (
        \currentEnv agent -> 
            let 
                (afterEnv, actions) = getAgentActions currentEnv agent 
            in 
                foldl getEnvFromAction afterEnv actions) env orderedAgents 

doEnvNaturalChange env =
    let
        turnChangedEnv = addOneToTurnEnv env
        randomChangedEnv = randomlyChangeEnv turnChangedEnv
    in
        randomChangedEnv
