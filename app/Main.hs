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
import AgentEnv

-- Initial Configuration --

maxTurn = 15
randomSeed = 10
initialBabiesPlaypens = 2
initialRobots = 2
initialObstacles = 3
initialDirt = 0
defaultHeight = 6
defaultWidth = 6
defaultShuffleTurn = 15

-- Simulation's initial state
-- initialGen = initRandomGen randomSeed
-- initState = initState0 initialGen
-- initState = initStateBabyTest initialGen
-- initState = initStateRobotTest initialGen
initState = getRandomInitialEnv initialBabiesPlaypens initialRobots initialDirt initialObstacles defaultHeight defaultWidth defaultShuffleTurn randomSeed

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
        agents = getEnvAgents env
        afterAgentsInteraction = interactAllAgent env agents
    in
        afterAgentsInteraction

doEnvNaturalChange env =
    let
        turnChangedEnv = addOneToTurnEnv env
        randomChangedEnv = randomlyChangeEnv turnChangedEnv
    in
        randomChangedEnv
