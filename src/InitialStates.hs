module InitialStates where

import RandomUtils
import Environment
import System.Random
import Agent
import AgentEnv (randomlyChangeEnv)



emptyDefaultEnv initialGen = Env {
    height = 5,
    width = 5,
    randGen = initialGen,
    currentTurn = 0,
    shuffleTurnAmount = 10,
    currentIdPointer = 1,
    agents = []
}

defaultAgents = [
        Agent {
            agentId = 0,
            posX = 0,
            posY = 0,
            agentType = Robot,
            state = RobotState {
                holdingAgents = []
            }
        },
        Agent {
            agentId = 1,
            posX = 2,
            posY = 0,
            agentType = Dirt,
            state = EmptyState
        },
        Agent {
            agentId = 2,
            posX = 2,
            posY = 2,
            agentType = Baby,
            state = EmptyState
        },
        Agent {
            agentId = 3,
            posX = 5,
            posY = 2,
            agentType = Baby,
            state = EmptyState
        },
        Agent {
            agentId = 4,
            posX = 2,
            posY = 3,
            agentType = Playpen,
            state = EmptyState
        },
        Agent {
            agentId = 5,
            posX = 3,
            posY = 3,
            agentType = Playpen,
            state = EmptyState
        },
        Agent {
            agentId = 6,
            posX = 4,
            posY = 2,
            agentType = Obstacle,
            state = EmptyState
        }
    ]

buildEnv = foldl addNewAgentToEnv

initState0 initialGen = buildEnv (emptyDefaultEnv initialGen) defaultAgents

babyTestAgents = [
        Agent {
            agentId = 1,
            posX = 0,
            posY = 3,
            agentType = Dirt,
            state = EmptyState
        },
        Agent {
            agentId = 99,
            posX = 1,
            posY = 3,
            agentType = Dirt,
            state = EmptyState
        },
        Agent {
            agentId = 100,
            posX = 2,
            posY = 3,
            agentType = Dirt,
            state = EmptyState
        },
        Agent {
            agentId = 2,
            posX = 0,
            posY = 4,
            agentType = Baby,
            state = EmptyState
        },
        Agent {
            agentId = 3,
            posX = 0,
            posY = 0,
            agentType = Baby,
            state = EmptyState
        },
        Agent {
            agentId = 4,
            posX = 0,
            posY = 1,
            agentType = Playpen,
            state = EmptyState
        },
        Agent {
            agentId = 5,
            posX = 1,
            posY = 0,
            agentType = Playpen,
            state = EmptyState
        },
        Agent {
            agentId = 6,
            posX = 1,
            posY = 4,
            agentType = Obstacle,
            state = EmptyState
        },
        Agent {
            agentId = 7,
            posX = 2,
            posY = 4,
            agentType = Obstacle,
            state = EmptyState
        }
    ]

initStateBabyTest initialGen = buildEnv (emptyDefaultEnv initialGen) babyTestAgents

robotTestAgents = [
        Agent {
            agentId = 0,
            posX = 0,
            posY = 0,
            agentType = Robot,
            state = RobotState {
                holdingAgents = []
            }
        },
        Agent {
            agentId = 0,
            posX = 0,
            posY = 4,
            agentType = Dirt,
            state = EmptyState
        },
        Agent {
            agentId = 0,
            posX = 2,
            posY = 0,
            agentType = Robot,
            state = RobotState {
                holdingAgents = []
            }
        },
        Agent {
            agentId = 0,
            posX = 3,
            posY = 2,
            agentType = Robot,
            state = RobotState {
                holdingAgents = []
            }
        },
        Agent {
            agentId = 0,
            posX = 3,
            posY = 2,
            agentType = Baby,
            state = EmptyState
        },

        Agent {
            agentId = 0,
            posX = 3,
            posY = 1,
            agentType = Dirt,
            state = EmptyState
        },
        Agent {
            agentId = 0,
            posX = 3,
            posY = 3,
            agentType = Dirt,
            state = EmptyState
        },
        Agent {
            agentId = 0,
            posX = 4,
            posY = 2,
            agentType = Dirt,
            state = EmptyState
        },
        Agent {
            agentId = 0,
            posX = 2,
            posY = 2,
            agentType = Dirt,
            state = EmptyState
        },
        Agent {
            agentId = 1,
            posX = 2,
            posY = 0,
            agentType = Dirt,
            state = EmptyState
        },
        Agent {
            agentId = 2,
            posX = 0,
            posY = 0,
            agentType = Baby,
            state = EmptyState
        },
        Agent {
            agentId = 3,
            posX = 0,
            posY = 0,
            agentType = Playpen,
            state = EmptyState
        },
        Agent {
            agentId = 3,
            posX = 0,
            posY = 1,
            agentType = Playpen,
            state = EmptyState
        },
        Agent {
            agentId = 3,
            posX = 1,
            posY = 1,
            agentType = Playpen,
            state = EmptyState
        }
    ]

initStateRobotTest initialGen = buildEnv (emptyDefaultEnv initialGen) robotTestAgents


getRandomInitialEnv babiesAmount robotsAmount dirtAmount obstacleAmount height width shuffleTurn seed =
    let
        randGen = mkStdGen seed
        babies = [getDefaultAgentFromAgentType Baby | _ <- [1..babiesAmount]]
        playpens = [getDefaultAgentFromAgentType Playpen | _ <- [1..babiesAmount]]
        robots = [getDefaultAgentFromAgentType Robot | _ <- [1..robotsAmount]]
        dirt = [getDefaultAgentFromAgentType Dirt | _ <- [1..dirtAmount]]
        obstacles = [getDefaultAgentFromAgentType Obstacle | _ <- [1..obstacleAmount]]
        initEnv = Env {height=height, width=width, randGen=randGen, currentTurn=0, shuffleTurnAmount=shuffleTurn, currentIdPointer=1, agents=[]}
        finalEnv = foldl addNewAgentToEnv initEnv (babies ++ playpens ++ robots ++ dirt ++ obstacles)
    in
        randomlyChangeEnv finalEnv
    