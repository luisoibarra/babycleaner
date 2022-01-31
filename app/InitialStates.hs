module InitialStates where

import RandomUtils
import Environment
import Agent

initState0 initialGen = Env {
    height = 5,
    width = 5,
    randGen = initialGen,
    currentTurn = 0,
    shuffleTurnAmount = 10,
    agents = [
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
}

initStateBabyTest initialGen = Env {
    height = 5,
    width = 5,
    randGen = initialGen,
    currentTurn = 0,
    shuffleTurnAmount = 10,
    agents = [
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
}

