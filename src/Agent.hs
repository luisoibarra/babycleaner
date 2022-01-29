module Agent where


data AgentType = Baby | Obstacle | Robot | Playpen | Dirt deriving Eq

data Agent = Agent {
    posX :: Int,
    posY :: Int,
    agentType :: AgentType
}

agentTypeIs agentType agent = let Agent {agentType = agentAgentType } = agent in agentAgentType == agentType

getAgentPos agent = let Agent {posX = posX, posY = posY} = agent in (posX, posY)
