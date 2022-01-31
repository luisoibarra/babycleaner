module Agent where


data AgentType = Obstacle | Playpen | Dirt | Baby | Robot deriving (Eq, Ord, Show)

type Id = Int

data Agent s = Agent {
    agentType :: AgentType,
    posX :: Int,
    posY :: Int,
    agentId :: Id,
    state :: s
} deriving (Eq, Ord)

instance Show (Agent s) where {
  show a = case getAgentType a of
      Obstacle -> "O"
      Playpen -> "P"
      Dirt -> "D"
      Baby -> "B"
      Robot -> "R"
}

data Action s =
    DoNothing {
        agent :: Agent s
    } |
    Move {
        agent :: Agent s
        -- moveDestPosX :: Integer,
        -- moveDestPosY :: Integer
    } |
    Clean {
        agent :: Agent s
    } |
    LeaveBaby {
        agent :: Agent s
    } |
    PickBaby {
        agent :: Agent s
    } |
    CreateDirt {
        agent :: Agent s
    }

data AgentState =
    EmptyState |
    RobotState {
        -- Stores the ids of the agents that the robot is holding, 
        -- the agents should be in the same position of the robot
        holdingAgents :: [Id]
    }

agentTypeIs agentType agent = let agentAgentType = getAgentType agent in agentAgentType == agentType

getAgentPos agent = let Agent {posX = posX, posY = posY} = agent in (posX, posY)

getAgentType agent = let Agent {agentType = agentAgentType } = agent in agentAgentType

getAgentState agent = let Agent {state = state } = agent in state
getAgentId agent = let Agent { agentId = id } = agent in id

getAgentFromAction DoNothing {agent = _agent} = _agent
getAgentFromAction Move {agent = _agent} = _agent
getAgentFromAction Clean {agent = _agent} = _agent
getAgentFromAction PickBaby {agent = _agent} = _agent
getAgentFromAction LeaveBaby {agent = _agent} = _agent
getAgentFromAction CreateDirt {agent = _agent} = _agent

showAgent x y agents =
    let
        positionAgents = filter (\agent -> getAgentPos agent == (x,y)) agents
        rawString = concatMap show positionAgents 
    in case length rawString of
        0 -> "   "
        1 -> " " ++ rawString ++ " "
        2 -> " " ++ rawString
        _ -> rawString
    

showBoard height width agents =
    let
        horizontalDelimiter = "-" ++ concat (replicate height "----")
        verticalDelimiter = "|"
    in
        horizontalDelimiter ++ "\n" ++
        -- Row
        concat [
            (verticalDelimiter ++ concat [showAgent x y agents ++ verticalDelimiter | x <- [0..width-1]])
            ++ "\n"
            ++ horizontalDelimiter ++ "\n"
            | y <- [height-1,height-2..0]]