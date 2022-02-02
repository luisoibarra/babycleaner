module Agent where


data AgentType = Obstacle | Playpen | Dirt | Baby | Robot deriving (Eq, Ord, Show)

type Id = Int

data Agent = Agent {
    agentType :: AgentType,
    posX :: Int,
    posY :: Int,
    agentId :: Id,
    state :: AgentState
} deriving (Eq, Ord)

instance Show Agent where {
  show a = case getAgentType a of
      Obstacle -> "O"
      Playpen -> "P"
      Dirt -> "D"
      Baby -> "B"
      Robot -> "R" ++ if isHoldingSomething a then "*" else ""
}

data Action s =
    DoNothing {
        agent :: Agent
    } |
    Move {
        agent :: Agent,
        destination :: (Int, Int)
        -- moveDestPosX :: Integer,
        -- moveDestPosY :: Integer
    } |
    Clean {
        agent :: Agent
    } |
    LeaveBaby {
        agent :: Agent
    } |
    PickBaby {
        agent :: Agent
    } |
    CreateDirt {
        agent :: Agent
    }

data AgentState =
    EmptyState |
    RobotState {
        -- Stores the ids of the agents that the robot is holding, 
        -- the agents should be in the same position of the robot
        holdingAgents :: [Id]
    } deriving (Eq, Ord)

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

isHoldingSomething agent =
    case getAgentState agent of
        RobotState {holdingAgents=_holdingAgents} -> (not . null) _holdingAgents
        _ -> False

changeAgentPos agent (posX, posY) = 
    let
        Agent {
                agentType=_agentType, 
                agentId=_agentId, 
                state=_state
            } = agent
    in Agent {
        agentType=_agentType, 
        posX=posX, 
        posY=posY, 
        agentId=_agentId, 
        state=_state}        

changeAgentState agent state = 
    let
        Agent {
                posX=_posX, 
                posY=_posY, 
                agentType=_agentType, 
                agentId=_agentId
            } = agent
    in Agent {
        agentType=_agentType, 
        posX=_posX, 
        posY=_posY, 
        agentId=_agentId, 
        state=state}        

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