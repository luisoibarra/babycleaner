module BehaviorBaby where
import Environment

import BehaviorUtils
import Agent

-- Brook Behavior--

babyBrookBehavior = [
        -- DoNothing When? -> In Playpen | When been holded by a Robot
        (inPlaypen, \env agent -> [DoNothing {agent = agent}]),
        (holededByRobot, \env agent -> [DoNothing {agent = agent}]),
        (\env agent -> True, \env agent -> [Move {agent = agent}, CreateDirt {agent = agent}])
    ]

inPlaypen = inAgentType Playpen

-- Apply Action to Env --

applyMoveBabyActionToEnv env action =
    let
        agent = getAgentFromAction action
        possibleMovePositions = getAllMoveablePositions env [Obstacle, Baby] agent
        (movePos, nextRandEnv) = pickRandomFromListEnv env possibleMovePositions
    in  if null possibleMovePositions then
            env
        else
            moveAgent nextRandEnv agent movePos

applyCreateDirtBabyActionToEnv env action = env -- TODO