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

applyMoveBabyActionToEnv env action = env -- TODO

applyCreateDirtBabyActionToEnv env action = env -- TODO