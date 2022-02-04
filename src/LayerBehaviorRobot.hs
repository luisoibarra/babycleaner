module LayerBehaviorRobot where

import BehaviorRobot
import Data.List
import BehaviorUtils
import Environment
import Agent
import Debug.Trace


robotLayerBehavior = [
        takeBabyToPlaypen,
        searchBaby,
        cleanDirt
        -- getDoNothingAction
    ]

layerSelectFunctionRobot :: [(Env s, Agent, b)] -> (Env s, b)
layerSelectFunctionRobot possibleActions =
    let
        -- possibleActions = trace (show $ map snd _possibleActions) _possibleActions
        (env, agent, actions) = head $ sortOn getRobotBehPriority possibleActions
    in
        -- trace (show (env, actions)) (env, actions)
        (env, actions) 

getRobotBehPriority (env, agent, actions)
    | trace ("Holding" ++ show (isCurrentlyHoldingBaby env agent)) isCurrentlyHoldingBaby env agent = 0
    | trace ("Free Babies" ++ show (existFreeBabies env agent)) existFreeBabies env agent = 1
    | trace ("Env Dirty" ++ show (isEnvDirty env agent)) isEnvDirty env agent = 2
    | isCurrentlyHoldingBaby env agent = 0
    | existFreeBabies env agent = 1
    | isEnvDirty env agent = 2
    | otherwise = 3
