module LayerBehaviorRobot where

import BrookBehaviorRobot
import Data.List
import BehaviorUtils
import Environment
import Agent
import Debug.Trace
import RandomUtils


robotLayerBehavior = [
        takeBabyToPlaypen,
        searchBaby,
        cleanDirt
        -- getDoNothingAction
    ]

randomLayerSelectFunctionRobot possibleActions =
    let 
        (currEnv, _, _) = head possibleActions 
        ((_, _, actions), nextEnv) = pickRandomFromListEnv currEnv possibleActions  
    in
        (nextEnv, actions)

layerSelectFunctionRobot :: [(Env s1, Agent, [Action])] -> (Env s1, [Action])
layerSelectFunctionRobot possibleActions =
    let
        -- possibleActions = trace (show $ map snd _possibleActions) _possibleActions
        (env, agent, actions) = head $ sortOn getRobotBehPriority possibleActions
    in
        -- trace (show (env, actions)) (env, actions)
        (env, actions)

getRobotBehPriority :: (Env s1, Agent, [Action]) -> Int
getRobotBehPriority (env, agent, actions)
    | isCurrentlyHoldingBaby env agent && all 
        (\a -> case a of 
                Move {} -> True 
                LeaveBaby {} -> True
                _ -> False) actions = 0
    | existFreeBabies env agent && all 
        (\a -> case a of 
                Move {} -> True 
                PickBaby {} -> True
                _ -> False) actions = 1
    | isEnvDirty env agent && all 
        (\a -> case a of 
                Move {} -> True 
                Clean {} -> True
                _ -> False) actions = 2
    | otherwise = 3
