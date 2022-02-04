import Environment
import Agent


horizontalLayeredAgent :: [Env s1 -> Agent -> (Env s1, [Action])] -> ([(Env s1, Agent, [Action])] -> (Env s1, [Action])) -> Env s1 -> Agent -> (Env s1, [Action])
horizontalLayeredAgent beh selectFunction env agent =
    let
        behaviorActions = foldl (\accList f -> f env agent:accList) [] beh
        selectedBehavior = selectFunction $ map (\(newEnv, actions) -> (newEnv, agent, actions)) behaviorActions
    in
        selectedBehavior