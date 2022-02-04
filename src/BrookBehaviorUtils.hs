module BrookBehaviorUtils where
import Environment
import Agent


-- Brook Architecture --
{-
The first predicate that is true, the corresponding action function is applied, 
returning a tuple of the env and the actions to perform.

The only changes in the environment should be in the randomGenerator. Any other changes should be 
reflected in the action list
-}
brookAgent :: [(Env s1 -> Agent -> Bool, Env s1 -> Agent -> (Env s1, [Action]))] -> Env s1 -> Agent -> (Env s1, [Action])
brookAgent beh env agent = 
    let (newEnv, actions, _) = foldl
            (\(accEnv, actions, anyTrue) (pred, actionFun) ->
                if not anyTrue then
                    if pred env agent then
                        let (newCurrEnv, newActions) = actionFun env agent in (newCurrEnv, newActions, True)
                    else (accEnv, [], False)
                else (accEnv, actions, True)) (env, [], False) beh
    in
        (newEnv, actions)