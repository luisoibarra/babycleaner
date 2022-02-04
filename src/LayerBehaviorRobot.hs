import BehaviorRobot


layerBehaviorRobot = [
        takeBabyToPlaypen,
        searchBaby,
        cleanDirt
    ]

layerSelectFunctionRobot possibleActions =
    let
        robotPriority = sortOn
    in
        (0, [])