module MyUtils exposing (combineLists)

import List exposing (length, map, concat)


-- Combines two lists into all possible combinations of pairs
-- eg.  combineLists ["a", "b", "c"] [1, 2, 3]
--      [ ("a", 1), ("a", 2),  ("a", 3), ("b", 1) ...


combineLists : List a -> List b -> List ( a, b )
combineLists list1 list2 =
    let
        pairUp item1 =
            map (\item2 -> ( item1, item2 )) list2
    in
        if (length list1) == 0 then
            []
        else if (length list2) == 0 then
            []
        else
            concat (map pairUp list1)
