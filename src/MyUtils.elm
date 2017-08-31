module MyUtils exposing (combineLists, findLargestCoordinates)

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



-- Get pair of largest x and largest y of all coordinates


findLargestCoordinates : List ( Int, Int ) -> ( Int, Int )
findLargestCoordinates coordinates =
    let
        h =
            List.head coordinates

        t =
            List.tail coordinates

        recursive : Maybe (List ( Int, Int )) -> Maybe ( Int, Int ) -> ( Int, Int )
        recursive c l =
            let
                cs =
                    Maybe.withDefault [] c

                ( lx, ly ) =
                    Maybe.withDefault ( 0, 0 ) l

                current =
                    let
                        ( x, y ) =
                            Maybe.withDefault ( 0, 0 ) (List.head cs)
                    in
                        if (lx > x) && (ly > y) then
                            ( lx, ly )
                        else if (ly > y) then
                            ( x, ly )
                        else if (lx > x) then
                            ( lx, y )
                        else
                            ( x, y )
            in
                if (length cs) == 0 then
                    ( lx, ly )
                else
                    recursive (List.tail cs) (Just current)
    in
        recursive t h
