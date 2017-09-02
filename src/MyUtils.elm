module MyUtils exposing (combineLists, findLargestCoordinates, rotateSquareMatrix, indexed2DMap)

import List exposing (length, map, concat)
import List.Extra exposing (getAt)


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


indexed2DMap : (( Int, Int ) -> a -> b) -> List (List a) -> List (List b)
indexed2DMap f matrix =
    let
        projectCell cellIndex rowIndex cell =
            f ( cellIndex, rowIndex ) cell

        projectRow rowIndex row =
            List.indexedMap (\cellIndex cell -> projectCell cellIndex rowIndex cell) row
    in
        List.indexedMap projectRow matrix


rotateSquareMatrix : List (List a) -> List (List (Maybe a))
rotateSquareMatrix input =
    let
        side =
            length input

        newMatrix =
            List.repeat side (List.repeat side Nothing)

        rotate ( x, y ) cell =
            let
                fromX =
                    side - y - 1

                fromY =
                    x

                cRow =
                    Maybe.withDefault [] (getAt fromY input)
            in
                getAt fromX cRow
    in
        indexed2DMap rotate input
