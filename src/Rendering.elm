module Rendering exposing (renderContent)

import Board exposing (..)
import Pieces exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (reverse)


{- Render Functions -}
-- render board as a sequence of rendered rows


renderBoard : Board -> Html msg
renderBoard board =
    let
        scoreText =
            String.append "Score: " (toString board.score)
    in
        div
            [ class "Board" ]
            (List.append
                [ div [] [ text scoreText ]
                ]
                (reverse (List.map renderRow board.rows))
            )



-- Render row as a sequence of rendered cells


renderRow : Row -> Html msg
renderRow row =
    div
        [ class "Row"
        , style [ ( "clear", "both" ) ]
        ]
        (List.map renderCell row.cells)



-- Render cell as an HTML colored div


renderCell : PieceType -> Html msg
renderCell pieceType =
    let
        backgroundColor =
            case pieceType of
                LShape ->
                    "blue"

                TShape ->
                    "red"

                IShape ->
                    "purple"

                SShape ->
                    "yellow"

                OShape ->
                    "green"

                None ->
                    "black"
    in
        div
            [ class "Cell"
            , style
                [ ( "float", "left" )
                , ( "height", "30px" )
                , ( "width", "30px" )
                , ( "margin", "0.5px" )
                , ( "background-color", backgroundColor )
                ]
            ]
            [ text "" ]


renderContent : Board -> Html msg
renderContent board =
    div
        [ class "content"
        , style
            [ ( "margin", "0 auto" )
            , ( "width", "350px" )
            ]
        ]
        [ renderBoard (projectBoard board) ]
