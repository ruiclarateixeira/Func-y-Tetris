module Rendering exposing (renderContent)

import Board exposing (..)
import Pieces exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (reverse)


{- Render Functions -}


renderLossMessage : Board -> Html Msg
renderLossMessage board =
    div
        [ style
            [ ( "position", "fixed" )
            , ( "height", "100px" )
            , ( "width", "240px" )
            , ( "margin-left", "-120px" )
            , ( "top", "35%" )
            , ( "left", "50%" )
            , ( "background-color", "green" )
            ]
        ]
        [ div
            [ style
                [ ( "float", "left" )
                , ( "width", "100%" )
                , ( "text-align", "center" )
                , ( "padding", "25px 0 5px 0" )
                , ( "font-size", "2em" )
                ]
            ]
            [ text "You Lost!" ]
        , div
            [ style
                [ ( "float", "left" )
                , ( "width", "100%" )
                , ( "text-align", "center" )
                ]
            ]
            [ button [ onClick Reset ] [ text "Try Again" ] ]
        ]



-- render board as a sequence of rendered rows


renderBoard : Board -> Html msg
renderBoard board =
    let
        scoreText =
            String.append "Score: " (toString board.score)
    in
        div
            [ class "Board"
            , style
                [ ( "display", "table" )
                , ( "margin", "0 auto" )
                ]
            ]
            (List.append
                [ div [] [ text scoreText ] ]
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


renderContent : Board -> Html Msg
renderContent board =
    div
        [ class "content"
        , style
            [ ( "margin", "0 auto" )
            ]
        ]
        [ renderBoard (projectBoard board)
        , if board.lost then
            renderLossMessage board
          else
            div [] []
        ]
