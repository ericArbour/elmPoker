module Main exposing (main)

import Array exposing (..)
import CompareHands.CompareHands exposing (compareHands)
import Html exposing (..)
import Html.Events exposing (..)
import Random exposing (..)


type alias Model =
    { card : String }


initModel : ( Model, Cmd Msg )
initModel =
    ( Model "", Cmd.none )


type Msg
    = Draw
    | NewCard Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw ->
            ( model, Random.generate NewCard (Random.int 0 51) )

        NewCard cardInt ->
            ( Model
                (case Array.get cardInt deck of
                    Just a ->
                        a

                    Nothing ->
                        ""
                )
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


deck : Array String
deck =
    Array.fromList
        [ "2H"
        , "3H"
        , "4H"
        , "5H"
        , "6H"
        , "7H"
        , "8H"
        , "9H"
        , "TH"
        , "JH"
        , "QH"
        , "KH"
        , "AH"
        , "2D"
        , "3D"
        , "4D"
        , "5D"
        , "6D"
        , "7D"
        , "8D"
        , "9D"
        , "TD"
        , "JD"
        , "QD"
        , "KD"
        , "AD"
        , "2S"
        , "3S"
        , "4S"
        , "5S"
        , "6S"
        , "7S"
        , "8S"
        , "9S"
        , "TS"
        , "JS"
        , "QS"
        , "KS"
        , "AS"
        , "2C"
        , "3C"
        , "4C"
        , "5C"
        , "6C"
        , "7C"
        , "8C"
        , "9C"
        , "TC"
        , "JC"
        , "QC"
        , "KC"
        , "AC"
        ]


hand1 =
    [ "AC"
    , "2C"
    , "3C"
    , "4C"
    , "5C"
    ]


hand2 =
    [ "AH"
    , "AD"
    , "AD"
    , "AD"
    , "KH"
    ]


test =
    compareHands hand1 hand2


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (toString model.card) ]
        , button [ onClick Draw ] [ text "Draw " ]
        ]


main =
    Html.program { init = initModel, view = view, update = update, subscriptions = subscriptions }
