module Main exposing (main)

import Array exposing (..)
import CompareHands.CompareHands exposing (compareHands)
import Deck exposing (deck)
import Html exposing (..)
import Html.Events exposing (..)
import Random exposing (..)


type alias Model =
    { current : Int, hand1 : List String, hand2 : List String, deck : Array String, limit : Int }


initModel : ( Model, Cmd Msg )
initModel =
    ( Model 1 [] [] deck 5, Cmd.none )


type Msg
    = DrawCard
    | DealCard Int


intToCard : Int -> Array String -> String
intToCard index deck =
    case Array.get index deck of
        Just a ->
            a

        Nothing ->
            ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DrawCard ->
            ( model, Random.generate DealCard (Random.int 0 (Array.length model.deck - 1)) )

        DealCard cardInt ->
            if model.current == 1 then
                if List.length model.hand1 == model.limit then
                    ( model, Cmd.none )
                else
                    ( { model
                        | hand1 = intToCard cardInt model.deck :: model.hand1
                        , hand2 = model.hand2
                        , deck = Array.filter (\card -> card /= intToCard cardInt model.deck) model.deck
                        , current = 2
                      }
                    , Cmd.none
                    )
            else if model.current == 2 then
                if List.length model.hand2 == model.limit then
                    ( model, Cmd.none )
                else
                    ( { model
                        | hand1 = model.hand1
                        , hand2 = intToCard cardInt model.deck :: model.hand2
                        , deck = Array.filter (\card -> card /= intToCard cardInt model.deck) model.deck
                        , current = 1
                      }
                    , Cmd.none
                    )
            else
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


test : Msg
test =
    DrawCard


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (toString model) ]
        , button [ onClick test ] [ text "Draw " ]
        , div [] []
        ]


main =
    Html.program { init = initModel, view = view, update = update, subscriptions = subscriptions }
