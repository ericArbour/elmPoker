module Main exposing (main)

import Array exposing (..)
import CompareHands.CompareHands exposing (compareHands)
import Deck exposing (deck)
import Html exposing (..)
import Html.Events exposing (..)
import Random exposing (..)
import Time exposing (Time, millisecond)


type alias Model =
    { current : Int
    , hand1 : ( List String, String )
    , hand2 : ( List String, String )
    , deck : Array String
    , limit : Int
    , result : Maybe Order
    }


initModel : ( Model, Cmd Msg )
initModel =
    ( Model 1 ( [], "" ) ( [], "" ) deck 5 Nothing, Cmd.none )


type Msg
    = DrawCard Time
    | DealCard Int
    | Evaluate


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
        DrawCard newTime ->
            ( model, Random.generate DealCard (Random.int 0 (Array.length model.deck - 1)) )

        DealCard cardInt ->
            if (model.hand1 |> Tuple.first |> List.length) == model.limit && (model.hand2 |> Tuple.first |> List.length) == model.limit then
                update Evaluate model
            else if (model.hand1 |> Tuple.first |> List.length) /= model.limit && model.current == 1 then
                ( { model
                    | hand1 = Tuple.mapFirst (\hand -> intToCard cardInt model.deck :: hand) model.hand1
                    , deck = Array.filter (\card -> card /= intToCard cardInt model.deck) model.deck
                    , current = 2
                  }
                , Cmd.none
                )
            else if (model.hand2 |> Tuple.first |> List.length) /= model.limit && model.current == 2 then
                ( { model
                    | hand2 = Tuple.mapFirst (\hand -> intToCard cardInt model.deck :: hand) model.hand2
                    , deck = Array.filter (\card -> card /= intToCard cardInt model.deck) model.deck
                    , current = 1
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        Evaluate ->
            ( { model | result = Just (compareHands (Tuple.first model.hand1) (Tuple.first model.hand2)) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (millisecond * 100) DrawCard


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (toString model) ]
        ]


main =
    Html.program { init = initModel, view = view, update = update, subscriptions = subscriptions }
