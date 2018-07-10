module Main exposing (main)

import Array exposing (..)
import CompareHands.CompareHands exposing (Result, compareHands)
import Deck exposing (deck)
import Html exposing (..)
import Http exposing (..)
import Random exposing (..)
import Time exposing (Time, millisecond)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)


type alias Model =
    { current : Int
    , hand1 : List String
    , hand2 : List String
    , deck : Array String
    , limit : Int
    , result : CompareHands.CompareHands.Result
    }


type alias Outcome =
    { hand1 : String
    , hand2 : String
    , hand1Type : String
    , hand2Type : String
    , result : String
    , deck : String
    }


initModel : Model
initModel =
    Model 1 [] [] deck 5 { order = EQ, hand1 = "", hand2 = "" }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


type Msg
    = DrawCard Time
    | DealCard Int
    | Evaluate
    | Submit
    | GoodData
    | BadData Http.Error


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
            if List.length model.hand1 == model.limit && List.length model.hand2 == model.limit then
                update Evaluate model
            else if List.length model.hand1 /= model.limit && model.current == 1 then
                ( { model
                    | hand1 = intToCard cardInt model.deck :: model.hand1
                    , deck = Array.filter (\card -> card /= intToCard cardInt model.deck) model.deck
                    , current = 2
                  }
                , Cmd.none
                )
            else if List.length model.hand2 /= model.limit && model.current == 2 then
                ( { model
                    | hand2 = intToCard cardInt model.deck :: model.hand2
                    , deck = Array.filter (\card -> card /= intToCard cardInt model.deck) model.deck
                    , current = 1
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        Evaluate ->
            update Submit { model | result = compareHands model.hand1 model.hand2 }

        Submit ->
            ( initModel
            , submit
                { hand1 = toString model.hand1
                , hand2 = toString model.hand2
                , hand1Type = model.result.hand1
                , hand2Type = model.result.hand2
                , result = toString model.result.order
                , deck = toString (Array.toList model.deck)
                }
            )

        GoodData->
            (model, Cmd.none)

        BadData error ->
            (model, Cmd.none)



submit : Outcome -> Cmd Msg
submit outcome =
  let
    url =
      "http://localhost:64483/api/showdowns"

    body =
        Debug.log "outcome"
        outcome
            |> outcomeEncoder
            |> Http.jsonBody

    request =
      Http.post url body outcomeDecoder
  in
    Http.send completedData request

completedData : Result.Result Http.Error a -> Msg
completedData result =
    case result of
        Ok data ->
            GoodData 

        Err error ->
            BadData error


outcomeEncoder : Outcome -> Encode.Value
outcomeEncoder outcome = 
    Encode.object 
        [ ("hand1", Encode.string outcome.hand1)
        , ("hand2", Encode.string outcome.hand2)
        , ("hand1Type", Encode.string outcome.hand1Type)
        , ("hand2Type", Encode.string outcome.hand2Type)
        , ("result", Encode.string outcome.result)
        , ("deck", Encode.string outcome.deck) 
        ]      

outcomeDecoder : Decoder String
outcomeDecoder =
    Decode.field "access_token" Decode.string 


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (millisecond * 100) DrawCard


view : Model -> Html Msg
view model =
    div []
        []


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }
