module CompareHands.Checkers exposing (..)

import Array exposing (..)
import CompareHands.GetValues exposing (getAceLowIndex, getIndex)
import CompareHands.Helpers exposing (getSuit, removeDups)


checkStraight : List String -> Maybe String
checkStraight hand =
    if List.length hand == 5 then
        let
            arrayHand =
                Array.fromList hand

            first : Maybe String
            first =
                Array.get 0 arrayHand

            last : Maybe String
            last =
                Array.get 4 arrayHand

            aceHighResult : Maybe String
            aceHighResult =
                case first of
                    Just f ->
                        case last of
                            Just l ->
                                if getIndex f - getIndex l == 4 then
                                    Just f
                                else
                                    Nothing

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing

            aceLowHand : Maybe (Array String)
            aceLowHand =
                case first of
                    Just f ->
                        if f == "A" then
                            Just (Array.push "A" (Array.slice 1 5 arrayHand))
                        else
                            Nothing

                    Nothing ->
                        Nothing

            aceLowFirst : Maybe String
            aceLowFirst =
                case aceLowHand of
                    Just array ->
                        Array.get 0 array

                    Nothing ->
                        Nothing

            aceLowLast : Maybe String
            aceLowLast =
                case aceLowHand of
                    Just array ->
                        Array.get 4 array

                    Nothing ->
                        Nothing

            aceLowResult : Maybe String
            aceLowResult =
                case aceLowFirst of
                    Just f ->
                        case aceLowLast of
                            Just l ->
                                if getAceLowIndex f - getAceLowIndex l == 4 then
                                    Just f
                                else
                                    Nothing

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
        in
        case aceHighResult of
            Just face ->
                Just face

            Nothing ->
                case aceLowResult of
                    Just face ->
                        Just face

                    Nothing ->
                        Nothing
    else
        Nothing


checkFlush : List String -> Bool
checkFlush hand =
    if
        List.length
            (List.foldr
                removeDups
                []
                (List.map getSuit hand)
            )
            == 1
    then
        True
    else
        False
