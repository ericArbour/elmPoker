module Flushes exposing (..)

import CompareHands.CompareHands exposing (justOrder)
import Expect exposing (Expectation)
import Test exposing (..)


flushes : Test
flushes =
    describe "flush against the strongest hand below it"
        [ test "weakest flush beats strongest straight" <|
            \_ ->
                let
                    hand1 =
                        [ "2S"
                        , "3S"
                        , "4S"
                        , "5S"
                        , "7S"
                        ]

                    hand2 =
                        [ "AH"
                        , "KD"
                        , "QD"
                        , "JD"
                        , "TH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "seven high flush with six kicker beats seven high flush with five kicker" <|
            \_ ->
                let
                    hand1 =
                        [ "2S"
                        , "3S"
                        , "4S"
                        , "6S"
                        , "7S"
                        ]

                    hand2 =
                        [ "2S"
                        , "3S"
                        , "4S"
                        , "5S"
                        , "7S"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "eight high flush beats seven high flush" <|
            \_ ->
                let
                    hand1 =
                        [ "2S"
                        , "3S"
                        , "4S"
                        , "5S"
                        , "8S"
                        ]

                    hand2 =
                        [ "2S"
                        , "4S"
                        , "5S"
                        , "6S"
                        , "7S"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "nine high flush beats eight high flush" <|
            \_ ->
                let
                    hand1 =
                        [ "2S"
                        , "3S"
                        , "4S"
                        , "5S"
                        , "9S"
                        ]

                    hand2 =
                        [ "3S"
                        , "5S"
                        , "6S"
                        , "7S"
                        , "8S"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "ten high flush beats nine high flush" <|
            \_ ->
                let
                    hand1 =
                        [ "2S"
                        , "3S"
                        , "4S"
                        , "5S"
                        , "TS"
                        ]

                    hand2 =
                        [ "4S"
                        , "6S"
                        , "7S"
                        , "8S"
                        , "9S"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "jack high flush beats ten high flush" <|
            \_ ->
                let
                    hand1 =
                        [ "2S"
                        , "3S"
                        , "4S"
                        , "5S"
                        , "JS"
                        ]

                    hand2 =
                        [ "5S"
                        , "7S"
                        , "8S"
                        , "9S"
                        , "TS"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "queen high flush beats jack high flush" <|
            \_ ->
                let
                    hand1 =
                        [ "2S"
                        , "3S"
                        , "4S"
                        , "5S"
                        , "QS"
                        ]

                    hand2 =
                        [ "6S"
                        , "8S"
                        , "9S"
                        , "TS"
                        , "JS"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "king high flush beats queen high flush" <|
            \_ ->
                let
                    hand1 =
                        [ "2S"
                        , "3S"
                        , "4S"
                        , "5S"
                        , "KS"
                        ]

                    hand2 =
                        [ "7S"
                        , "9S"
                        , "TS"
                        , "JS"
                        , "QS"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "ace high flush beats king high flush" <|
            \_ ->
                let
                    hand1 =
                        [ "2S"
                        , "3S"
                        , "4S"
                        , "6S"
                        , "AS"
                        ]

                    hand2 =
                        [ "8S"
                        , "TS"
                        , "JS"
                        , "QS"
                        , "KS"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "ace high flush  with king kicker beats ace high flush with six kicker" <|
            \_ ->
                let
                    hand1 =
                        [ "9S"
                        , "JS"
                        , "QS"
                        , "KS"
                        , "AS"
                        ]

                    hand2 =
                        [ "2S"
                        , "3S"
                        , "4S"
                        , "6S"
                        , "AS"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        ]
