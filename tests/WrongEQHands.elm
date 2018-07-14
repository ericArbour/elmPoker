module WrongEQHands exposing (..)

import CompareHands.CompareHands exposing (justOrder)
import Expect exposing (Expectation)
import Test exposing (..)


wrongEQHands : Test
wrongEQHands =
    describe "Incorrect EQ Hands"
        [ test "Incorrect EQ Hands 1" <|
            \_ ->
                let
                    hand1 =
                        [ "KS", "2H", "8C", "5S", "AC" ]

                    hand2 =
                        [ "AS", "6D", "5D", "KD", "7H" ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "Incorrect EQ Hands 2" <|
            \_ ->
                let
                    hand1 =
                        [ "7S", "8C", "QH", "6C", "5H" ]

                    hand2 =
                        [ "QD", "3C", "2S", "5D", "9S" ]
                in
                Expect.equal LT (justOrder hand1 hand2)
        , test "Incorrect EQ Hands 3" <|
            \_ ->
                let
                    hand1 =
                        [ "3H", "2C", "9C", "KH", "AD" ]

                    hand2 =
                        [ "8D", "7D", "AH", "KC", "6S" ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "Incorrect EQ Hands 4" <|
            \_ ->
                let
                    hand1 =
                        [ "KH", "6D", "AD", "7S", "4H" ]

                    hand2 =
                        [ "AC", "KC", "4S", "2S", "8C" ]
                in
                Expect.equal LT (justOrder hand1 hand2)
        , test "Incorrect EQ Hands 5" <|
            \_ ->
                let
                    hand1 =
                        [ "5C", "TC", "8D", "4D", "2H" ]

                    hand2 =
                        [ "3S", "6D", "TS", "8S", "2C" ]
                in
                Expect.equal LT (justOrder hand1 hand2)
        , test "Incorrect EQ Hands 6" <|
            \_ ->
                let
                    hand1 =
                        [ "QD", "4C", "JS", "7D", "8C" ]

                    hand2 =
                        [ "5S", "QC", "JH", "8S", "6C" ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "Incorrect EQ Hands 7" <|
            \_ ->
                let
                    hand1 =
                        [ "7S", "JS", "6S", "2C", "TH" ]

                    hand2 =
                        [ "4D", "5S", "JD", "6C", "TD" ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "Incorrect EQ Hands 8" <|
            \_ ->
                let
                    hand1 =
                        [ "6H", "7H", "3S", "AC", "9D" ]

                    hand2 =
                        [ "AD", "3H", "9H", "8S", "2H" ]
                in
                Expect.equal LT (justOrder hand1 hand2)
        , test "Incorrect EQ Hands 9" <|
            \_ ->
                let
                    hand1 =
                        [ "6C", "KS", "5D", "4D", "9S" ]

                    hand2 =
                        [ "9D", "7S", "6S", "KD", "2C" ]
                in
                Expect.equal LT (justOrder hand1 hand2)
        , test "Incorrect EQ Hands 10" <|
            \_ ->
                let
                    hand1 =
                        [ "7H", "9H", "2C", "AC", "KS" ]

                    hand2 =
                        [ "4D", "5H", "KC", "9S", "AD" ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        ]
