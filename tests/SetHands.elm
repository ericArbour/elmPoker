module SetHands exposing (..)

import CompareHands exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


setHands : Test
setHands =
    describe "weakest set hand against the strongest set below it"
        [ test "weakest set beats strongest two pair" <|
            \_ ->
                let
                    hand1 =
                        [ "2S"
                        , "2C"
                        , "2C"
                        , "3C"
                        , "4C"
                        ]

                    hand2 =
                        [ "AH"
                        , "AD"
                        , "KD"
                        , "KD"
                        , "QH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest set of threes beats strongest set of twos" <|
            \_ ->
                let
                    hand1 =
                        [ "3S"
                        , "3C"
                        , "3C"
                        , "2C"
                        , "4C"
                        ]

                    hand2 =
                        [ "2H"
                        , "2D"
                        , "2D"
                        , "AD"
                        , "KH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest set of fours beats strongest set of threes" <|
            \_ ->
                let
                    hand1 =
                        [ "4S"
                        , "4C"
                        , "4C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "3H"
                        , "3D"
                        , "3D"
                        , "AD"
                        , "KH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest set of fives beats strongest set of fours" <|
            \_ ->
                let
                    hand1 =
                        [ "5S"
                        , "5C"
                        , "5C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "4H"
                        , "4D"
                        , "4D"
                        , "AD"
                        , "KH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest set of sixes beats strongest set of fives" <|
            \_ ->
                let
                    hand1 =
                        [ "6S"
                        , "6C"
                        , "6C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "5H"
                        , "5D"
                        , "5D"
                        , "AD"
                        , "KH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest set of sevens beats strongest set of sixes" <|
            \_ ->
                let
                    hand1 =
                        [ "7S"
                        , "7C"
                        , "7C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "6H"
                        , "6D"
                        , "6D"
                        , "AD"
                        , "KH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest set of eights beats strongest set of sevens" <|
            \_ ->
                let
                    hand1 =
                        [ "8S"
                        , "8C"
                        , "8C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "7H"
                        , "7D"
                        , "7D"
                        , "AD"
                        , "KH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest set of nines beats strongest set of eights" <|
            \_ ->
                let
                    hand1 =
                        [ "9S"
                        , "9C"
                        , "9C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "8H"
                        , "8D"
                        , "8D"
                        , "AD"
                        , "KH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest set of tens beats strongest set of nines" <|
            \_ ->
                let
                    hand1 =
                        [ "TS"
                        , "TC"
                        , "TC"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "9H"
                        , "9D"
                        , "9D"
                        , "AD"
                        , "KH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest set of jacks beats strongest set of tens" <|
            \_ ->
                let
                    hand1 =
                        [ "JS"
                        , "JC"
                        , "JC"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "TH"
                        , "TD"
                        , "TD"
                        , "AD"
                        , "KH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest set of queens beats strongest set of jacks" <|
            \_ ->
                let
                    hand1 =
                        [ "QS"
                        , "QC"
                        , "QC"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "JH"
                        , "JD"
                        , "JD"
                        , "AD"
                        , "KH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest set of kings beats strongest set of queens" <|
            \_ ->
                let
                    hand1 =
                        [ "KS"
                        , "KC"
                        , "KC"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "QH"
                        , "QD"
                        , "QD"
                        , "AD"
                        , "KH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "weakest set of aces beats strongest set of kings" <|
            \_ ->
                let
                    hand1 =
                        [ "AS"
                        , "AC"
                        , "AC"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "KH"
                        , "KD"
                        , "KD"
                        , "AD"
                        , "QH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        ]
