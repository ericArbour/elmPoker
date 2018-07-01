module TwoPair exposing (..)

import CompareHands.CompareHands exposing (justOrder)
import Expect exposing (Expectation)
import Test exposing (..)


twoPair : Test
twoPair =
    describe "weakest two pair hand against the strongest two pair below it"
        [ test "weakest two pair containing threes beats strongest single pair" <|
            \_ ->
                let
                    hand1 =
                        [ "3S"
                        , "3C"
                        , "2C"
                        , "2C"
                        , "4C"
                        ]

                    hand2 =
                        [ "AH"
                        , "AD"
                        , "KD"
                        , "QD"
                        , "JH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest two pair containing fours beats strongest two pair containing threes" <|
            \_ ->
                let
                    hand1 =
                        [ "4S"
                        , "4C"
                        , "2C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "3H"
                        , "3D"
                        , "2D"
                        , "2D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest two pair containing fives beats strongest two pair containing fours" <|
            \_ ->
                let
                    hand1 =
                        [ "5S"
                        , "5C"
                        , "2C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "4H"
                        , "4D"
                        , "3D"
                        , "3D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest two pair containing sixes beats strongest two pair containing fives" <|
            \_ ->
                let
                    hand1 =
                        [ "6S"
                        , "6C"
                        , "2C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "5H"
                        , "5D"
                        , "4D"
                        , "4D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest two pair containing sevens beats strongest two pair containing sixes" <|
            \_ ->
                let
                    hand1 =
                        [ "7S"
                        , "7C"
                        , "2C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "6H"
                        , "6D"
                        , "5D"
                        , "5D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest two pair containing eights beats strongest two pair containing sevens" <|
            \_ ->
                let
                    hand1 =
                        [ "8S"
                        , "8C"
                        , "2C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "7H"
                        , "7D"
                        , "6D"
                        , "6D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest two pair containing nines beats strongest two pair containing eights" <|
            \_ ->
                let
                    hand1 =
                        [ "9S"
                        , "9C"
                        , "2C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "8H"
                        , "8D"
                        , "7D"
                        , "7D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest two pair containing tens beats strongest two pair containing nines" <|
            \_ ->
                let
                    hand1 =
                        [ "TS"
                        , "TC"
                        , "2C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "9H"
                        , "9D"
                        , "8D"
                        , "8D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest two pair containing jacks beats strongest two pair containing tens" <|
            \_ ->
                let
                    hand1 =
                        [ "JS"
                        , "JC"
                        , "2C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "TH"
                        , "TD"
                        , "9D"
                        , "9D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest two pair containing fours queens strongest two pair containing jacks" <|
            \_ ->
                let
                    hand1 =
                        [ "QS"
                        , "QC"
                        , "2C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "JH"
                        , "JD"
                        , "TD"
                        , "TD"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest two pair containing kings beats strongest two pair containing queens" <|
            \_ ->
                let
                    hand1 =
                        [ "KS"
                        , "KC"
                        , "2C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "QH"
                        , "QD"
                        , "JD"
                        , "JD"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test "weakest two pair containing aces beats strongest two pair containing kings" <|
            \_ ->
                let
                    hand1 =
                        [ "AS"
                        , "AC"
                        , "2C"
                        , "2C"
                        , "3C"
                        ]

                    hand2 =
                        [ "KH"
                        , "KD"
                        , "QD"
                        , "QD"
                        , "AH"
                        ]
                in
                Expect.equal GT
                    (justOrder hand1 hand2)
        , test
            "kings, second pair kicker beats weaker second pair kicker with stronger single card kicker"
          <|
            \_ ->
                let
                    hand1 =
                        [ "KS"
                        , "KC"
                        , "3C"
                        , "3C"
                        , "2C"
                        ]

                    hand2 =
                        [ "KH"
                        , "KD"
                        , "2D"
                        , "2D"
                        , "AH"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        , test
            "fours, second pair kicker beats weaker second pair kicker with stronger single card kicker"
          <|
            \_ ->
                let
                    hand1 =
                        [ "4S"
                        , "4C"
                        , "3C"
                        , "3C"
                        , "2C"
                        ]

                    hand2 =
                        [ "4S"
                        , "4C"
                        , "2C"
                        , "2C"
                        , "AC"
                        ]
                in
                Expect.equal GT (justOrder hand1 hand2)
        ]
