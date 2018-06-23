module UncleJoey exposing (..)

import CompareHands exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


uncleJoey : Test
uncleJoey =
    describe "weakest full house hand against the strongest below it"
        [ test "weakest full beats strongest flush" <|
            \_ ->
                let
                    hand1 =
                        [ "2S"
                        , "2C"
                        , "2C"
                        , "3C"
                        , "3C"
                        ]

                    hand2 =
                        [ "AS"
                        , "KS"
                        , "QS"
                        , "JS"
                        , "9S"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "threes over twos beats twos over aces" <|
            \_ ->
                let
                    hand1 =
                        [ "3S"
                        , "3C"
                        , "3C"
                        , "4C"
                        , "4C"
                        ]

                    hand2 =
                        [ "2H"
                        , "2D"
                        , "2D"
                        , "AD"
                        , "AH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "fours over twos beats threes over aces" <|
            \_ ->
                let
                    hand1 =
                        [ "4S"
                        , "4C"
                        , "4C"
                        , "2C"
                        , "2C"
                        ]

                    hand2 =
                        [ "3H"
                        , "3D"
                        , "3D"
                        , "AD"
                        , "AH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "fives over twos beats fours over aces" <|
            \_ ->
                let
                    hand1 =
                        [ "5S"
                        , "5C"
                        , "5C"
                        , "2C"
                        , "2C"
                        ]

                    hand2 =
                        [ "4H"
                        , "4D"
                        , "4D"
                        , "AD"
                        , "AH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "sixes over twos beats fives over aces" <|
            \_ ->
                let
                    hand1 =
                        [ "6S"
                        , "6C"
                        , "6C"
                        , "2C"
                        , "2C"
                        ]

                    hand2 =
                        [ "5H"
                        , "5D"
                        , "5D"
                        , "AD"
                        , "AH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "sevens over twos beats sixes over aces" <|
            \_ ->
                let
                    hand1 =
                        [ "7S"
                        , "7C"
                        , "7C"
                        , "2C"
                        , "2C"
                        ]

                    hand2 =
                        [ "6H"
                        , "6D"
                        , "6D"
                        , "AD"
                        , "AH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "eights over twos sevens twos over aces" <|
            \_ ->
                let
                    hand1 =
                        [ "8S"
                        , "8C"
                        , "8C"
                        , "2C"
                        , "2C"
                        ]

                    hand2 =
                        [ "7H"
                        , "7D"
                        , "7D"
                        , "AD"
                        , "AH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "nines over twos beats eights over aces" <|
            \_ ->
                let
                    hand1 =
                        [ "9S"
                        , "9C"
                        , "9C"
                        , "2C"
                        , "2C"
                        ]

                    hand2 =
                        [ "8H"
                        , "8D"
                        , "8D"
                        , "AD"
                        , "AH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "tens over twos beats nines over aces" <|
            \_ ->
                let
                    hand1 =
                        [ "TS"
                        , "TC"
                        , "TC"
                        , "2C"
                        , "2C"
                        ]

                    hand2 =
                        [ "9H"
                        , "9D"
                        , "9D"
                        , "AD"
                        , "AH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "jacks over twos beats tens over aces" <|
            \_ ->
                let
                    hand1 =
                        [ "JS"
                        , "JC"
                        , "JC"
                        , "2C"
                        , "2C"
                        ]

                    hand2 =
                        [ "TH"
                        , "TD"
                        , "TD"
                        , "AD"
                        , "AH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "queens over twos beats jacks over aces" <|
            \_ ->
                let
                    hand1 =
                        [ "QS"
                        , "QC"
                        , "QC"
                        , "2C"
                        , "2C"
                        ]

                    hand2 =
                        [ "JH"
                        , "JD"
                        , "JD"
                        , "AD"
                        , "AH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "kings over twos beats queens over aces" <|
            \_ ->
                let
                    hand1 =
                        [ "KS"
                        , "KC"
                        , "KC"
                        , "2C"
                        , "2C"
                        ]

                    hand2 =
                        [ "QH"
                        , "QD"
                        , "QD"
                        , "AD"
                        , "AH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        , test "aces over twos beats kings over aces" <|
            \_ ->
                let
                    hand1 =
                        [ "AS"
                        , "AC"
                        , "AC"
                        , "2C"
                        , "2C"
                        ]

                    hand2 =
                        [ "KH"
                        , "KD"
                        , "KD"
                        , "QD"
                        , "QH"
                        ]
                in
                Expect.equal GT (compareHands hand1 hand2)
        ]
