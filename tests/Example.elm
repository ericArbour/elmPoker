module Example exposing (..)

import CompareHands exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    test "weakest ace high beats strongest king high" <|
        \_ ->
            let
                hand1 =
                    [ "6C"
                    , "4S"
                    , "3S"
                    , "2C"
                    , "AC"
                    ]

                hand2 =
                    [ "8H"
                    , "TH"
                    , "JD"
                    , "QH"
                    , "KH"
                    ]
            in
            Expect.equal GT (compareHands hand1 hand2)
