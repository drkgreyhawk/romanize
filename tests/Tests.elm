module Tests exposing (..)

import Expect
import Fuzz
import Romanize
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Romanize"
        [ Test.describe "Romanize.standardConversion"
            [ Test.fuzz (Fuzz.intRange -50 0) "Test conversion of less than 1" <|
                \num ->
                    Romanize.standardConversion num
                        |> Expect.equal ("Romanize | Integer out of bounds: " ++ String.fromInt num)
            , Test.test "Test conversion of 1s = 1" <|
                \_ ->
                    Romanize.standardConversion 1
                        |> Expect.equal "I"
            , Test.test "Test conversion of 1s = 2" <|
                \_ ->
                    Romanize.standardConversion 2
                        |> Expect.equal "II"
            , Test.test "Test conversion of 1s = 3" <|
                \_ ->
                    Romanize.standardConversion 3
                        |> Expect.equal "III"
            , Test.test "Test conversion of 1s = 4" <|
                \_ ->
                    Romanize.standardConversion 4
                        |> Expect.equal "IV"
            , Test.test "Test conversion of 1s = 5" <|
                \_ ->
                    Romanize.standardConversion 5
                        |> Expect.equal "V"
            , Test.test "Test conversion of 1s = 6" <|
                \_ ->
                    Romanize.standardConversion 6
                        |> Expect.equal "VI"
            , Test.test "Test conversion of 1s = 7" <|
                \_ ->
                    Romanize.standardConversion 7
                        |> Expect.equal "VII"
            , Test.test "Test conversion of 1s = 8" <|
                \_ ->
                    Romanize.standardConversion 8
                        |> Expect.equal "VIII"
            , Test.test "Test conversion of 1s = 9" <|
                \_ ->
                    Romanize.standardConversion 9
                        |> Expect.equal "IX"
            , Test.test "Test conversion of 10s = 10" <|
                \_ ->
                    Romanize.standardConversion 10
                        |> Expect.equal "X"
            , Test.test "Test conversion of 10s = 20" <|
                \_ ->
                    Romanize.standardConversion 20
                        |> Expect.equal "XX"
            , Test.test "Test conversion of 10s = 30" <|
                \_ ->
                    Romanize.standardConversion 30
                        |> Expect.equal "XXX"
            , Test.test "Test conversion of 10s = 40" <|
                \_ ->
                    Romanize.standardConversion 40
                        |> Expect.equal "XL"
            , Test.test "Test conversion of 10s = 50" <|
                \_ ->
                    Romanize.standardConversion 50
                        |> Expect.equal "L"
            , Test.test "Test conversion of 10s = 60" <|
                \_ ->
                    Romanize.standardConversion 60
                        |> Expect.equal "LX"
            , Test.test "Test conversion of 10s = 70" <|
                \_ ->
                    Romanize.standardConversion 70
                        |> Expect.equal "LXX"
            , Test.test "Test conversion of 10s = 80" <|
                \_ ->
                    Romanize.standardConversion 80
                        |> Expect.equal "LXXX"
            , Test.test "Test conversion of 10s = 90" <|
                \_ ->
                    Romanize.standardConversion 90
                        |> Expect.equal "XC"
            , Test.test "Test conversion of 100s = 100" <|
                \_ ->
                    Romanize.standardConversion 100
                        |> Expect.equal "C"
            , Test.test "Test conversion of 100s = 200" <|
                \_ ->
                    Romanize.standardConversion 200
                        |> Expect.equal "CC"
            , Test.test "Test conversion of 100s = 300" <|
                \_ ->
                    Romanize.standardConversion 300
                        |> Expect.equal "CCC"
            , Test.test "Test conversion of 100s = 400" <|
                \_ ->
                    Romanize.standardConversion 400
                        |> Expect.equal "CD"
            , Test.test "Test conversion of 100s = 500" <|
                \_ ->
                    Romanize.standardConversion 500
                        |> Expect.equal "D"
            , Test.test "Test conversion of 100s = 600" <|
                \_ ->
                    Romanize.standardConversion 600
                        |> Expect.equal "DC"
            , Test.test "Test conversion of 100s = 700" <|
                \_ ->
                    Romanize.standardConversion 700
                        |> Expect.equal "DCC"
            , Test.test "Test conversion of 100s = 800" <|
                \_ ->
                    Romanize.standardConversion 800
                        |> Expect.equal "DCCC"
            , Test.test "Test conversion of 100s = 900" <|
                \_ ->
                    Romanize.standardConversion 900
                        |> Expect.equal "CM"
            , Test.test "Test conversion of 1000s = 1000" <|
                \_ ->
                    Romanize.standardConversion 1000
                        |> Expect.equal "M"
            , Test.test "Test conversion of 1000s = 2000" <|
                \_ ->
                    Romanize.standardConversion 2000
                        |> Expect.equal "MM"
            , Test.test "Test conversion of 1000s = 3000" <|
                \_ ->
                    Romanize.standardConversion 3000
                        |> Expect.equal "MMM"
            , Test.fuzz (Fuzz.intRange 4000 4050) "Test conversion of greater than 3999" <|
                \num ->
                    Romanize.standardConversion num
                        |> Expect.equal ("Romanize | Integer out of bounds: " ++ String.fromInt num)
            ]
        ]
