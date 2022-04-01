module Tests exposing (..)

import Test exposing (Test)
import Romanize
import Expect


suite : Test
suite =
    Test.describe "Romanize"
        [ -- Test.todo "Implement the first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
        --Test.test "answer" <|
        --     \_ ->
        --         Question.answer "What is the Answer to the Ultimate Question of Life, The Universe, and Everything?"
        --             |> Expect.equal 42
         Test.test "Testing conversion of non-overline number" <|
            \_ ->
                Romanize.standardConversion 1000
                    |> Expect.equal "M"
        , Test.test "Testing conversion of overline number" <|
            \_ ->
                Romanize.standardConversion 4000
                    |> Expect.equal "_IV"
        ]