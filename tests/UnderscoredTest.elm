module UnderscoredTest exposing (underscoredTest)

import Char
import Expect
import Fuzz exposing (..)
import String exposing (replace, uncons)
import String.Extra exposing (..)
import Test exposing (..)
import TestData


underscoredTest : Test
underscoredTest =
    describe "underscored"
        [ fuzz string "It is a lowercased string" <|
            \s ->
                underscored s
                    |> String.toLower
                    |> Expect.equal (underscored s |> String.toLower)
        , fuzz string "It replaces spaces and dashes with an underscore" <|
            \s ->
                let
                    expected =
                        String.toLower
                            >> String.trim
                            >> replace "  " " "
                            >> replace " " "-"
                            >> replace "-" "_"
                            >> replace "__" "_"
                            >> replace "__" "_"
                in
                underscored (String.toLower s)
                    |> Expect.equal (expected s)
        , fuzz TestData.randomStrings "It puts an underscore before each uppercase characters group unless it starts with uppercase" <|
            \s ->
                underscored s
                    |> Expect.equal (replaceUppercase s |> String.toLower)
        ]


replaceUppercase : String -> String
replaceUppercase string =
    string
        |> String.toList
        |> List.indexedMap Tuple.pair
        |> List.foldr recordUpperCasePositions []
        |> List.foldl reduceList []
        |> List.foldl replacePositions string


recordUpperCasePositions : ( Int, Char ) -> List ( Int, Char ) -> List ( Int, Char )
recordUpperCasePositions ( index, char ) acc =
    if Char.isUpper char then
        ( index, char ) :: acc

    else
        acc


reduceList : ( Int, Char ) -> List ( Int, Int, Char ) -> List ( Int, Int, Char )
reduceList ( index, char ) acc =
    case acc of
        ( start, end, c ) :: rest ->
            if index == end + 1 then
                ( start, index, c ) :: rest

            else
                ( index, index, char ) :: acc

        [] ->
            ( index, index, char ) :: acc


replacePositions : ( Int, Int, Char ) -> String -> String
replacePositions ( start, _, c ) string =
    if start == 0 then
        string

    else
        replaceSlice ("_" ++ String.fromChar c) start (start + 1) string
