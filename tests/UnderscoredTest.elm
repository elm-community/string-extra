module UnderscoredTest exposing (underscoredTest)

import Char
import Expect
import Fuzz exposing (..)
import Random
import Shrink
import String exposing (replace, uncons)
import String.Extra exposing (..)
import Test exposing (..)


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
        , fuzz
            nonEmptyString
            "It puts an underscore before each uppercase characters group unless it starts with uppercase"
          <|
            \s ->
                underscored s
                    |> Expect.equal (replaceUppercase s |> String.toLower)
        ]


char : Random.Generator Char
char =
    --Random.uniform (Random.map Char.fromCode (Random.int 97 122)) [ Random.map Char.fromCode (Random.int 65 90) ]
    Random.uniform ( 97, 122 ) [ ( 65, 90 ) ]
        |> Random.andThen (\( a, b ) -> Random.int a b)
        |> Random.map Char.fromCode


nonEmptyString : Fuzzer String
nonEmptyString =
    let
        producer =
            Random.int 1 10 |> Random.andThen (\i -> Random.map String.fromList (Random.list i char))
    in
    custom producer Shrink.string


replaceUppercase : String -> String
replaceUppercase string =
    string
        |> String.toList
        |> List.indexedMap Tuple.pair
        |> List.foldr recordUpperCasePositions []
        |> List.foldl reduceList []
        |> List.foldl replacePositions string


recordUpperCasePositions : ( Int, Char ) -> List ( Int, Char ) -> List ( Int, Char )
recordUpperCasePositions ( index, char_ ) acc =
    if Char.isUpper char_ then
        ( index, char_ ) :: acc

    else
        acc


reduceList : ( Int, Char ) -> List ( Int, Int, Char ) -> List ( Int, Int, Char )
reduceList ( index, char_ ) acc =
    case acc of
        ( start, end, c ) :: rest ->
            if index == end + 1 then
                ( start, index, c ) :: rest

            else
                ( index, index, char_ ) :: acc

        [] ->
            ( index, index, char_ ) :: acc


replacePositions : ( Int, Int, Char ) -> String -> String
replacePositions ( start, _, c ) string =
    if start == 0 then
        string

    else
        replaceSlice ("_" ++ String.fromChar c) start (start + 1) string
