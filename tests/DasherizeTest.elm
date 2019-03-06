module DasherizeTest exposing (dasherizeTest)

import Char
import Expect
import Fuzz exposing (..)
import Random
import Shrink
import String exposing (replace, uncons)
import String.Extra exposing (..)
import Test exposing (..)


dasherizeTest : Test
dasherizeTest =
    describe "dasherize"
        [ fuzz string "It is a lowercased string" <|
            \s ->
                dasherize s
                    |> String.toLower
                    |> Expect.equal (dasherize s)
        , fuzz string "It replaces spaces and underscores with a dash" <|
            \s ->
                let
                    expected =
                        String.toLower
                            >> String.trim
                            >> replace "  " " "
                            >> replace " " "-"
                            >> replace "_" "-"
                            >> replace "--" "-"
                            >> replace "--" "-"
                in
                dasherize (String.toLower s)
                    |> String.toLower
                    |> Expect.equal (expected s)
        , fuzz nonEmptyString "It puts dash before every single uppercase character" <|
            \s ->
                dasherize s
                    |> Expect.equal (replaceUppercase s |> String.toLower)
        ]


char : Random.Generator Char
char =
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
        |> List.map
            (\c ->
                if Char.isUpper c then
                    "-" ++ String.fromChar c

                else
                    String.fromChar c
            )
        |> String.join ""
