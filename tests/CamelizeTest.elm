module CamelizeTest exposing (camelizeTest)

import Char
import Expect
import Fuzz exposing (..)
import Random.Pcg as Random
import Regex
import Shrink
import String
import String exposing (uncons)
import String.Extra exposing (..)
import Test exposing (..)


camelizeTest : Test
camelizeTest =
    describe "camelize"
        [ fuzz string "It does not contain dashes" <|
            \s ->
                camelize s
                    |> String.contains "-"
                    |> Expect.false "Camelize should remove dashes"
        , fuzz string "It does not contain underscores" <|
            \s ->
                camelize s
                    |> String.contains "-"
                    |> Expect.false "Camelize should remove underscores"
        , fuzz string "It is the same lowercased string after removing the dashes and spaces" <|
            \s ->
                let
                    expected = replace "-" ""
                        >> replace "_" ""
                        >> Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> "")
                        >> String.toLower
                in
                    camelize s
                        |> String.toLower
                        |> Expect.equal (expected s)
        , fuzz (validWords '-') "The first letter after each dash is capitalized" <|
            \s ->
                camelize s
                    |> Expect.equal (runCamelize "-" s)
        , fuzz (validWords ' ') "The first letter after each space is capitalized" <|
            \s ->
                camelize s
                    |> Expect.equal (runCamelize " " s)
        ]


runCamelize : String -> String -> String
runCamelize separator string =
    string
        |> String.trim
        |> replace (separator ++ separator) separator
        |> String.split separator
        |> List.indexedMap capitalizeOdds
        |> String.join ""


capitalizeOdds : Int -> String -> String
capitalizeOdds pos str =
    if pos > 0 then
        toSentenceCase str
    else
        str


latinChars : List (Random.Generator Char)
latinChars =
    [ Random.map Char.fromCode (Random.int 97 122), Random.map Char.fromCode (Random.int 65 90) ]


withChar : Char -> Random.Generator Char
withChar ch =
    Random.choices <| latinChars ++ [ Random.constant ch ]


validWords : Char -> Fuzzer String
validWords ch =
    let
        producer =
            Random.int 1 10 |> Random.andThen (\i -> Random.map String.fromList (Random.list i (withChar ch)))
    in
        custom producer Shrink.string
