module HumanizeTest exposing (humanizeTest)

import Char
import Expect
import Fuzz exposing (..)
import Random.Pcg as Random
import Regex
import Shrink
import String
import String.Extra exposing (..)
import Test exposing (..)
import Tuple exposing (first, second)


humanizeTest : Test
humanizeTest =
    describe "humanize"
        [ fuzz (validWords []) "It starts with an uppercase letter after trimming" <|
            \s ->
                let
                    expected =
                        String.trim
                            >> toSentenceCase
                            >> String.uncons
                            >> Maybe.map (first >> String.fromChar)
                            >> Maybe.withDefault ""
                in
                    humanize s
                        |> String.uncons
                        |> Maybe.map (first >> String.fromChar)
                        |> Maybe.withDefault ""
                        |> Expect.equal (expected s)
        , fuzz (validWords []) "The tail of the string is lowercased" <|
            \s ->
                humanize s
                    |> String.uncons
                    |> Maybe.map second
                    |> Maybe.withDefault "a"
                    |> String.filter ((/=) ' ')
                    |> String.all Char.isLower
                    |> Expect.true "Not all characters in the string are lowercased"
        , fuzz (validWords [ '_', '-' ]) "It removes a trailing `_id` & replaces underscores and dashes with a single whitespace" <|
            \s ->
                let
                    expected =
                        String.toLower
                            >> Regex.replace (Regex.AtMost 1) (Regex.regex "_id$") (\_ -> "")
                            >> replace "-" " "
                            >> replace "_" " "
                            >> Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> " ")
                            >> String.trim
                in
                    humanize (String.toLower s)
                        |> String.toLower
                        |> Expect.equal (expected s)
        , fuzz string "It yields the same string after removing underscores, dashes and spaces" <|
            \s ->
                let
                    expected =
                        replace "-" ""
                            >> replace "_" ""
                            >> Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> "")
                            >> String.toLower
                in
                    humanize s
                        |> replace " " ""
                        |> String.toLower
                        |> Expect.equal (expected s)
        , fuzz (validWords []) "It adds a space before each uppercase letter" <|
            \s ->
                let
                    expected =
                        replaceUppercase >> String.toLower >> String.trim
                in
                    humanize s
                        |> String.toLower
                        |> Expect.equal (expected s)
        , fuzz string "It does not leave double spaces around" <|
            \s ->
                let
                    expected =
                        replaceUppercase >> String.toLower >> String.trim
                in
                    humanize s
                        |> String.contains "  "
                        |> Expect.false "The string contains double spaces"
        , fuzz idString "It strips the _id at the end" <|
            \s ->
                humanize s
                    |> String.endsWith "id"
                    |> Expect.false "The string should not end with id"
        ]


idString : Fuzzer String
idString =
    validWords [ '-', '_' ]
        |> map (\s -> s ++ "s_id")


latinChars : List (Random.Generator Char)
latinChars =
    [ Random.map Char.fromCode (Random.int 97 122), Random.map Char.fromCode (Random.int 65 90) ]


withChar : List Char -> Random.Generator Char
withChar ch =
    Random.choices <| latinChars ++ (List.map Random.constant ch)


validWords : List Char -> Fuzzer String
validWords ch =
    let
        producer =
            Random.int 1 10 |> Random.andThen (\i -> Random.map String.fromList (Random.list i (withChar ch)))
    in
        custom producer Shrink.noShrink


replaceUppercase : String -> String
replaceUppercase string =
    string
        |> String.toList
        |> List.map
            (\c ->
                if Char.isUpper c then
                    " " ++ (String.fromChar c)
                else
                    String.fromChar c
            )
        |> String.join ""
