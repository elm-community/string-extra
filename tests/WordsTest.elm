module WordsTest exposing (allTests)

import Expect
import Fuzz exposing (Fuzzer, list, string)
import List exposing (foldl)
import String.Extra exposing (isBlank)
import Test exposing (Test, fuzz)


fuzzMultilineText : Fuzzer String
fuzzMultilineText =
    let
        -- Is there a system-independent value for newline?
        newlineCharacter =
            "\n"

        appendLine text followingLine =
            text ++ followingLine ++ newlineCharacter
    in
    list string
        |> Fuzz.map (foldl appendLine "")


allTests : Test
allTests =
    fuzz fuzzMultilineText "match String.words when not blank; return [] otherwise" <|
        \text ->
            let
                -- I don't know how to write a fuzzer that generates
                -- only non-blank collections of lines, so I combined the two tests,
                -- which I otherwise dislike doing.
                expected =
                    if isBlank text then
                        []

                    else
                        String.words text
            in
            Expect.equal
                expected
                (String.Extra.words text)
