module UnicodeTests exposing (unicodeTests)

import Char
import Expect
import Fuzz exposing (..)
import String
import String.Extra exposing (..)
import Test exposing (..)


bmpCodePointFuzzer : Fuzzer Int
bmpCodePointFuzzer =
    frequency
        [ ( 1, intRange 0 0xD7FF )
        , ( 1, intRange 0xE000 0xFFFF )
        ]


unicodeStringFuzzer : Fuzzer String
unicodeStringFuzzer =
    let
        singletonFuzzer =
            bmpCodePointFuzzer |> map (\codePoint -> [ codePoint ])

        leadingSurrogateFuzzer =
            intRange 0xD800 0xDBFF

        trailingSurrogateFuzzer =
            intRange 0xDC00 0xDFFF

        surrogatePairFuzzer =
            tuple ( leadingSurrogateFuzzer, trailingSurrogateFuzzer )
                |> map (\( leading, trailing ) -> [ leading, trailing ])

        sublistFuzzer =
            frequency
                [ ( 1, singletonFuzzer )
                , ( 1, surrogatePairFuzzer )
                ]
    in
    list sublistFuzzer
        |> map List.concat
        |> map (List.map Char.fromCode)
        |> map String.fromList


codePointFuzzer : Fuzzer Int
codePointFuzzer =
    let
        astralCodePointFuzzer =
            intRange 0x00010000 0x0010FFFF
    in
    frequency
        [ ( 1, bmpCodePointFuzzer )
        , ( 1, astralCodePointFuzzer )
        ]


expectedStringLength : List Int -> Int
expectedStringLength codePoints =
    let
        numCodeUnits codePoint =
            if codePoint <= 0xFFFF then
                1

            else
                2
    in
    codePoints |> List.map numCodeUnits |> List.sum


hardCodedTestCases : List ( String, List Int )
hardCodedTestCases =
    [ ( "", [] )
    , ( "©§π", [ 169, 167, 960 ] )
    , ( "💩!", [ 128169, 33 ] )
    , ( "abc", [ 97, 98, 99 ] )
    ]


unicodeTests : Test
unicodeTests =
    describe "unicode"
        [ fuzz unicodeStringFuzzer "fromCodePoints is inverse of toCodePoints" <|
            \string ->
                fromCodePoints (toCodePoints string)
                    |> Expect.equal string
        , fuzz (list codePointFuzzer) "toCodePoints is inverse of fromCodePoints" <|
            \codePoints ->
                toCodePoints (fromCodePoints codePoints)
                    |> Expect.equal codePoints
        , fuzz (list codePointFuzzer) "string length is greater than or equal to number of code points" <|
            \codePoints ->
                String.length (fromCodePoints codePoints)
                    |> Expect.atLeast (List.length codePoints)
        , fuzz unicodeStringFuzzer "number of code points is less than or equal to string length" <|
            \string ->
                List.length (toCodePoints string)
                    |> Expect.atMost (String.length string)
        , fuzz (list codePointFuzzer) "encoded string length is as expected" <|
            \codePoints ->
                String.length (fromCodePoints codePoints)
                    |> Expect.equal (expectedStringLength codePoints)
        , describe "toCodePoints works as expected on hard-coded test cases"
            (hardCodedTestCases
                |> List.indexedMap
                    (\index ( string, codePoints ) ->
                        test ("toCodePoints works properly - test case " ++ Debug.toString index)
                            (\() -> toCodePoints string |> Expect.equal codePoints)
                    )
            )
        , describe "fromCodePoints works as expected on hard-coded test cases"
            (hardCodedTestCases
                |> List.indexedMap
                    (\index ( string, codePoints ) ->
                        test ("fromCodePoints works properly - test case " ++ Debug.toString index)
                            (\() -> fromCodePoints codePoints |> Expect.equal string)
                    )
            )
        ]
