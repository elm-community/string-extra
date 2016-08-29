module UnicodeTest exposing (unicodeClaims)

import String.Extra exposing (..)
import String
import Char
import Check exposing (Claim, suite, claim, that, is, for, true)
import Check.Producer exposing (Producer, string, rangeInt, filter, tuple, tuple3, bool, list, map)


bmpCodePointProducer : Producer Int
bmpCodePointProducer =
    rangeInt 0 0xFFFF
        |> filter (\value -> value <= 0xD7FF || value >= 0xE000)


unicodeStringProducer : Producer String
unicodeStringProducer =
    let
        leadingSurrogateProducer =
            rangeInt 0xD800 0xDBFF

        trailingSurrogateProducer =
            rangeInt 0xDC00 0xDFFF

        surrogatePairProducer =
            tuple ( leadingSurrogateProducer, trailingSurrogateProducer )

        sublistProducer =
            tuple3 ( bmpCodePointProducer, surrogatePairProducer, bool )
                |> map
                    (\( bmpCodePoint, surrogatePair, flag ) ->
                        if flag then
                            [ bmpCodePoint ]
                        else
                            let
                                ( leadingSurrogate, trailingSurrogate ) =
                                    surrogatePair
                            in
                                [ leadingSurrogate, trailingSurrogate ]
                    )
    in
        list sublistProducer
            |> map List.concat
            |> map (List.map Char.fromCode)
            |> map String.fromList


codePointProducer : Producer Int
codePointProducer =
    let
        astralCodePointProducer =
            rangeInt 0x00010000 0x0010FFFF
    in
        tuple3 ( bmpCodePointProducer, astralCodePointProducer, bool )
            |> map
                (\( bmpCodePoint, astralCodePoint, flag ) ->
                    if flag then
                        bmpCodePoint
                    else
                        astralCodePoint
                )


expectedStringLength : List Int -> Int
expectedStringLength codePoints =
    codePoints
        |> List.map
            (\codePoint ->
                if codePoint <= 0xFFFF then
                    1
                else
                    2
            )
        |> List.sum


hardCodedTestCases : Producer ( String, List Int )
hardCodedTestCases =
    rangeInt 0 3
        |> map
            (\index ->
                case index of
                    1 ->
                        ( "abc", [ 97, 98, 99 ] )

                    2 ->
                        ( "©§π", [ 169, 167, 960 ] )

                    3 ->
                        ( "💩!", [ 128169, 33 ] )

                    _ ->
                        ( "", [] )
            )


unicodeClaims : Claim
unicodeClaims =
    suite "unicode"
        [ claim "fromCodePoints is inverse of toCodePoints"
            `that` (toCodePoints >> fromCodePoints)
            `is` identity
            `for` unicodeStringProducer
        , claim "toCodePoints is inverse of fromCodePoints"
            `that` (fromCodePoints >> toCodePoints)
            `is` identity
            `for` (list codePointProducer)
        , claim "string length is greater than or equal to number of code points"
            `true` (\codePoints ->
                        String.length (fromCodePoints codePoints)
                            >= List.length codePoints
                   )
            `for` (list codePointProducer)
        , claim "number of code points is less than or equal to string length"
            `true` (\string ->
                        List.length (toCodePoints string)
                            <= String.length string
                   )
            `for` unicodeStringProducer
        , claim "encoded string length is as expected"
            `that` (fromCodePoints >> String.length)
            `is` expectedStringLength
            `for` (list codePointProducer)
        , claim "toCodePoints works as expected on hard-coded test cases"
            `that` (fst >> toCodePoints)
            `is` snd
            `for` hardCodedTestCases
        , claim "fromCodePoints works as expected on hard-coded test cases"
            `that` (snd >> fromCodePoints)
            `is` fst
            `for` hardCodedTestCases
        ]
