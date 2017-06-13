module ReplaceSliceTest exposing (replaceSliceTest)

import String.Extra exposing (..)
import String
import Test exposing (..)
import Fuzz exposing (..)
import Expect


replaceSliceTest : Test
replaceSliceTest =
    describe "replaceSlice"
        [ fuzz replaceSliceProducer "Result contains the substitution string" <|
            \( string, sub, start, end ) ->
                case string of
                    "" ->
                        replaceSlice sub start end string
                            |> Expect.equal sub

                    _ ->
                        replaceSlice sub start end string
                            |> String.contains sub
                            |> Expect.true "The slice was not subtituted"
        , fuzz replaceSliceProducer "Result string has the length of the substitution + string after removing the slice" <|
            \( string, sub, start, end ) ->
                case string of
                    "" ->
                        replaceSlice sub start end string
                            |> String.length
                            |> Expect.equal (String.length sub)

                    _ ->
                        replaceSlice sub start end string
                            |> String.length
                            |> Expect.equal ((String.length string - (end - start)) + (String.length sub))
        , fuzz replaceSliceProducer "Start of the original string remains the same" <|
            \( string, sub, start, end ) ->
                case string of
                    "" ->
                        replaceSlice sub start end string
                            |> Expect.equal sub

                    _ ->
                        replaceSlice sub start end string
                            |> String.slice 0 start
                            |> Expect.equal (String.slice 0 start string)
        , fuzz replaceSliceProducer "End of the original string remains the same" <|
            \( string, sub, start, end ) ->
                let
                    replaced =
                        replaceSlice sub start end string
                in
                    case string of
                        "" ->
                            replaced
                                |> Expect.equal sub

                        _ ->
                            replaced
                                |> String.slice (start + (String.length sub)) (String.length replaced)
                                |> Expect.equal (String.slice end (String.length string) string)
        ]


replaceSliceProducer : Fuzzer ( String, String, Int, Int )
replaceSliceProducer =
    let
        validIntRange i1 i2 =
            if i2 > i1 then
                intRange i1 i2
            else
                constant i1
    in
        string
            |> andThen
                (\s ->
                    tuple3
                        ( constant s
                        , string
                        , validIntRange 0 ((String.length s) - 1)
                        )
                )
            |> andThen
                (\( s1, s2, i ) ->
                    tuple4
                        ( constant s1
                        , constant s2
                        , constant i
                        , validIntRange (i + 1) (String.length s1)
                        )
                )
