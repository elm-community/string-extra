module Tests exposing (..)

import String.Extra exposing (..)
import String exposing (uncons, fromChar, toUpper, toLower)
import Regex
import Test exposing (..)
import Fuzz exposing (..)
import Expect
--import CamelizeTest exposing (camelizeClaims)
--import UnderscoredTest exposing (underscoredClaims)
--import DasherizeTest exposing (dasherizeClaims)
--import HumanizeTest exposing (humanizeClaims)
--import UnindentTest exposing (unindentClaims)


tail : String -> String
tail =
    uncons >> Maybe.map snd >> Maybe.withDefault ""


toSentenceCaseTest : Test
toSentenceCaseTest =
    describe "toSentenceCase"
        [ fuzz string "It converts the first char of the string to uppercase" <|
            \string ->
                let
                    result = 
                        string
                            |> toSentenceCase
                            |> uncons
                            |> Maybe.map (fst >> fromChar)
                            |> Maybe.withDefault ""

                    expected =
                        string
                            |> uncons
                            |> Maybe.map (fst >> fromChar >> toUpper)
                            |> Maybe.withDefault ""
                in
                    Expect.equal expected result

        , fuzz string "The tail of the string remains untouched" <|
            \string ->
                let
                    result =
                        (toSentenceCase >> tail) string

                    expected =
                        tail string
                in
                    Expect.equal expected result
        ]


decapitalizeTest : Test
decapitalizeTest =
    describe "decapitalize"
        [ fuzz string "It only converst the first char in the string to lowercase" <|
            \string ->
                let
                    result =
                        string
                            |> decapitalize
                            |> uncons
                            |> Maybe.map (fst >> fromChar)
                            |> Maybe.withDefault ""

                    expected =
                        string
                            |> uncons
                            |> Maybe.map (fst >> fromChar >> toLower)
                            |> Maybe.withDefault ""
                in
                    Expect.equal expected result

        , fuzz string "It does not change the tail of the string" <|
            \string ->
                let
                    result =
                        (decapitalize >> tail) string

                    expected =
                        tail string
                in
                    Expect.equal expected result
        ]


toTitleCaseTest : Test
toTitleCaseTest =
    describe "toTitleCase"
        [ fuzz (list string) "It converts the first letter of each word to uppercase" <|
            \strings ->
                let
                    result =
                        strings
                            |> String.join " "
                            |> toTitleCase
                            |> String.words

                    expected =
                        strings
                            |> String.join " "
                            |> String.words
                            |> List.map toSentenceCase
                in
                    Expect.equal expected result

        , fuzz (list string) "It does not change the length of the string" <|
            \strings ->
                let 
                    result =
                        strings
                            |> String.join " "
                            |> toTitleCase
                            |> String.length

                    expected =
                        strings
                            |> String.join " "
                            |> String.length
                in
                    Expect.equal expected result
        ]


replaceTest : Test
replaceTest =
    describe "replace"
        [ fuzz2 string string "It substitutes all occurences of the same sequence" <|
            \string substitute ->
                replace string substitute string
                    |> Expect.equal substitute

        , fuzz string "It substitutes multiple occurances" <|
            \string ->
                replace "a" "b" string
                    |> String.contains "a"
                    |> Expect.false "Given string should not contain any 'a'"

        , test "It should replace special characters" <|
            \_ ->
                replace "\\" "deepthought" "this is a special string \\"
                    |> String.contains "deepthought"
                    |> Expect.true "String should contain deepthought"
        ]


--replaceSliceClaims =
--    suite "replace"
--        [ claim "Result contains the substitution string"
--            `true`
--                (\( string, sub, start, end ) ->
--                    replaceSlice sub start end string |> String.contains sub
--                )
--            `for` replaceSliceProducer
--        , claim "Result string has the length of the substitution + string after removing the slice"
--            `that`
--                (\( string, sub, start, end ) ->
--                    replaceSlice sub start end string |> String.length
--                )
--            `is`
--                (\( string, sub, start, end ) ->
--                    (String.length string - (end - start)) + (String.length sub)
--                )
--            `for` replaceSliceProducer
--        , claim "Start of the original string remains the same"
--            `that`
--                (\( string, sub, start, end ) ->
--                    replaceSlice sub start end string |> String.slice 0 start
--                )
--            `is`
--                (\( string, _, start, _ ) ->
--                    String.slice 0 start string
--                )
--            `for` replaceSliceProducer
--        , claim "End of the original string remains the same"
--            `that`
--                (\( string, sub, start, end ) ->
--                    let
--                        replaced =
--                            replaceSlice sub start end string
--                    in
--                        replaced |> String.slice (start + (String.length sub)) (String.length replaced)
--                )
--            `is`
--                (\( string, _, _, end ) ->
--                    String.slice end (String.length string) string
--                )
--            `for` replaceSliceProducer
--        ]
--
--
--replaceSliceProducer : Producer ( String, String, Int, Int )
--replaceSliceProducer =
--    filter
--        (\( string, sub, start, end ) ->
--            (start < end)
--                && (String.length string >= end)
--                && (not <| String.isEmpty sub)
--        )
--        (tuple4 ( string, string, (rangeInt 0 10), (rangeInt 0 10) ))
--
--
--breakClaims : Claim
--breakClaims =
--    suite "break"
--        [ claim "The list should have as many elements as the ceil division of the length"
--            `that` (\( string, width ) -> break width string |> List.length)
--            `is`
--                (\( string, width ) ->
--                    let
--                        b =
--                            toFloat (String.length string)
--
--                        r =
--                            ceiling (b / (toFloat width))
--                    in
--                        clamp 1 10 r
--                )
--            `for` tuple ( string, (rangeInt 1 10) )
--        , claim "Concatenating the result yields the original string"
--            `that` (\( string, width ) -> break width string |> String.concat)
--            `is` (\( string, _ ) -> string)
--            `for` tuple ( string, (rangeInt 1 10) )
--        , claim "No element in the list should have more than `width` chars"
--            `true`
--                (\( string, width ) ->
--                    break width string
--                        |> List.map (String.length)
--                        |> List.filter ((<) width)
--                        |> List.isEmpty
--                )
--            `for` tuple ( string, (rangeInt 1 10) )
--        ]
--
--
--softBreakClaims : Claim
--softBreakClaims =
--    suite "softBreak"
--        [ claim "Concatenating the result yields the original string"
--            `that` (\( string, width ) -> softBreak width string |> String.concat)
--            `is` (\( string, _ ) -> string)
--            `for` tuple ( string, (rangeInt 1 10) )
--        , claim "The list should not have more elements than words"
--            `true`
--                (\( string, width ) ->
--                    let
--                        broken =
--                            softBreak width string |> List.length
--
--                        words =
--                            String.words string |> List.length
--                    in
--                        broken <= words
--                )
--            `for` tuple ( string, (rangeInt 1 10) )
--        ]
--

cleanTest : Test
cleanTest =
    describe "clean"
        [ fuzz string "The String.split result is the same as String.words" <|
            \string ->
                let 
                    result =
                        string 
                            |> clean 
                            |> String.split " "
                    
                    expected =
                        String.words string
                in
                    Expect.equal expected result

        , fuzz string "It trims the string on the left side" <|
            \string ->
                string
                    |> clean
                    |> String.startsWith " "
                    |> Expect.false "Did not trim the start of the string"

        , fuzz string "It trims the string on the right side" <|
            \string ->
                string
                    |> clean
                    |> String.endsWith " "
                    |> Expect.false "Did not trim the end of the string"
        ]

--insertAtClaims : Claim
--insertAtClaims =
--    suite "insertAt"
--        [ claim "Result contains the substitution string"
--            `true`
--                (\( sub, at, string ) ->
--                    string
--                        |> insertAt sub at
--                        |> String.contains sub
--                )
--            `for` insertAtProducer
--        , claim "Resulting string has length as the sum of both arguments"
--            `that`
--                (\( sub, at, string ) ->
--                    (String.length sub) + (String.length string)
--                )
--            `is`
--                (\( sub, at, string ) ->
--                    insertAt sub at string
--                        |> String.length
--                )
--            `for` insertAtProducer
--        , claim "Start of the string remains the same"
--            `that`
--                (\( sub, at, string ) ->
--                    String.slice 0 at string
--                )
--            `is`
--                (\( sub, at, string ) ->
--                    insertAt sub at string
--                        |> String.slice 0 at
--                )
--            `for` insertAtProducer
--        , claim "End of the string remains the same"
--            `that`
--                (\( sub, at, string ) ->
--                    String.slice at (String.length string) string
--                )
--            `is`
--                (\( sub, at, string ) ->
--                    insertAt sub at string
--                        |> String.slice (at + (String.length sub))
--                            ((String.length string) + String.length sub)
--                )
--            `for` insertAtProducer
--        ]
--
--
--insertAtProducer : Producer ( String, Int, String )
--insertAtProducer =
--    filter
--        (\( sub, at, string ) ->
--            (String.length string >= at)
--                && (not <| String.isEmpty sub)
--        )
--        (tuple3 ( string, (rangeInt 0 10), string ))
--

isBlankTest : Test
isBlankTest =
    describe "isBlank"
        [ test "Returns true if the given string is blank" <|
            \_ ->
                isBlank ""
                    |> Expect.true "Did not return true"

        , test "Returns false if the given string is not blank" <|
            \_ ->
                isBlank " Slartibartfast"
                    |> Expect.false "Did not return false"
        ]


--classifyClaims : Claim
--classifyClaims =
--    suite "classify"
--        [ claim "It does not contain non-word characters"
--            `false` (classify >> Regex.contains (Regex.regex "[\\W]"))
--            `for` string
--        , claim "It starts with an uppercase letter"
--            `that` (classify >> uncons >> Maybe.map fst)
--            `is` (String.trim >> String.toUpper >> uncons >> Maybe.map fst)
--            `for` filter (not << Regex.contains (Regex.regex "[\\W_]")) string
--        , claim "It is camelized once replaced non word charactes with a compatible string"
--            `that` (classify >> uncons >> Maybe.map snd)
--            `is` (replace "." "-" >> camelize >> uncons >> Maybe.map snd)
--            `for` filter (Regex.contains (Regex.regex "^[a-zA-Z\\s\\.\\-\\_]+$")) string
--        ]
--

surroundTest : Test
surroundTest =
    describe "surround"
        [ fuzz2 string string "It starts with the wrapping string" <|
            \string wrap ->
                string
                    |> surround wrap
                    |> String.startsWith wrap
                    |> Expect.true "Did not start with the wrapping string"

        , fuzz2 string string "It ends with the wrapping string" <|
            \string wrap ->
                string
                    |> surround wrap
                    |> String.endsWith wrap
                    |> Expect.true "Did not end with the wrapping string"

        , fuzz2 string string "It contains the original string" <|
            \string wrap ->
                string
                    |> surround wrap
                    |> String.contains string
                    |> Expect.true "Did not contain the string"

        , fuzz2 string string "It does not have anything else inside" <|
            \string wrap ->
                let
                    result =
                        String.length (surround wrap string)

                    expected =
                        (String.length string) + (2 * String.length wrap)
                in
                    Expect.equal expected result
        ]


-- TODO: ensure this test only gets strings that contain the needle in the
-- haystack?
countOccurrencesTest : Test
countOccurrencesTest =
    describe "countOccurrences"
        [ fuzz2 string string "Removing the occurences should yield the right length" <|
            \needle haystack ->
                let
                    times =
                        countOccurrences needle haystack

                    result =
                        String.length haystack - (times * (String.length needle))

                    expected =
                        String.length (replace needle "" haystack)
                in
                    Expect.equal expected result
            ]

--ellipsisClaims : Claim
--ellipsisClaims =
--    suite "ellipsis"
--        [ claim "The resulting string lenght does not exceed the specified length"
--            `true`
--                (\( howLong, string ) ->
--                    ellipsis howLong string
--                        |> String.length
--                        |> (>=) howLong
--                )
--            `for` (tuple ( rangeInt 3 20, string ))
--        , claim "The resulting string contains three dots and the end if necessary"
--            `true`
--                (\( howLong, string ) ->
--                    ellipsis howLong string
--                        |> String.endsWith "..."
--                )
--            `for`
--                filter
--                    (\( howLong, string ) -> String.length string >= howLong + 3)
--                    (tuple ( rangeInt 0 20, string ))
--        , claim "It starts with the left of the original string"
--            `true`
--                (\( howLong, string ) ->
--                    string
--                        |> String.startsWith (ellipsis howLong string |> String.dropRight 3)
--                )
--            `for`
--                filter
--                    (\( howLong, string ) -> String.length string >= howLong + 3)
--                    (tuple ( rangeInt 0 20, string ))
--        , claim "The resulting string does not contain three dots if it is short enough"
--            `false`
--                (\( howLong, string ) ->
--                    ellipsis howLong string
--                        |> String.endsWith "..."
--                )
--            `for`
--                filter
--                    (\( howLong, string ) -> String.length string <= howLong)
--                    (tuple ( rangeInt 0 20, string ))
--        ]

unquoteTest : Test
unquoteTest =
    describe "unquote"
        [ test "Removes quotes from the start of the string" <|
            \_ ->
                unquote "\"Magrathea\""
                    |> Expect.equal "Magrathea"
        ]


wrapTest : Test
wrapTest =
    describe "wrap"
        [ fuzz2 (intRange 1 20) string "Wraps given string at the requested length" <|
            \howLong string ->
                wrap howLong string
                    |> String.split "\n"
                    |> List.map (\str -> String.length str <= howLong)
                    |> List.all ((==) True)
                    |> Expect.true "Given string was not wrapped at requested length"

        , test "Does not wrap string shorter than the requested length" <|
            \_ ->
                wrap 50 "Heart of Gold"
                    |> String.contains "\n"
                    |> Expect.false "Short string was wrapped"
        ]


all : Test
all =
    describe "String.Extra"
        [ toSentenceCaseTest
        , decapitalizeTest
        , toTitleCaseTest
        , replaceTest
        --, replaceSliceClaims
        --, breakClaims
        --, softBreakClaims
        , cleanTest
        --, insertAtClaims
        , isBlankTest
        --, camelizeClaims
        --, classifyClaims
        , surroundTest
        --, underscoredClaims
        --, dasherizeClaims
        --, humanizeClaims
        --, unindentClaims
        , countOccurrencesTest
        --, ellipsisClaims
        , unquoteTest
        , wrapTest
        ]
