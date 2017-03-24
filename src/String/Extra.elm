module String.Extra
    exposing
        ( toSentenceCase
        , toTitleCase
        , decapitalize
        , replace
        , replaceSlice
        , insertAt
        , break
        , softBreak
        , clean
        , isBlank
        , camelize
        , underscored
        , dasherize
        , classify
        , humanize
        , quote
        , unquote
        , surround
        , unsurround
        , wrap
        , wrapWith
        , softWrap
        , softWrapWith
        , unindent
        , countOccurrences
        , ellipsis
        , softEllipsis
        , ellipsisWith
        , toSentence
        , toSentenceOxford
        , stripTags
        , rightOf
        , leftOf
        , rightOfBack
        , leftOfBack
        , fromInt
        , fromFloat
        , toCodePoints
        , fromCodePoints
        , pluralize
        , nonEmpty
        )

{-| Additional functions for working with Strings

## Change words casing

@docs toSentenceCase, toTitleCase, decapitalize

## Inflector functions

Functions borrowed from the Rails Inflector class

@docs camelize, classify, underscored, dasherize, humanize

## Replace and Splice

@docs replace, replaceSlice, insertAt, clean, nonEmpty

## Splitting

@docs break, softBreak

## Wrapping

@docs wrap, wrapWith, softWrap, softWrapWith, quote, surround

## Checks

@docs isBlank, countOccurrences

## Formatting

@docs clean, unquote, unsurround, unindent, ellipsis, softEllipsis, ellipsisWith, stripTags, pluralize

## Converting Lists

@docs toSentence, toSentenceOxford

## Finding

@docs rightOf, leftOf, rightOfBack, leftOfBack

## Converting Numbers

@docs fromInt, fromFloat

## Converting UTF-32

@docs toCodePoints, fromCodePoints

-}

import String exposing (uncons, cons, words, join)
import Char exposing (toUpper, toLower)
import Regex exposing (regex, escape, HowMany(..))
import Maybe exposing (Maybe(..))
import List
import Bitwise


{-| Change the case of the first letter of a string to either uppercase or
    lowercase, depending of the value of `wantedCase`. This is an internal
    function for use in `toSentenceCase` and `decapitalize`.

-}
changeCase : (Char -> Char) -> String -> String
changeCase mutator word =
    uncons word
        |> Maybe.map (\( head, tail ) -> (cons (mutator head) tail))
        |> Maybe.withDefault ""


{-| Capitalize the first letter of a string.

    toSentenceCase "this is a phrase" == "This is a phrase"
    toSentenceCase "hello, world" == "Hello, world"

-}
toSentenceCase : String -> String
toSentenceCase word =
    changeCase (toUpper) word


{-| Downcase the first letter of a string.

    decapitalize "This is a phrase" == "this is a phrase"
    decapitalize "Hello, World" == "hello, World"

-}
decapitalize : String -> String
decapitalize word =
    changeCase (toLower) word


{-| Capitalize the first character of each word in a string.

    toTitleCase "this is a phrase" == "This Is A Phrase"
    toTitleCase "hello, world" == "Hello, World"

-}
toTitleCase : String -> String
toTitleCase ws =
    let
        uppercaseMatch =
            Regex.replace All (regex "\\w+") (.match >> toSentenceCase)
    in
        ws
            |> Regex.replace All
                (regex "^([a-z])|\\s+([a-z])")
                (.match >> uppercaseMatch)


{-| Replace all occurrences of the search string with the substitution string.

    replace "Mary" "Sue" "Hello, Mary" == "Hello, Sue"

-}
replace : String -> String -> String -> String
replace search substitution string =
    string
        |> Regex.replace All (regex (escape search)) (\_ -> substitution)


{-| Replace text within a portion of a string given a substitution
string, a start index and an end index. The substitution includes the character
at the start index but not the one at the end index.

    replaceSlice "Sue" 4 7 "Hi, Bob" == "Hi, Sue"
    replaceSlice "elephants" 0  6 "snakes on a plane!" == "elephants on a plane!"
    replaceSlice "under" 7  9 "snakes on a plane!" == "snakes under a plane!"

-}
replaceSlice : String -> Int -> Int -> String -> String
replaceSlice substitution start end string =
    (String.slice 0 start string) ++ substitution ++ (String.slice end (String.length string) string)


{-| Insert a substring at the specified index.

    insertAt "world" 6 "Hello " == "Hello world"
-}
insertAt : String -> Int -> String -> String
insertAt insert pos string =
    replaceSlice insert pos pos string


{-| Break a string into a list of strings of a specified maximum length.

    break 10 "The quick brown fox" == ["The quick ", "brown fox"]
    break 2 "" == [""]

-}
break : Int -> String -> List String
break width string =
    if width == 0 || string == "" then
        [ string ]
    else
        breaker width string []


breaker : Int -> String -> List String -> List String
breaker width string acc =
    case string of
        "" ->
            List.reverse acc

        _ ->
            breaker width
                (String.dropLeft width string)
                ((String.slice 0 width string) :: acc)


{-| Break a string into a list of strings of a specified maximum length,
without truncating words.

    softBreak 6 "The quick brown fox" == ["The quick", " brown", " fox"]

-}
softBreak : Int -> String -> List String
softBreak width string =
    if width <= 0 then
        []
    else
        string
            |> Regex.find All (softBreakRegexp width)
            |> List.map (.match)


softBreakRegexp : Int -> Regex.Regex
softBreakRegexp width =
    regex <| ".{1," ++ (toString width) ++ "}(\\s+|$)|\\S+?(\\s+|$)"


{-| Trim the whitespace of both sides of the string and compress
repeated whitespace internally to a single whitespace char.

    clean " The   quick brown   fox    " == "The quick brown fox"

-}
clean : String -> String
clean string =
    string
        |> Regex.replace All (regex "\\s\\s+") (always " ")
        |> String.trim


{-| Test if a string is empty or only contains whitespace.

    isBlank "" == True
    isBlank "\n" == True
    isBlank "  " == True
    isBlank " a" == False

-}
isBlank : String -> Bool
isBlank string =
    Regex.contains (regex "^\\s*$") string


{-| Convert an underscored or dasherized string to a camelized one.

    camelize "-moz-transform" == "MozTransform"

-}
camelize : String -> String
camelize string =
    Regex.replace All
        (regex "[-_\\s]+(.)?")
        (\{ submatches } ->
            case submatches of
                (Just match) :: _ ->
                    String.toUpper match

                _ ->
                    ""
        )
        (String.trim string)


{-| Convert a string to a camelized string starting with an uppercase letter.
All non-word characters will be stripped out of the original string.

    classify "some_class_name" == "SomeClassName"
    classify "myLittleCamel.class.name" == "MyLittleCamelClassName"

-}
classify : String -> String
classify string =
    string
        |> Regex.replace All (regex "[\\W_]") (always " ")
        |> camelize
        |> replace " " ""
        |> toSentenceCase


{-| Surround a string with another string.

    surround "bar" "foo" == "barfoobar"

-}
surround : String -> String -> String
surround wrap string =
    wrap ++ string ++ wrap


{-| Remove surrounding strings from another string.

    unsurround "foo" "foobarfoo" == "bar"

-}
unsurround : String -> String -> String
unsurround wrap string =
    if String.startsWith wrap string && String.endsWith wrap string then
        let
            length =
                String.length wrap
        in
            string
                |> String.dropLeft length
                |> String.dropRight length
    else
        string


{-| Add quotes to a string.

    quote "foo" == "\"foo\""

-}
quote : String -> String
quote string =
    surround "\"" string


{-| Remove quotes that surround a string.

    unquote "\"foo\"" == "foo"
    unquote "\"foo\"bar\""

-}
unquote : String -> String
unquote string =
    unsurround "\"" string


{-| Return a string joined by underscores after separating it by its uppercase characters.
Any sequence of spaces or dashes will also be converted to a single underscore.
The final string will be lowercased.

    underscore "SomeClassName" == "some_class_name"
    underscore "some-class-name" == "some_class_name"
    underscore "SomeClass name" == "some_class_name

-}
underscored : String -> String
underscored string =
    string
        |> String.trim
        |> Regex.replace All (regex "([a-z\\d])([A-Z]+)") (.submatches >> List.filterMap identity >> String.join "_")
        |> Regex.replace All (regex "[_-\\s]+") (always "_")
        |> String.toLower


{-| Return a string joined by dashes after separating it by its uppercase characters.
Any sequence of spaces or underscores will also be converted to a single dash.
The final string will be lowercased.

    dasherize "SomeClassName" == "-some-class-name"
    dasherize "some_class_name" = "some-class-name"
    dasherize "someClass name" = "some-class-name"

-}
dasherize : String -> String
dasherize string =
    string
        |> String.trim
        |> Regex.replace All (regex "([A-Z])") (.match >> String.append "-")
        |> Regex.replace All (regex "[_-\\s]+") (always "-")
        |> String.toLower


{-| Separate a string into parts of a given width, using a given separator.

Look at `wrap` if you just want to wrap using newlines.

    wrapWith 7 "\n" "My very long text" === "My very\nlong text"
    wrapWith 100 "\n" "Too short" === "Too short"

-}
wrapWith : Int -> String -> String -> String
wrapWith width separator string =
    string
        |> break width
        |> String.join separator


{-| Chop a given string into parts of a given width, separating them with a
new line.

    wrap 7 "My very long text" === "My very\nlong te\nxt"
    wrap 100 "Too short" === "Too short"

-}
wrap : Int -> String -> String
wrap width string =
    wrapWith width "\n" string


{-| Chop a given string into parts of a given width without breaking words apart,
and then separate them using a new line.

    softWrap 7 "My very long text" === "My very\nlong text"
    softWrap 3 "Hello World" === "Hello \nWorld"
    softWrap 100 "Too short" === "Too short"

-}
softWrap : Int -> String -> String
softWrap width string =
    softWrapWith width "\n" string


{-| Chop a given string into parts of a given width without breaking words apart,
and then separate them using the given separator.

    softWrapWith 7 "..." "My very long text" === "My very...long text"
    softWrapWith 3 "\n" "Hello World" === "Hello \nWorld"
    softWrapWith 100 "\t" "Too short" === "Too short"

-}
softWrapWith : Int -> String -> String -> String
softWrapWith width separator string =
    string
        |> softBreak width
        |> String.join separator


{-| Convert an underscored, camelized, or dasherized string into one that can be
read by humans. Also remove beginning and ending whitespace, and removes the
postfix '_id'. The first character will be capitalized.

    humanize "this_is_great" == "This is great"
    humanize "ThisIsGreat" = "This is great"
    humanize "this-is-great" = "This is great"
    humanize "author_id" = "Author"

-}
humanize : String -> String
humanize string =
    string
        |> Regex.replace All (regex "[A-Z]") (.match >> String.append "-")
        |> Regex.replace All (regex "_id$|[-_\\s]+") (always " ")
        |> String.trim
        |> String.toLower
        |> toSentenceCase


{-| Remove the shortest sequence of leading spaces or tabs on each line
of the string, so that at least one of the lines will not have any
leading spaces nor tabs and the rest of the lines will have the same
amount of indentation removed.

    unindent "  Hello\n    World " == "Hello\n  World"
    unindent "\t\tHello\n\t\t\t\tWorld" == "Hello\n\t\tWorld"

-}
unindent : String -> String
unindent multilineSting =
    let
        lines =
            String.lines multilineSting

        countLeadingWhitespace count line =
            case String.uncons line of
                Nothing ->
                    count

                Just ( char, rest ) ->
                    case char of
                        ' ' ->
                            countLeadingWhitespace (count + 1) rest

                        '\t' ->
                            countLeadingWhitespace (count + 1) rest

                        _ ->
                            count

        isNotWhitespace char =
            char /= ' ' && char /= '\t'

        minLead =
            lines
                |> List.filter (String.any isNotWhitespace)
                |> List.map (countLeadingWhitespace 0)
                |> List.minimum
                |> Maybe.withDefault 0
    in
        lines
            |> List.map (String.dropLeft minLead)
            |> String.join "\n"


{-| Return the number of occurrences of a substring in another string.

    countOccurrences "Hello" "Hello World" == 1
    countOccurrences "o" "Hello World" == 2
-}
countOccurrences : String -> String -> Int
countOccurrences needle haystack =
    if (String.length needle) == 0 || (String.length haystack) == 0 then
        0
    else
        haystack
            |> String.indexes needle
            |> List.length


{-| Truncate the second string at the specified length if the string is
longer than the specified length, and replace the end of the truncated
string with the first string, such that the resulting string is of the
specified length.

The resulting string will have at most the specified length.

    ellipsisWith 5 " .." "Hello World" == "He .."
    ellipsisWith 10 " .."  "Hello World" == "Hello W .."
    ellipsisWith 10 " .." "Hello" == "Hello"
    ellipsisWith 8 " .." "Hello World" == "Hello .."

-}
ellipsisWith : Int -> String -> String -> String
ellipsisWith howLong append string =
    if String.length string <= howLong then
        string
    else
        (String.left (howLong - (String.length append)) string) ++ append


{-| Truncate the string at the specified length if the string is
longer than the specified length, and replace the end of the truncated
string with `"..."`, such that the resulting string is of the
specified length.

The resulting string will have at most the specified length.

    ellipsis 5 "Hello World" == "He..."
    ellipsis 10 "Hello World" == "Hello W..."
    ellipsis 10 "Hello" == "Hello"
    ellipsis 8 "Hello World" == "Hello..."

-}
ellipsis : Int -> String -> String
ellipsis howLong string =
    ellipsisWith howLong "..." string


{-| Truncate the string at the last complete word less than or equal to
the specified length and append `"..."`. When the specified length is
less than the length of the first word, the ellipsis is appended to the
first word. When the specified length is greater than or equal to the
length of the string, an identical string is returned.

In contrast to `ellipsis`, this function will not produce incomplete
words, and the resulting string can exceed the specified length. In
addition, it removes trailing whitespace and punctuation characters at
the end of the truncated string.

    softEllipsis 1 "Hello, World" == "Hello..."
    softEllipsis 5 "Hello, World" == "Hello..."
    softEllipsis 6 "Hello, World" == "Hello..."
    softEllipsis 15 "Hello, cruel world" == "Hello, cruel..."
    softEllipsis 10 "Hello" == "Hello"

-}
softEllipsis : Int -> String -> String
softEllipsis howLong string =
    if String.length string <= howLong then
        string
    else
        string
            |> Regex.find (AtMost 1) (softBreakRegexp howLong)
            |> List.map .match
            |> String.join ""
            |> Regex.replace All (regex "([\\.,;:\\s])+$") (always "")
            |> flip String.append "..."


{-| Convert a list of strings into a human-readable list.

    toSentence [] == ""
    toSentence ["lions"] == "lions"
    toSentence ["lions", "tigers"] == "lions and tigers"
    toSentence ["lions", "tigers", "bears"] == "lions, tigers and bears"

-}
toSentence : List String -> String
toSentence list =
    case list of
        x :: y :: z :: more ->
            toSentenceHelper " and " (x ++ ", " ++ y) (z :: more)

        _ ->
            toSentenceBaseCase list


{-| Convert a list of strings into a human-readable list using an oxford comma.

    toSentenceOxford [] == ""
    toSentenceOxford ["lions"] == "lions"
    toSentenceOxford ["lions", "tigers"] == "lions and tigers"
    toSentenceOxford ["lions", "tigers", "bears"] == "lions, tigers, and bears"

-}
toSentenceOxford : List String -> String
toSentenceOxford list =
    case list of
        x :: y :: z :: more ->
            toSentenceHelper ", and " (x ++ ", " ++ y) (z :: more)

        _ ->
            toSentenceBaseCase list


toSentenceBaseCase : List String -> String
toSentenceBaseCase list =
    case list of
        x :: [] ->
            x

        x :: y :: [] ->
            x ++ " and " ++ y

        _ ->
            ""


toSentenceHelper : String -> String -> List String -> String
toSentenceHelper lastPart sentence list =
    case list of
        [] ->
            sentence

        x :: [] ->
            sentence ++ lastPart ++ x

        x :: xs ->
            toSentenceHelper lastPart (sentence ++ ", " ++ x) xs


{-| Remove all HTML tags from the string, preserving the text inside them.

    stripTags "a <a href=\"#\">link</a>" == "a link"
    stripTags "<script>alert('hello world!')</script> == "alert('hello world!')"

-}
stripTags : String -> String
stripTags string =
    string
        |> Regex.replace All (regex "<\\/?[^>]+>") (always "")


{-| Given a number, a singular string, and a plural string, return the number
followed by a space, followed by either the singular string if the number was 1,
or the plural string otherwise.

    pluralize "elf" "elves" 2 == "2 elves"
    pluralize "elf" "elves" 1 == "1 elf"
    pluralize "elf" "elves" 0 == "0 elves"
-}
pluralize : String -> String -> number -> String
pluralize singular plural count =
    if count == 1 then
        "1 " ++ singular
    else
        (toString count) ++ " " ++ plural


{-| Search a string from left to right for a pattern and return a substring
consisting of the characters in the string that are to the right of the pattern.

    rightOf "_" "This_is_a_test_string" == "is_a_test_string"
-}
rightOf : String -> String -> String
rightOf pattern string =
    string
        |> Regex.find (AtMost 1) (regex <| (escape pattern) ++ "(.*)$")
        |> List.map (.submatches >> firstResult)
        |> String.join ""


{-| Search a string from left to right for a pattern and return a substring
consisting of the characters in the string that are to the left of the pattern.

    leftOf "_" "This_is_a_test_string" == "This"
-}
leftOf : String -> String -> String
leftOf pattern string =
    string
        |> Regex.find (AtMost 1) (regex <| "^(.*?)" ++ (escape pattern))
        |> List.map (.submatches >> firstResult)
        |> String.join ""


firstResult : List (Maybe String) -> String
firstResult list =
    firstResultHelp "" list


firstResultHelp : String -> List (Maybe String) -> String
firstResultHelp default list =
    case list of
        [] ->
            default

        (Just a) :: _ ->
            a

        Nothing :: rest ->
            firstResultHelp default rest


{-| Search a string from right to left for a pattern and return a substring
consisting of the characters in the string that are to the right of the pattern.

    rightOfBack "_" "This_is_a_test_string" == "string"
-}
rightOfBack : String -> String -> String
rightOfBack pattern string =
    string
        |> String.indexes pattern
        |> List.reverse
        |> List.head
        |> Maybe.map ((+) (String.length pattern) >> flip String.dropLeft string)
        |> Maybe.withDefault ""


{-| Search a string from right to left for a pattern and return a substring
consisting of the characters in the string that are to the left of the pattern.

    leftOfBack "_" "This_is_a_test_string" == "This_is_a_test"
-}
leftOfBack : String -> String -> String
leftOfBack pattern string =
    string
        |> String.indexes pattern
        |> List.reverse
        |> List.head
        |> Maybe.map (flip String.left string)
        |> Maybe.withDefault ""


{-| Turn an integer into a string.

This works the same way as `Basics.toString` except its type is restricted,
so if you accidentally pass it something other than an `Int`, you get an error.
-}
fromInt : Int -> String
fromInt =
    toString


{-| Turn a float-point number into a string.

This works the same way as `Basics.toString` except its type is restricted,
so if you accidentally pass it something other than a `Float`, you get an error.
-}
fromFloat : Float -> String
fromFloat =
    toString


{-| Code point of Unicode character to use to indicate an 'unknown or
unrepresentable character'
(https://en.wikipedia.org/wiki/Specials_(Unicode_block))
-}
replacementCodePoint : Int
replacementCodePoint =
    0xFFFD


{-| Convert a string into a list of UTF-32 code points.

    toCodePoints "abc" == [ 97, 98, 99 ]
    toCodePoints "©§π" == [ 169, 167, 960 ]

If every character in the string can be represented by a single UTF-16 code
unit, `toCodePoints` is equivalent to:

    String.toList >> List.map Char.toCode

However, for characters that do not fit into a single UTF-16 code unit and
have to be represented by a surrogate pair, the above will return each code
unit in the surrogate pair separately:

    -- 💩 is U+1F4A9 PILE OF POO
    List.map Char.toCode (String.toList "💩!") == [ 55357, 56489, 33 ]

`toCodePoints` detects and combines surrogate pairs of code units to return a
list of valid UTF-32 code points:

    toCodePoints "💩!" == [ 128169, 33 ]

Note that this still does not necessarily correspond to logical/visual
characters, since it is possible for things like accented characters to be
represented as two separate UTF-32 code points (a base character and a
combining accent).
-}
toCodePoints : String -> List Int
toCodePoints string =
    let
        -- Convert a list of UTF-16 code units to a reversed list of UTF-32 code
        -- points (merging surrogate pairs into single code points where
        -- necessary)
        combineAndReverse codeUnits accumulated =
            case codeUnits of
                [] ->
                    accumulated

                first :: afterFirst ->
                    -- We have at least one code unit - might be a code point
                    -- itself, or the leading code unit of a surrogate pair
                    if first >= 0 && first <= 0xD7FF then
                        -- First code unit is in BMP (and is therefore a valid
                        -- UTF-32 code point), use it as is and continue with
                        -- remaining code units
                        combineAndReverse afterFirst (first :: accumulated)
                    else if first >= 0xD800 && first <= 0xDBFF then
                        -- First code unit is a leading surrogate
                        case afterFirst of
                            [] ->
                                -- Should never happen - leading surrogate with
                                -- no following code unit, replace it with the
                                -- replacement character
                                replacementCodePoint :: accumulated

                            second :: afterSecond ->
                                -- Good, there is a following code unit (which
                                -- should be a trailing surrogate)
                                if second >= 0xDC00 && second <= 0xDFFF then
                                    -- Second code unit is a valid trailing
                                    -- surrogate
                                    let
                                        -- Reconstruct UTF-32 code point from
                                        -- surrogate pair
                                        codePoint =
                                            0x00010000
                                                + ((first - 0xD800) * 1024)
                                                + (second - 0xDC00)
                                    in
                                        -- Continue with following code units
                                        combineAndReverse afterSecond
                                            (codePoint :: accumulated)
                                else
                                    -- Should never happen - second code unit
                                    -- is not a valid trailing surrogate,
                                    -- replace the leading surrogate with the
                                    -- replacement character and continue with
                                    -- remaining code units (perhaps the
                                    -- second code unit is a valid leading
                                    -- surrogate or standalone character, so
                                    -- don't skip it)
                                    combineAndReverse afterFirst
                                        (replacementCodePoint :: accumulated)
                    else if first >= 0xE000 && first <= 0xFFFF then
                        -- First code unit is in BMP (and is therefore a valid
                        -- UTF-32 code point), use it as is and continue with
                        -- remaining code units
                        combineAndReverse afterFirst (first :: accumulated)
                    else
                        -- Should never happen - first code unit is invalid,
                        -- replace it with the replacement character and
                        -- continue with remaining code units
                        combineAndReverse afterFirst
                            (replacementCodePoint :: accumulated)

        allCodeUnits =
            List.map Char.toCode (String.toList string)
    in
        List.reverse (combineAndReverse allCodeUnits [])


{-| Convert a list of UTF-32 code points into a string. Inverse of
`toCodePoints`.

    fromCodePoints [ 97, 98, 99 ] == "abc"
    fromCodePoints [ 169, 167, 960 ] == "©§π"

If every code point is a valid UTF-16 code unit, `fromCodePoints` is equivalent
to:

    List.map Char.fromCode >> String.fromList

However, `fromCodePoints` additionally splits code points that do not fit in a
single UTF-16 code unit into surrogate pairs, so that even code points outside
the Basic Multilingual Plane (BMP) can be included in the resulting string:

    -- Code point 128169 is 💩, U+1F4A9 PILE OF POO
    fromCodePoints [ 128169, 33 ] == "💩!"
-}
fromCodePoints : List Int -> String
fromCodePoints allCodePoints =
    let
        -- Convert a list of UTF-32 code points into a reversed list of UTF-16
        -- code units (splitting single code points into surrogate pairs where
        -- necessary)
        splitAndReverse codePoints accumulated =
            case codePoints of
                [] ->
                    accumulated

                codePoint :: rest ->
                    if codePoint >= 0 && codePoint <= 0xD7FF then
                        -- Code point is valid UTF-16 code unit, use it as is
                        -- and continue with remaining code points
                        splitAndReverse rest (codePoint :: accumulated)
                    else if codePoint >= 0x00010000 && codePoint <= 0x0010FFFF then
                        -- Code point must be split into a surrogate pair of
                        -- UTF-16 code units
                        let
                            subtracted =
                                codePoint - 0x00010000

                            leading =
                                (Bitwise.shiftRightBy 10 subtracted) + 0xD800

                            trailing =
                                (Bitwise.and subtracted 1023) + 0xDC00
                        in
                            splitAndReverse rest
                                (trailing :: leading :: accumulated)
                    else if codePoint >= 0xE000 && codePoint <= 0xFFFF then
                        -- Code point is valid UTF-16 code unit, use it as is
                        -- and continue with remaining code points
                        splitAndReverse rest (codePoint :: accumulated)
                    else
                        -- Should never happen - invalid code point, replace
                        -- it with the replacement character and continue with
                        -- remaining code points
                        splitAndReverse rest
                            (replacementCodePoint :: accumulated)

        allCodeUnits =
            List.reverse (splitAndReverse allCodePoints [])
    in
        String.fromList (List.map Char.fromCode allCodeUnits)


{-| Convert a string to a Nothing when empty.

    nonEmpty "" == Nothing
    nonEmpty "Hello world" == Just "Hello world"
-}
nonEmpty : String -> Maybe String
nonEmpty string =
    if String.isEmpty string then
        Nothing
    else
        Just string
