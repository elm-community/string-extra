module String.Extra
    exposing
        ( toSentenceCase
        , toTitleCase
        , replace
        , replaceSlice
        , insertAt
        , break
        , softBreak
        , clean
        , isBlank
        , camelize
        , classify
        )

{-| Additional functions for working with Strings

## Change words casing

@docs toSentenceCase, toTitleCase, camelize, classify

## Replace and Splice

@docs replace, replaceSlice, insertAt, clean

## Splitting

@docs break, softBreak

## Checks

@docs isBlank
-}

import String exposing (uncons, cons, words, join)
import Char exposing (toUpper)
import Regex exposing (regex, escape, HowMany(..))
import Maybe exposing (Maybe(..))
import List


{-| Make a string's first character uppercase

    toSentenceCase "this is a phrase" == "This is a phare"
    toSentenceCase "hello, world" == "Hello, world"

-}
toSentenceCase : String -> String
toSentenceCase word =
    uncons word
        |> Maybe.map (\( head, tail ) -> cons (toUpper head) tail)
        |> Maybe.withDefault ""


{-| Uppercase the first character of each word in a string

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
                (\{ match } -> uppercaseMatch match)


{-| Replace all occurrences of the search string with the substitution string.

    replace "Mary" "Sue" "Hello, Mary" == "Hello, Sue"

-}
replace : String -> String -> String -> String
replace search substitution string =
    string
        |> Regex.replace All (regex (escape search)) (\_ -> substitution)


{-| Replace text within a portion of a string given a substitution
string, a start index and an end index.

    replaceSlice "Sue" 4 6 "Hi, Bob" == "Hi, Sue"
    replaceSlice "elephants" 0  6 "snakes on a plane!" == "elephants on a plane!"
    replaceSlice "under" 7  9 "snakes on a plane!" == "snakes under a plane!"

-}
replaceSlice : String -> Int -> Int -> String -> String
replaceSlice substitution start end string =
    (String.slice 0 start string) ++ substitution ++ (String.slice end (String.length string) string)


{-| Inserts a substring at the specified index.

    insertAt "world" 6 "Hello " === "Hello world"
-}
insertAt : String -> Int -> String -> String
insertAt insert pos string =
    replaceSlice insert pos pos string


{-| Breaks a string into a list of strings of maximum the provided size.

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


{-| Breaks a string into a list of strings of maximum the provided size,
without cutting words at the edge.

    softBreak 6 "The quick brown fox" == ["The quick", " brown", " fox"]

-}
softBreak : Int -> String -> List String
softBreak width string =
    if width <= 0 then
        []
    else
        string
            |> Regex.find All (regex <| ".{1," ++ (toString width) ++ "}(\\s|$)|\\S+?(\\s|$)")
            |> List.map (.match)


{-| Trims the whitespace of both sides of the string and compresses
reapeated whitespace internally to a single whitespace char.

    clean " The   quick brown   fox    " == "The quick brown fox"

-}
clean : String -> String
clean string =
    string
        |> Regex.replace All (regex "\\s\\s+") (always " ")
        |> String.trim


{-| Tests if a string is empty or only contains whitespace

   isBlank "" === True
   isBlank "\n" === True
   isBlank "  " === True
   isBlank " a" === False

-}
isBlank : String -> Bool
isBlank string =
    Regex.contains (regex "^\\s*$") string


{-| Converts underscored or dasherized string to a camelized one.

   camelize "-moz-transform" === "MozTransform"

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


{-| Converts string to camelized string starting with an uppercase.
All non word characters will be stripped out of the original string.

   classify "some_class_name" === "SomeClassName"
   classify "myLittleCamel.class.name" === "MyLittleCamelClassName"

-}
classify : String -> String
classify string =
    string
        |> Regex.replace All (regex "[\\W_]") (always " ")
        |> camelize
        |> replace " " ""
        |> toSentenceCase
