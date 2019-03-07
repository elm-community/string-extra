module TestData exposing
    ( randomString
    , randomStrings
    , randomStringsWithCharGenerators
    , randomStringsWithChars
    )

import Fuzz exposing (Fuzzer)
import Random
import Shrink


withChar : List Char -> Random.Generator Char
withChar ch =
    withCharGenerators <| List.map Random.constant ch


withCharGenerators : List (Random.Generator Char) -> Random.Generator Char
withCharGenerators charGenerators =
    Random.andThen identity <|
        Random.uniform (Random.map Char.fromCode (Random.int 97 122)) <|
            Random.map Char.fromCode (Random.int 65 90)
                :: charGenerators


randomString : Random.Generator String
randomString =
    randomStringWithCharGenerators [ withChar [] ]


randomStringWithCharGenerators : List (Random.Generator Char) -> Random.Generator String
randomStringWithCharGenerators charGenerators =
    Random.int 1 10
        |> Random.andThen (\i -> Random.map String.fromList <| Random.list i (withCharGenerators charGenerators))


randomStrings : Fuzzer String
randomStrings =
    randomStringsWithChars []


randomStringsWithChars : List Char -> Fuzzer String
randomStringsWithChars chars =
    randomStringsWithCharGenerators [ withChar chars ]


randomStringsWithCharGenerators : List (Random.Generator Char) -> Fuzzer String
randomStringsWithCharGenerators charGenerators =
    Fuzz.custom (randomStringWithCharGenerators charGenerators) Shrink.string
