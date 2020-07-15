module RemoveAccentsTest exposing (removeAccentsTest)

import Expect
import String.Extra exposing (removeAccents)
import Test exposing (..)


removeAccentsTest : Test
removeAccentsTest =
    describe "removeAccents"
        [ test "Should result string without accents" <|
            \() ->
                removeAccents "ąáàãâäćęéèêëíìîïłóòõôöśúùûüçźżĄÁÀÃÂÄĆĘÉÈÊËÍÌÎÏŁÓÒÕÖÔŚÚÙÛÜÇŹŻ"
                    |> Expect.equal "aaaaaaceeeeeiiiilooooosuuuuczzAAAAAACEEEEEIIIILOOOOOSUUUUCZZ"
        , test "Should result in phrase without accents" <|
            \() ->
                removeAccents "andré JOÂO"
                    |> Expect.equal "andre JOAO"
        , test "Should produce a Polish phrase without accents" <|
            \() ->
                removeAccents "Cześć! Jak się masz? Śmiało usuń akcenty!"
                    |> Expect.equal "Czesc! Jak sie masz? Smialo usun akcenty!"
        ]
