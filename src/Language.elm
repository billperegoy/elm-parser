module Language exposing (..)

import Parser exposing (..)
import Char


type alias Output =
    { name : String, params : List String }


maybeWhitespaceParser : Parser ()
maybeWhitespaceParser =
    ignore zeroOrMore (\c -> (c == ' ') || (c == '\n'))


whitespaceParser : Parser ()
whitespaceParser =
    ignore oneOrMore (\c -> (c == ' ') || (c == '\n'))


variableParser : Parser String
variableParser =
    succeed identity
        |= keep oneOrMore Char.isLower


function : Parser Output
function =
    succeed makeOutput
        |. maybeWhitespaceParser
        |. keyword "function"
        |. whitespaceParser
        |= variableParser
        |. maybeWhitespaceParser
        |= paramsParser
        |. maybeWhitespaceParser
        |. body
        |. maybeWhitespaceParser
        |. end


makeOutput : String -> List String -> Output
makeOutput a b =
    Output a b


paramsParser : Parser (List String)
paramsParser =
    succeed identity
        |. symbol "("
        -- FIXME This seems like a hack
        |. maybeWhitespaceParser
        |= andThen (\n -> variableList [ n ]) variableParser
        |. symbol ")"


variableList : List String -> Parser (List String)
variableList elems =
    oneOf
        [ nextVariable
            |> andThen (\n -> variableList (n :: elems))
        , succeed (List.reverse elems)
        ]


nextVariable : Parser String
nextVariable =
    delayedCommit maybeWhitespaceParser <|
        succeed identity
            |. symbol ","
            |. maybeWhitespaceParser
            |= variableParser
            |. maybeWhitespaceParser


paramList : a -> List a
paramList a =
    [ a ]


body : Parser ()
body =
    symbol "{"
        |. maybeWhitespaceParser
        |. symbol "}"


parse : String -> Result Error Output
parse input =
    run function input
