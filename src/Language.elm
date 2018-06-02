module Language exposing (..)

import Parser exposing (..)
import Char


type alias Output =
    { name : String, params : List String }


maybeWhitespace : Parser ()
maybeWhitespace =
    ignore zeroOrMore (\c -> (c == ' ') || (c == '\n'))


whitespace : Parser ()
whitespace =
    ignore oneOrMore (\c -> (c == ' ') || (c == '\n'))


variable : Parser String
variable =
    succeed identity
        |= keep oneOrMore Char.isLower


function : Parser Output
function =
    succeed makeOutput
        |. maybeWhitespace
        |. keyword "function"
        |. whitespace
        |= variable
        |. maybeWhitespace
        |= params
        |. maybeWhitespace
        |. body
        |. maybeWhitespace
        |. end


makeOutput : String -> List String -> Output
makeOutput a b =
    Output a b


params : Parser (List String)
params =
    succeed identity
        |. symbol "("
        -- FIXME This seems like a hack
        |. maybeWhitespace
        |= andThen (\n -> variableList [ n ]) variable
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
    delayedCommit maybeWhitespace <|
        succeed identity
            |. symbol ","
            |. maybeWhitespace
            |= variable
            |. maybeWhitespace


paramList : a -> List a
paramList a =
    [ a ]


body : Parser ()
body =
    symbol "{"
        |. maybeWhitespace
        |. symbol "}"


parse : String -> Result Error Output
parse input =
    run function input
