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


makeOutput a b =
    Output a b


params : Parser (List String)
params =
    succeed paramList
        |. symbol "("
        |. maybeWhitespace
        |= variable
        |. maybeWhitespace
        |. symbol ")"


paramList a =
    [ a ]


body : Parser ()
body =
    symbol "{"
        |. maybeWhitespace
        |. symbol "}"


parse : String -> Output
parse input =
    let
        result =
            Debug.log "output:" (run function input)
    in
        case result of
            Ok r ->
                r

            Err _ ->
                { name = "x", params = [] }
