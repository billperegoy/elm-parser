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
    succeed paramList
        |. symbol "("
        |. maybeWhitespace
        |= variable
        |. maybeWhitespace
        |. symbol ")"


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
