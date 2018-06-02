module Language exposing (..)

import Parser exposing (..)
import Parser.LanguageKit exposing (..)
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


functionParser : Parser Output
functionParser =
    succeed makeOutput
        |. maybeWhitespaceParser
        |. keyword "function"
        |. whitespaceParser
        |= variableParser
        |. maybeWhitespaceParser
        |= paramList
        |. maybeWhitespaceParser
        |. body
        |. maybeWhitespaceParser
        |. end


makeOutput : String -> List String -> Output
makeOutput a b =
    Output a b


body : Parser ()
body =
    symbol "{"
        |. maybeWhitespaceParser
        |. symbol "}"


paramList : Parser (List String)
paramList =
    sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = maybeWhitespaceParser
        , item = variableParser
        , trailing = Forbidden
        }


parse : String -> Result Error Output
parse input =
    run functionParser input
