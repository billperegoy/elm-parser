module Language exposing (..)

import Parser
    exposing
        ( run
        , ignore
        , end
        , keyword
        , succeed
        , oneOrMore
        , zeroOrMore
        , keep
        , symbol
        , (|.)
        , (|=)
        , Parser
        , Error
        )
import Parser.LanguageKit
    exposing
        ( sequence
        , Trailing(..)
        )
import Char


type alias Output =
    { name : String
    , params : List String
    }


maybeWhitespaceParser : Parser ()
maybeWhitespaceParser =
    ignore zeroOrMore whitespaceChar


whitespaceParser : Parser ()
whitespaceParser =
    ignore oneOrMore whitespaceChar


whitespaceChar : Char -> Bool
whitespaceChar char =
    List.member char [ ' ', '\n', '\t' ]


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
        |= paramListParser
        |. maybeWhitespaceParser
        |. body
        |. maybeWhitespaceParser
        |. end


makeOutput : String -> List String -> Output
makeOutput name params =
    { name = name
    , params = params
    }


body : Parser ()
body =
    symbol "{"
        |. maybeWhitespaceParser
        |. symbol "}"


paramListParser : Parser (List String)
paramListParser =
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
