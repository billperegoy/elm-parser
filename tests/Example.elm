module Example exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Language exposing (..)


suite : Test
suite =
    describe "Basic parsing"
        [ test "can extract a single parameter" <|
            \_ ->
                let
                    code =
                        """
                             function myfun(a) {
                             }
                            """

                    parsedResult =
                        Language.parse code
                in
                    Expect.equal parsedResult (Ok { name = "myfun", params = [ "a" ] })
        , test "can extract multiple parameters" <|
            \_ ->
                let
                    code =
                        """
                             function myfun(a, b) {
                             }
                            """

                    parsedResult =
                        Language.parse code
                in
                    Expect.equal parsedResult (Ok { name = "myfun", params = [ "a", "b" ] })
        ]
