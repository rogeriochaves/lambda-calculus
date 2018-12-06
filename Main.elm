module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)


main : Program () () msg
main =
    Browser.sandbox { init = (), view = always view, update = \_ _ -> () }


view : Html msg
view =
    [ ( "λx.x", Abstraction "x" (Variable "x") )
    , ( "(λx.x) y", Application (Abstraction "x" (Variable "x")) (Variable "y") )
    , ( "(λx.z) y", Application (Abstraction "x" (Variable "z")) (Variable "y") )
    , ( "(λx.λy.xy) z"
      , Application
            (Abstraction "x"
                (Abstraction "y"
                    (Application (Variable "x") (Variable "y"))
                )
            )
            (Variable "z")
      )
    , ( "(λx.λy.xy) (λz.a)"
      , Application
            (Abstraction "x"
                (Abstraction "y"
                    (Application (Variable "x") (Variable "y"))
                )
            )
            (Abstraction "z" (Variable "a"))
      )
    , ( "(λy.λz.yz) z"
      , Application
            (Abstraction "y"
                (Abstraction "z"
                    (Application (Variable "y") (Variable "z"))
                )
            )
            (Variable "z")
      )
    , ( "(λx.λy.xy) (λy.a)"
      , Application
            (Abstraction "x"
                (Abstraction "y"
                    (Application (Variable "x") (Variable "y"))
                )
            )
            (Abstraction "y" (Variable "a"))
      )
    , ( "(λx.xx) (λy.yy)"
      , Application
            (Abstraction "x"
                (Application (Variable "x") (Variable "x"))
            )
            (Abstraction "y"
                (Application (Variable "y") (Variable "y"))
            )
      )
    , ( "(λx.xxx) (λy.yyy)"
      , Application
            (Abstraction "x"
                (Application (Variable "x") (Application (Variable "x") (Variable "x")))
            )
            (Abstraction "y"
                (Application (Variable "y") (Application (Variable "y") (Variable "y")))
            )
      )
    , ( "(λx.λy.λz.xyz) y"
      , Application
            (Abstraction "x"
                (Abstraction "y"
                    (Abstraction "z"
                        (Application
                            (Application (Variable "x")
                                (Variable "y")
                            )
                            (Variable "z")
                        )
                    )
                )
            )
            (Variable "y")
      )
    , ( "(λabz.zba)yx(λpq.q)"
      , Application
            (Application
                (Application
                    (Abstraction "a"
                        (Abstraction "b"
                            (Abstraction "z"
                                (Application
                                    (Application (Variable "z")
                                        (Variable "b")
                                    )
                                    (Variable "a")
                                )
                            )
                        )
                    )
                    (Variable "y")
                )
                (Variable "x")
            )
            (Abstraction "p"
                (Abstraction "q"
                    (Variable "q")
                )
            )
      )
    , ( "(λyz.zy)((λx.xxx)(λx.xxx))(λy.xxx)"
      , Application
            (Application
                (Abstraction "y"
                    (Abstraction "z"
                        (Application (Variable "z") (Variable "y"))
                    )
                )
                (Application
                    (Abstraction "x"
                        (Application (Variable "x") (Application (Variable "x") (Variable "x")))
                    )
                    (Abstraction "x"
                        (Application (Variable "x") (Application (Variable "x") (Variable "x")))
                    )
                )
            )
            (Abstraction "y"
                (Application (Variable "x") (Application (Variable "x") (Variable "x")))
            )
      )
    , ( "(λxyz.xz(yz))(λab.a)(λcde.c(de))(λfg.f)(λhij.h(ij))(λkl.k)"
      , Application
            (Application
                (Application
                    (Application
                        (Application
                            (Abstraction "x"
                                (Abstraction "y"
                                    (Abstraction "z"
                                        (Application (Variable "x")
                                            (Application (Variable "z")
                                                (Application (Variable "y") (Variable "z"))
                                            )
                                        )
                                    )
                                )
                            )
                            (Abstraction "a" (Abstraction "b" (Variable "a")))
                        )
                        (Abstraction "c"
                            (Abstraction "d"
                                (Abstraction "e"
                                    (Application (Variable "c")
                                        (Application (Variable "d") (Variable "e"))
                                    )
                                )
                            )
                        )
                    )
                    (Abstraction "f" (Abstraction "g" (Variable "f")))
                )
                (Abstraction "h"
                    (Abstraction "i"
                        (Abstraction "j"
                            (Application (Variable "h")
                                (Application (Variable "i") (Variable "j"))
                            )
                        )
                    )
                )
            )
            (Abstraction "k" (Abstraction "l" (Variable "k")))
      )
    ]
        |> List.map
            (\( mathForm, ast ) ->
                tr []
                    [ td [] [ text mathForm ]
                    , td [] [ text <| toString <| reduce ast ]
                    ]
            )
        |> (++)
            [ tr []
                [ td [ width 180 ] [ text "Original" ]
                , td [] [ text "β-reduction" ]
                ]
            ]
        |> table [ attribute "border" "1", attribute "cellpadding" "5" ]


type Term
    = Variable String
    | Abstraction String Term
    | Application Term Term


reduce : Term -> Term
reduce term =
    case term of
        Variable _ ->
            term

        Abstraction param body ->
            Abstraction param (reduce body)

        Application (Abstraction param body) value ->
            maybeReduce term (substitute param value body)

        Application (Variable _) _ ->
            term

        Application term1 term2 ->
            maybeReduce term (Application (reduce term1) term2)


substitute : String -> Term -> Term -> Term
substitute param value term =
    case term of
        Variable name ->
            if name == param then
                value

            else
                term

        Abstraction param_ body ->
            Abstraction param_ (substitute param value body)

        Application term1 term2 ->
            Application (substitute param value term1) (substitute param value term2)


maybeReduce : Term -> Term -> Term
maybeReduce current next =
    if current == next || String.length (Debug.toString next) > String.length (Debug.toString current) * 2 then
        next

    else
        reduce <| next


toString : Term -> String
toString term =
    case term of
        Variable name ->
            name

        Abstraction param body ->
            case body of
                Abstraction param2 (Abstraction param3 body3) ->
                    "λ" ++ param ++ param2 ++ param3 ++ "." ++ toString body3

                Abstraction param2 body2 ->
                    "λ" ++ param ++ param2 ++ "." ++ toString body2

                _ ->
                    "λ" ++ param ++ "." ++ toString body

        Application term1 term2 ->
            let
                maybeParens term_ =
                    case term_ of
                        Variable _ ->
                            toString term_

                        Application (Variable _) (Variable _) ->
                            toString term_

                        _ ->
                            "(" ++ toString term_ ++ ")"
            in
            maybeParens term1 ++ maybeParens term2
