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

    -- , ( "(λx.xxx) (λy.yyy)"
    --   , Application
    --         (Abstraction "x"
    --             (Application (Variable "x") (Application (Variable "x") (Variable "x")))
    --         )
    --         (Abstraction "y"
    --             (Application (Variable "y") (Application (Variable "y") (Variable "y")))
    --         )
    --   )
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

    , ( "(λxyz.xz(yz))(λxy.x)(λxyz.x(yz))(λxy.x)(λxyz.x(yz))(λxy.x)"
      , Application
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
                    (Abstraction "x" (Abstraction "y" (Variable "x")))
                )
                (Application
                    (Abstraction "x"
                        (Abstraction "y"
                            (Abstraction "z"
                                (Application (Variable "x")
                                    (Application (Variable "y") (Variable "z"))
                                )
                            )
                        )
                    )
                    (Abstraction "x" (Abstraction "y" (Variable "x")))
                )
            )
            (Application
                (Abstraction "x"
                    (Abstraction "y"
                        (Abstraction "z"
                            (Application (Variable "x")
                                (Application (Variable "y") (Variable "z"))
                            )
                        )
                    )
                )
                (Abstraction "x" (Abstraction "y" (Variable "x")))
            )
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
            maybeReduce term (apply param value body)

        Application term1 term2 ->
            maybeReduce term (Application (reduce term1) (reduce term2))


maybeReduce : Term -> Term -> Term
maybeReduce current next =
    if current == next || String.length (Debug.toString next) > String.length (Debug.toString current) then
        next

    else
        reduce <| next


apply : String -> Term -> Term -> Term
apply param value body =
    case body of
        Variable name ->
            if name == param then
                reduce <| value

            else
                reduce body

        Abstraction param_ body_ ->
            Abstraction param_ (reduce <| apply param value body_)

        Application term1 term2 ->
            Application (apply param value term1) (apply param value term2)


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
