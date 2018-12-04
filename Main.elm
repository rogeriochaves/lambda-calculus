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
                        (Application (Variable "x")
                            (Application (Variable "y") (Variable "z"))
                        )
                    )
                )
            )
            (Variable "z")
      )
    , ( "(λxyz.zyx)yx(λpq.q)"
      , Application
            (Abstraction "x"
                (Abstraction "y"
                    (Abstraction "z"
                        (Application (Variable "z")
                            (Application (Variable "y")
                                (Variable "x")
                            )
                        )
                    )
                )
            )
            (Application
                (Variable "y")
                (Application
                    (Variable "x")
                    (Abstraction "p"
                        (Abstraction "q"
                            (Variable "q")
                        )
                    )
                )
            )
      )
    , ( "(λyz.zy)((λx.xxx)(λx.xxx))(λy.xxx)"
      , Application
            (Abstraction "y"
                (Abstraction "z"
                    (Application (Variable "z") (Variable "y"))
                )
            )
            (Application
                (Application
                    (Abstraction "x"
                        (Application (Variable "x") (Application (Variable "x") (Variable "x")))
                    )
                    (Abstraction "x"
                        (Application (Variable "x") (Application (Variable "x") (Variable "x")))
                    )
                )
                (Abstraction "y"
                    (Application (Variable "x") (Application (Variable "x") (Variable "x")))
                )
            )
      )
    , ( "(λxyz.xz(yz))(λxy.x)(λxyz.x(yz))(λxy.x)(λxyz.x(yz))(λxy.x)"
      , Application
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
            (Application
                (Abstraction "x" (Abstraction "y" (Variable "x")))
                (Application
                    (Abstraction "x"
                        (Abstraction "y"
                            (Abstraction "z"
                                (Application (Variable "x") (Application (Variable "y") (Variable "z")))
                            )
                        )
                    )
                    (Application
                        (Abstraction "x" (Abstraction "y" (Variable "x")))
                        (Application
                            (Abstraction "x"
                                (Abstraction "y"
                                    (Abstraction "z"
                                        (Application (Variable "x") (Application (Variable "y") (Variable "z")))
                                    )
                                )
                            )
                            (Abstraction "x" (Abstraction "y" (Variable "x")))
                        )
                    )
                )
            )
      )
    ]
        |> List.map
            (\( mathForm, ast ) ->
                tr []
                    [ td [] [ text mathForm ]
                    , td [] [ text <| toString <| reduce Dict.empty ast ]
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


type alias Stack =
    Dict String Term


reduce : Stack -> Term -> Term
reduce stack term =
    case term of
        Variable name ->
            case Dict.get name stack of
                Just var ->
                    var

                Nothing ->
                    term

        Abstraction param body ->
            let
                ( param_, stack_ ) =
                    if keyPresent param stack then
                        -- α-conversion
                        ( nextLetter param, Dict.insert param (Variable <| nextLetter param) stack )

                    else
                        ( param, stack )
            in
            Abstraction param_ (reduce stack_ body)

        Application (Abstraction param body) term2 ->
            let
                stack_ =
                    Dict.insert param term2 stack
            in
            reduce stack_ body

        Application term1 term2 ->
            let
                current =
                    Application term1 term2

                reduced =
                    Application (reduce stack term1) (reduce stack term2)
            in
            if reduced == current || String.length (Debug.toString reduced) > String.length (Debug.toString current) then
                reduced

            else
                reduce Dict.empty reduced


keyPresent : String -> Stack -> Bool
keyPresent name stack =
    Dict.values stack
        |> List.any (keyPresentOnTerm name)


keyPresentOnTerm : String -> Term -> Bool
keyPresentOnTerm name term =
    case term of
        Variable name_ ->
            name_ == name

        Abstraction param term_ ->
            List.any identity
                [ param == name
                , keyPresentOnTerm name term_
                ]

        Application term1 term2 ->
            List.any (keyPresentOnTerm name) [ term1, term2 ]


nextLetter : String -> String
nextLetter =
    String.map
        (\c ->
            if c == 'z' then
                'a'

            else
                Char.fromCode (Char.toCode c + 1)
        )


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
