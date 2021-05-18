module Page.Register exposing (..)

import Bool.Extra as BX
import EndPoint as EP
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Encode as Encode
import Page as P
import Util as U



-- MODEL


type alias Mdl =
    { key : String
    , cred : U.Cred
    , reset_pw : Bool
    , confirmation : String
    , msg : String
    }


init : Bool -> String -> ( Mdl, Cmd Msg )
init reset_pw email =
    ( Mdl "" (U.Cred email "") reset_pw "" ""
    , Cmd.none
    )



-- UPDATE


type Msg
    = Goto P.Page
    | FromU FromU
    | FromS FromS


type FromU
    = RegisterMe
    | EditKey String
    | EditPassWord String
    | EditConfirmation String


type FromS
    = RegisteredYou U.HttpResultAny


update : Msg -> Mdl -> ( Mdl, Cmd Msg )
update msg mdl =
    case msg of
        Goto _ ->
            ( mdl, Cmd.none )

        FromU fromU ->
            case fromU of
                RegisterMe ->
                    case faultOf mdl of
                        Just fault ->
                            ( { mdl | msg = fault }, Cmd.none )

                        _ ->
                            ( mdl, U.post_ EP.Register (enc mdl) (FromS << RegisteredYou) )

                EditKey s ->
                    ( { mdl | key = s }, Cmd.none )

                EditPassWord s ->
                    ( { mdl
                        | cred =
                            let
                                cred =
                                    mdl.cred
                            in
                            { cred | password = s }
                      }
                    , Cmd.none
                    )

                EditConfirmation s ->
                    ( { mdl | confirmation = s }, Cmd.none )

        FromS fromS ->
            case fromS of
                RegisteredYou (Ok _) ->
                    ( mdl, U.cmd Goto P.LP )

                RegisteredYou (Err e) ->
                    ( { mdl | msg = U.strHttpError e }, Cmd.none )


faultOf : Mdl -> Maybe String
faultOf mdl =
    let
        passwordLen =
            8
    in
    [ mdl.cred.password /= mdl.confirmation
    , String.length mdl.cred.password < passwordLen
    , String.length mdl.key /= 36
    ]
        |> U.overwrite Nothing
            ([ "Password does not match confirmation."
             , [ "Password should be at least", U.int passwordLen, "length." ] |> String.join " "
             , [ "Enter the"
               , mdl.reset_pw |> BX.ifElse "reset" "register"
               , "key correctly."
               ]
                |> String.join " "
             ]
                |> List.map Just
            )


enc : Mdl -> Encode.Value
enc mdl =
    Encode.object
        [ ( "key", Encode.string mdl.key )
        , ( "email", Encode.string mdl.cred.email )
        , ( "password", Encode.string mdl.cred.password )
        , ( "reset_pw", Encode.bool mdl.reset_pw )
        ]



-- VIEW


view : Mdl -> Html Msg
view mdl =
    div [ class "pre-app" ]
        [ h1 [ class "pre-app__title" ] [ mdl.reset_pw |> BX.ifElse "Reset Password" "Register" |> text ]
        , div [] [ U.input "password" (mdl.reset_pw |> BX.ifElse "Reset Key" "Register Key") mdl.key EditKey ]
        , div [] [ U.input "password" "New Password" mdl.cred.password EditPassWord ]
        , div [] [ U.input "password" "Confirmation" mdl.confirmation EditConfirmation ]
        , div [] [ button [ onClick RegisterMe ] [ mdl.reset_pw |> BX.ifElse "Reset Password" "Register" |> text ] ]
        , div [] [ text mdl.msg ]
        ]
        |> Html.map FromU



-- SUBSCRIPTIONS


subscriptions : Mdl -> Sub Msg
subscriptions _ =
    Sub.none
