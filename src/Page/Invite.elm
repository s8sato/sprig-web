module Page.Invite exposing (..)

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
    { email : String
    , forgot_pw : Bool
    , tz : String
    , msg : String
    }


init : Bool -> String -> ( Mdl, Cmd Msg )
init forgot_pw tz =
    ( Mdl "" forgot_pw tz "", Cmd.none )



-- UPDATE


type Msg
    = Goto P.Page
    | FromU FromU
    | FromS FromS


type FromU
    = InviteMe
    | EditEmail String


type FromS
    = InvitedYou U.HttpResultAny


update : Msg -> Mdl -> ( Mdl, Cmd Msg )
update msg mdl =
    case msg of
        Goto _ ->
            ( mdl, Cmd.none )

        FromU fromU ->
            case fromU of
                InviteMe ->
                    ( mdl, U.post_ EP.Invite (enc mdl) (FromS << InvitedYou) )

                EditEmail s ->
                    ( { mdl | email = s }, Cmd.none )

        FromS fromS ->
            case fromS of
                InvitedYou (Ok _) ->
                    ( mdl, U.cmd Goto P.Register )

                InvitedYou (Err e) ->
                    ( { mdl | msg = U.strHttpError e }, Cmd.none )


enc : Mdl -> Encode.Value
enc mdl =
    Encode.object
        [ ( "email", Encode.string mdl.email )
        , ( "forgot_pw", Encode.bool mdl.forgot_pw )
        , ( "tz", Encode.string mdl.tz )
        ]



-- VIEW


view : Mdl -> Html Msg
view mdl =
    div [ class "pre-app" ]
        [ h1 [ class "pre-app__title" ] [ mdl.forgot_pw |> BX.ifElse "Forgot Password" "Invite" |> text ]
        , div [] [ U.input "email" "Email" mdl.email EditEmail ]
        , div [] [ button [ onClick InviteMe ] [ mdl.forgot_pw |> BX.ifElse "Get Reset Key" "Get Invitation" |> text ] ]
        , div [] [ text mdl.msg ]
        ]
        |> Html.map FromU



-- SUBSCRIPTIONS


subscriptions : Mdl -> Sub Msg
subscriptions _ =
    Sub.none
