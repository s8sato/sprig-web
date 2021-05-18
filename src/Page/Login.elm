module Page.Login exposing (..)

import Bool.Extra as BX
import EndPoint as EP
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Encode as Encode
import Maybe.Extra as MX
import Page as P
import Task
import Time
import Util as U



-- MODEL


type alias Mdl =
    { isDemo : Bool
    , cred : U.Cred
    , tz : String
    , forgot_pw : Bool
    , msg : String
    }


init : Maybe U.Cred -> ( Mdl, Cmd Msg )
init cred =
    ( { isDemo = cred |> MX.isJust
      , cred = cred |> Maybe.withDefault (U.Cred "" "")
      , tz = ""
      , forgot_pw = False
      , msg = ""
      }
    , Task.perform SetTz Time.getZoneName
    )



-- UPDATE


type Msg
    = Goto P.Page
    | SetTz Time.ZoneName
    | FromU FromU
    | FromS FromS


type FromU
    = Login
    | NewAccount
    | ForgotPW
    | EditEmail String
    | EditPassWord String


type FromS
    = LoggedIn U.HttpResultAny


update : Msg -> Mdl -> ( Mdl, Cmd Msg )
update msg mdl =
    case msg of
        Goto _ ->
            ( mdl, Cmd.none )

        SetTz zoneName ->
            ( { mdl
                | tz =
                    case zoneName of
                        Time.Name name ->
                            name

                        _ ->
                            "UTC"
              }
            , mdl.isDemo |> BX.ifElse (Login |> U.cmd FromU) Cmd.none
            )

        FromU fromU ->
            case fromU of
                Login ->
                    ( mdl, U.post_ EP.Auth (enc mdl) (FromS << LoggedIn) )

                NewAccount ->
                    ( { mdl | forgot_pw = False }, U.cmd Goto P.Invite )

                ForgotPW ->
                    ( { mdl | forgot_pw = True }, U.cmd Goto P.Invite )

                EditEmail s ->
                    ( { mdl
                        | cred =
                            let
                                cred =
                                    mdl.cred
                            in
                            { cred | email = s }
                      }
                    , Cmd.none
                    )

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

        FromS fromS ->
            case fromS of
                LoggedIn (Ok _) ->
                    ( mdl, U.cmd Goto P.LP )

                LoggedIn (Err e) ->
                    ( { mdl | msg = U.strHttpError e }, Cmd.none )


enc : Mdl -> Encode.Value
enc mdl =
    Encode.object
        [ ( "email", Encode.string mdl.cred.email )
        , ( "password", Encode.string mdl.cred.password )
        , ( "tz", Encode.string mdl.tz )
        ]



-- VIEW


view : Mdl -> Html Msg
view mdl =
    div [ class "pre-app" ]
        [ h1 [ class "pre-app__title" ] [ text "Login" ]
        , div [] [ U.input "email" "Email" mdl.cred.email EditEmail ]
        , div [] [ U.input "password" "Password" mdl.cred.password EditPassWord ]
        , div [] [ button [ onClick Login ] [ text "Login" ] ]
        , div [] [ button [ onClick NewAccount ] [ text "New Account" ] ]
        , div [] [ button [ onClick ForgotPW ] [ text "Forgot Password" ] ]
        , div [] [ text mdl.msg ]
        ]
        |> Html.map FromU



-- SUBSCRIPTIONS


subscriptions : Mdl -> Sub Msg
subscriptions _ =
    Sub.none
