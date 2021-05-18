module Page.LP exposing (..)

import Dict
import EndPoint as EP
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (required)
import Maybe.Extra as MX
import Page as P
import Time
import TimeZone
import Util as U



-- MODEL


type alias Mdl =
    { isDemo : Bool
    , user : Maybe U.AuthedUser
    , cred : Maybe U.Cred
    , msg : String
    }


init : Bool -> ( Mdl, Cmd Msg )
init isDemo =
    ( Mdl isDemo Nothing Nothing ""
    , GetMe |> U.cmd identity
    )



-- UPDATE


type Msg
    = Goto P.Page
    | GetMe
    | GetAccount
    | FromS FromS


type FromS
    = GotYou (U.HttpResult ResGetMe)
    | GotAccount (U.HttpResult ResGetAccount)


update : Msg -> Mdl -> ( Mdl, Cmd Msg )
update msg mdl =
    case msg of
        Goto _ ->
            ( mdl, Cmd.none )

        GetMe ->
            ( mdl, U.get EP.Auth [] (FromS << GotYou) decGetMe )

        GetAccount ->
            ( mdl, U.get EP.Register [] (FromS << GotAccount) decGetAccount )

        FromS fromS ->
            case fromS of
                GotYou (Ok ( _, res )) ->
                    ( { mdl | user = Just res }, U.cmd Goto (P.App_ P.App) )

                GotYou (Err e) ->
                    case U.errCode e of
                        -- Unauthorized
                        Just 401 ->
                            if mdl.isDemo then
                                ( mdl, GetAccount |> U.cmd identity )

                            else
                                ( mdl, U.cmd Goto P.Login )

                        _ ->
                            ( { mdl | msg = U.strHttpError e }, Cmd.none )

                GotAccount (Ok ( _, res )) ->
                    ( { mdl | cred = Just res }, U.cmd Goto P.Login )

                GotAccount (Err e) ->
                    ( { mdl | msg = U.strHttpError e }, Cmd.none )



-- VIEW


view : Mdl -> Html Msg
view mdl =
    div [ class "pre-app" ]
        [ h1 [ class "pre-app__title" ] [ text "LP" ]
        , div [] [ text mdl.msg ]
        ]



-- SUBSCRIPTIONS


subscriptions : Mdl -> Sub Msg
subscriptions _ =
    Sub.none



-- INTERFACE


type alias ResGetMe =
    U.AuthedUser


decGetMe : Decoder ResGetMe
decGetMe =
    Decode.succeed U.AuthedUser
        |> required "name" string
        |> required "tz"
            (string
                |> Decode.map
                    (\s ->
                        Dict.get s TimeZone.zones
                            |> MX.unwrap Time.utc (\z -> z ())
                    )
            )
        |> required "timescale" (string |> Decode.map U.timescale)
        |> required "allocations" (list U.decAllocation)


type alias ResGetAccount =
    U.Cred


decGetAccount : Decoder ResGetAccount
decGetAccount =
    Decode.succeed U.Cred
        |> required "email" string
        |> required "password" string
