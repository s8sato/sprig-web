port module Main exposing (..)

import Browser
import Config
import Html exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Page as P
import Page.App.App as App
import Page.Invite as Invite
import Page.LP as LP
import Page.Login as Login
import Page.Objet as Objet
import Page.Register as Register
import Util as U


main : Program () Mdl Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port newTab : Decode.Value -> Cmd msg


port tabHere : (Decode.Value -> msg) -> Sub msg


port setCaret : Decode.Value -> Cmd msg



-- MODEL


type alias Mdl =
    { mdl0 : Mdl0
    , mdl1 : Mdl1
    , isDemo : Bool
    }


type Mdl0
    = LPMdl LP.Mdl
    | InviteMdl Invite.Mdl
    | RegisterMdl Register.Mdl
    | LoginMdl Login.Mdl
    | AppMdl App.Mdl


type Mdl1
    = ObjetMdl Objet.Mdl


init : () -> ( Mdl, Cmd Msg )
init _ =
    let
        isDemo =
            Config.isDemo

        ( m0, c0 ) =
            LP.init isDemo |> U.map LPMdl LPMsg

        ( m1, c1 ) =
            Objet.init |> U.map ObjetMdl ObjetMsg
    in
    ( Mdl m0 m1 isDemo
    , Cmd.batch
        [ c0 |> Cmd.map Msg0
        , c1 |> Cmd.map Msg1
        ]
    )



-- UPDATE


type Msg
    = Msg0 Msg0
    | Msg1 Msg1


type Msg0
    = LPMsg LP.Msg
    | InviteMsg Invite.Msg
    | RegisterMsg Register.Msg
    | LoginMsg Login.Msg
    | AppMsg App.Msg


type Msg1
    = ObjetMsg Objet.Msg


update : Msg -> Mdl -> ( Mdl, Cmd Msg )
update msg mdl =
    case findGoto msg of
        Just page ->
            goto mdl page

        _ ->
            case msg of
                Msg0 msg0 ->
                    let
                        ( m0, c0 ) =
                            case ( msg0, mdl.mdl0 ) of
                                ( LPMsg msg_, LPMdl mdl_ ) ->
                                    LP.update msg_ mdl_ |> U.map LPMdl LPMsg

                                ( InviteMsg msg_, InviteMdl mdl_ ) ->
                                    Invite.update msg_ mdl_ |> U.map InviteMdl InviteMsg

                                ( RegisterMsg msg_, RegisterMdl mdl_ ) ->
                                    Register.update msg_ mdl_ |> U.map RegisterMdl RegisterMsg

                                ( LoginMsg msg_, LoginMdl mdl_ ) ->
                                    Login.update msg_ mdl_ |> U.map LoginMdl LoginMsg

                                ( AppMsg msg_, AppMdl mdl_ ) ->
                                    case msg_ of
                                        App.NewTab url ->
                                            ( mdl.mdl0, url |> Encode.string |> newTab )

                                        App.SetCaret i ->
                                            ( mdl.mdl0, i |> Encode.int |> setCaret )

                                        _ ->
                                            App.update msg_ mdl_ |> U.map AppMdl AppMsg

                                _ ->
                                    ( mdl.mdl0, Cmd.none )
                    in
                    ( { mdl | mdl0 = m0 }, c0 |> Cmd.map Msg0 )

                Msg1 msg1 ->
                    let
                        ( m1, c1 ) =
                            case ( msg1, mdl.mdl1 ) of
                                ( ObjetMsg msg_, ObjetMdl mdl_ ) ->
                                    Objet.update msg_ mdl_ |> U.map ObjetMdl ObjetMsg
                    in
                    ( { mdl | mdl1 = m1 }, c1 |> Cmd.map Msg1 )



-- VIEW


view : Mdl -> Html Msg
view mdl =
    let
        v0 =
            (case mdl.mdl0 of
                LPMdl m ->
                    LP.view m |> Html.map LPMsg

                InviteMdl m ->
                    Invite.view m |> Html.map InviteMsg

                RegisterMdl m ->
                    Register.view m |> Html.map RegisterMsg

                LoginMdl m ->
                    Login.view m |> Html.map LoginMsg

                AppMdl m ->
                    App.view m |> Html.map AppMsg
            )
                |> Html.map Msg0

        v1 =
            (case mdl.mdl1 of
                ObjetMdl m ->
                    Objet.view m |> Html.map ObjetMsg
            )
                |> Html.map Msg1

        onApp =
            case mdl.mdl0 of
                AppMdl _ ->
                    True

                _ ->
                    False

        bem =
            U.bem "univ"
    in
    div
        [ bem "" [ ( "pre-app", not onApp ) ] ]
        (if onApp then
            [ v0 ]

         else
            [ v0
            , div [ bem "objet" [] ]
                [ text "https://elm-lang.org/examples/cube"
                , v1
                ]
            ]
        )



-- SUBSCRIPTIONS


subscriptions : Mdl -> Sub Msg
subscriptions mdl =
    let
        s0 =
            case mdl.mdl0 of
                LPMdl m ->
                    LP.subscriptions m |> Sub.map LPMsg

                InviteMdl m ->
                    Invite.subscriptions m |> Sub.map InviteMsg

                RegisterMdl m ->
                    Register.subscriptions m |> Sub.map RegisterMsg

                LoginMdl m ->
                    Login.subscriptions m |> Sub.map LoginMsg

                AppMdl m ->
                    App.subscriptions m |> Sub.map AppMsg

        s1 =
            case mdl.mdl1 of
                ObjetMdl m ->
                    Objet.subscriptions m |> Sub.map ObjetMsg
    in
    Sub.batch
        [ s0 |> Sub.map Msg0
        , s1 |> Sub.map Msg1
        , tabHere (Decode.decodeValue Decode.int >> Result.withDefault 0 >> App.TabHere >> App.FromS >> AppMsg >> Msg0)
        ]



-- HELPER


findGoto : Msg -> Maybe P.Page
findGoto msg =
    case msg of
        Msg0 msg0 ->
            case msg0 of
                LPMsg (LP.Goto page) ->
                    Just page

                InviteMsg (Invite.Goto page) ->
                    Just page

                RegisterMsg (Register.Goto page) ->
                    Just page

                LoginMsg (Login.Goto page) ->
                    Just page

                AppMsg (App.Goto page) ->
                    Just page

                _ ->
                    Nothing

        _ ->
            Nothing


goto : Mdl -> P.Page -> ( Mdl, Cmd Msg )
goto mdl page =
    case ( mdl.mdl0, page ) of
        ( AppMdl _, P.LP ) ->
            -- logout to cancel demo mode
            LP.init False
                |> U.map LPMdl LPMsg
                |> (\( m0, c0 ) -> ( { mdl | mdl0 = m0, isDemo = False }, c0 |> Cmd.map Msg0 ))

        _ ->
            let
                mdl0 =
                    mdl.mdl0

                ( m0, c0 ) =
                    case page of
                        P.LP ->
                            LP.init mdl.isDemo |> U.map LPMdl LPMsg

                        P.Invite ->
                            case mdl0 of
                                LoginMdl m ->
                                    Invite.init m.forgot_pw m.tz |> U.map InviteMdl InviteMsg

                                _ ->
                                    ( mdl0, Cmd.none )

                        P.Register ->
                            case mdl0 of
                                InviteMdl m ->
                                    Register.init m.forgot_pw m.email |> U.map RegisterMdl RegisterMsg

                                _ ->
                                    ( mdl0, Cmd.none )

                        P.Login ->
                            case mdl0 of
                                LPMdl m ->
                                    Login.init m.cred |> U.map LoginMdl LoginMsg

                                _ ->
                                    ( mdl0, Cmd.none )

                        P.App_ P.App ->
                            case mdl0 of
                                LPMdl m ->
                                    case m.user of
                                        Just user ->
                                            App.init mdl.isDemo user |> U.map AppMdl AppMsg

                                        _ ->
                                            ( mdl0, Cmd.none )

                                _ ->
                                    ( mdl0, Cmd.none )
            in
            ( { mdl | mdl0 = m0 }, c0 |> Cmd.map Msg0 )
