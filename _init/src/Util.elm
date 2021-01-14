module Util exposing (..)

import Bool.Extra as BX
import Config
import Date
import Dict
import EndPoint as EP
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Http.Detailed
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra as LX
import Maybe.Extra as MX
import Task
import Time
import Time.Extra exposing (Interval(..))
import Url.Builder exposing (QueryParameter)


url : EP.EndPoint -> List QueryParameter -> String
url ep query =
    let
        path =
            case ep of
                EP.Invite ->
                    [ "invite" ]

                EP.Register ->
                    [ "register" ]

                EP.Auth ->
                    [ "auth" ]

                EP.App_ app ->
                    "app"
                        :: (case app of
                                EP.Tasks ->
                                    [ "tasks" ]

                                EP.Task tid ->
                                    [ "task", String.fromInt tid ]
                           )
    in
    Url.Builder.crossOrigin Config.epBase path query



-- Http.Detailed.Error to inform user of 400 BadRequest details


type alias HttpError =
    Http.Detailed.Error String



-- type alias HttpResult_ a =
--     Result (Http.Detailed.Error String) a


type alias HttpResult a =
    Result HttpError ( Http.Metadata, a )


type alias HttpResultAny =
    HttpResult String


anyOk : HttpResultAny
anyOk =
    Ok ( { url = "", statusCode = 200, statusText = "Ok", headers = Dict.empty }, "" )



-- Http.riskyRequest allows API to set and receive Cookie


request : String -> EP.EndPoint -> List QueryParameter -> Http.Body -> (HttpResult a -> msg) -> Decoder a -> Cmd msg
request method ep query body resMsg dec =
    Http.riskyRequest
        { method = method
        , headers = []
        , url = url ep query
        , body = body
        , expect = Http.Detailed.expectJson resMsg dec
        , timeout = Nothing
        , tracker = Nothing
        }


request_ : String -> EP.EndPoint -> List QueryParameter -> Http.Body -> (HttpResultAny -> msg) -> Cmd msg
request_ method ep query body resMsg =
    Http.riskyRequest
        { method = method
        , headers = []
        , url = url ep query
        , body = body
        , expect = Http.Detailed.expectString resMsg
        , timeout = Nothing
        , tracker = Nothing
        }


get : EP.EndPoint -> List QueryParameter -> (HttpResult a -> msg) -> Decoder a -> Cmd msg
get ep query resMsg dec =
    request "GET" ep query Http.emptyBody resMsg dec


post : EP.EndPoint -> Encode.Value -> (HttpResult a -> msg) -> Decoder a -> Cmd msg
post ep enc resMsg dec =
    request "POST" ep [] (Http.jsonBody enc) resMsg dec


post_ : EP.EndPoint -> Encode.Value -> (HttpResult String -> msg) -> Cmd msg
post_ ep enc resMsg =
    request_ "POST" ep [] (Http.jsonBody enc) resMsg


put : EP.EndPoint -> Encode.Value -> (HttpResult a -> msg) -> Decoder a -> Cmd msg
put ep enc resMsg dec =
    request "PUT" ep [] (Http.jsonBody enc) resMsg dec


put_ : EP.EndPoint -> (HttpResult String -> msg) -> Cmd msg
put_ ep resMsg =
    request_ "PUT" ep [] Http.emptyBody resMsg


delete : EP.EndPoint -> Encode.Value -> (HttpResult a -> msg) -> Decoder a -> Cmd msg
delete ep enc resMsg dec =
    request "DELETE" ep [] (Http.jsonBody enc) resMsg dec


delete_ : EP.EndPoint -> (HttpResult String -> msg) -> Cmd msg
delete_ ep resMsg =
    request_ "DELETE" ep [] Http.emptyBody resMsg


errCode : Http.Detailed.Error String -> Maybe Int
errCode e =
    case e of
        Http.Detailed.BadStatus meta _ ->
            Just meta.statusCode

        _ ->
            Nothing


strHttpError : Http.Detailed.Error String -> String
strHttpError e =
    case e of
        Http.Detailed.BadUrl msg ->
            msg

        Http.Detailed.Timeout ->
            "Timeout"

        Http.Detailed.NetworkError ->
            "Network Error"

        Http.Detailed.BadStatus meta body ->
            case meta.statusCode of
                400 ->
                    "Oops, " ++ body

                401 ->
                    "Authentication failed."

                500 ->
                    "Internal Server Error"

                code ->
                    [ String.fromInt code, meta.statusText ] |> String.join " "

        Http.Detailed.BadBody _ _ msg ->
            msg


cmd : (a -> msg) -> a -> Cmd msg
cmd msgFrom x =
    Task.perform msgFrom (Task.succeed x)


map : (a -> mdl) -> (b -> msg) -> ( a, Cmd b ) -> ( mdl, Cmd msg )
map toMdl toMsg =
    Tuple.mapBoth toMdl (Cmd.map toMsg)


input : String -> String -> String -> (String -> msg) -> Html msg
input t p v toMsg =
    Html.input [ type_ t, placeholder p, value v, onInput toMsg ] []


len : List a -> String
len l =
    List.length l |> String.fromInt


len1 : List a -> String
len1 l =
    List.length l
        |> (\len_ -> 0 < len_ |> BX.ifElse (String.fromInt len_) "")


type alias Timescale =
    { interval : Interval
    , multiple : Int
    }


timescale : String -> Timescale
timescale s =
    case s of
        "Y" ->
            Timescale Year 1

        "Q" ->
            Timescale Quarter 1

        "M" ->
            Timescale Month 1

        "W" ->
            Timescale Week 1

        "D" ->
            Timescale Day 1

        "6h" ->
            Timescale Hour 6

        "h" ->
            Timescale Hour 1

        "15m" ->
            Timescale Minute 15

        "m" ->
            Timescale Minute 1

        "s" ->
            Timescale Second 1

        _ ->
            Timescale Day 1


strInterval : Interval -> String
strInterval i =
    case i of
        Year ->
            "Y"

        Quarter ->
            "Q"

        Month ->
            "M"

        Week ->
            "W"

        Day ->
            "D"

        Hour ->
            "h"

        Minute ->
            "m"

        Second ->
            "s"

        _ ->
            "?"


strTimescale : Timescale -> String
strTimescale t =
    1 < t.multiple |> BX.ifElse (t.multiple |> int) "" |> (\mult -> mult ++ strInterval t.interval)


fmtTS : Timescale -> String
fmtTS t =
    case t.interval of
        Year ->
            "Y//"

        Quarter ->
            "Y/M/"

        Month ->
            "Y/M/"

        Week ->
            "/M/D"

        Day ->
            "/M/D"

        Hour ->
            "//DTh:"

        Minute ->
            "h:m"

        Second ->
            ":m:s"

        _ ->
            "?"


clock : Time.Zone -> Time.Posix -> String
clock z t =
    let
        date =
            Date.fromPosix z t |> Date.format "yyyy/MM/dd EEE "

        time =
            [ Time.toHour
            , Time.toMinute
            ]
                |> List.map
                    (\to -> to z t |> String.fromInt |> String.padLeft 2 '0')
                |> String.join ":"
    in
    date ++ time


fmtDT : Timescale -> Time.Zone -> Time.Posix -> String
fmtDT ts z t =
    let
        date =
            Date.fromPosix z t

        h =
            Time.toHour z t |> int |> String.padLeft 2 '0'

        m =
            Time.toMinute z t |> int |> String.padLeft 2 '0'

        s =
            Time.toSecond z t |> int |> String.padLeft 2 '0'
    in
    case ts.interval of
        Year ->
            date |> Date.format "yyyy"

        Quarter ->
            date |> Date.format "yyyy/MM/"

        Month ->
            date |> Date.format "yyyy/MM/"

        Week ->
            date |> Date.format "/MM/dd"

        Day ->
            date |> Date.format "/MM/dd"

        Hour ->
            date |> Date.format "//dd" |> (\day -> [ day, "T", h, ":" ]) |> String.concat

        Minute ->
            [ h, ":", m ] |> String.concat

        Second ->
            [ ":", m, ":", s ] |> String.concat

        _ ->
            "?"


int : Int -> String
int =
    String.fromInt


lt : Time.Posix -> Time.Posix -> Bool
lt right left =
    Time.posixToMillis left |> (\l -> l < Time.posixToMillis right)


overwrite : a -> List a -> List Bool -> a
overwrite default xs bs =
    LX.zip xs bs
        |> List.foldl (\( x, b ) acc -> b |> BX.ifElse x acc) default


apply : Int -> (a -> a) -> a -> a
apply n f x =
    List.repeat n ()
        |> List.foldl (\_ -> f) x


ifTrue : (a -> Bool) -> (a -> a) -> a -> a
ifTrue p f x =
    p x |> BX.ifElse (f x) x


enumerate : List a -> List ( Int, a )
enumerate =
    List.indexedMap Tuple.pair
