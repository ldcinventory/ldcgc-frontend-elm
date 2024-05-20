module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Dict
import Effect exposing (Effect)
import Json.Decode
import Json.Decode.Extra as Decode
import Route exposing (Route)
import Route.Path
import Shared.Json exposing (decodeUser)
import Shared.Model
import Shared.Msg
import Task
import Time
import Toast



-- FLAGS


type alias Flags =
    { user : Maybe Shared.Model.User
    , apiUrl : String
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed Flags
        |> Decode.andMap (Json.Decode.field "user" (Json.Decode.maybe decodeUser))
        |> Decode.andMap (Json.Decode.field "apiUrl" Json.Decode.string)



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult _ =
    let
        flags : Flags
        flags =
            flagsResult
                |> Result.withDefault { user = Nothing, apiUrl = "" }
    in
    ( { user = flags.user
      , apiUrl = flags.apiUrl
      , tray = Toast.tray
      , time = Time.millisToPosix 0
      }
    , Effect.sendCmd <| Task.perform Shared.Msg.SetCurrentTime Time.now
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    let
        maybeRedirect : Maybe Route.Path.Path
        maybeRedirect =
            Dict.get "from" route.query
                |> Maybe.andThen Route.Path.fromString
    in
    case msg of
        Shared.Msg.SignIn user ->
            ( { model
                | user = Just user
              }
            , Effect.batch
                [ case maybeRedirect of
                    Just redirect ->
                        Effect.pushRoute
                            { path = redirect
                            , query = route.query
                            , hash = route.hash
                            }

                    Nothing ->
                        Effect.pushRoute
                            { path = Route.Path.Home_
                            , query = Dict.empty
                            , hash = Nothing
                            }

                -- FIXME: only save in localStorage if `Remember me?` is ✅
                , Effect.saveUser user
                ]
            )

        Shared.Msg.SignOut ->
            ( { model | user = Nothing }
            , Effect.clearUser
            )

        Shared.Msg.SetCurrentTime time ->
            ( { model | time = time }
            , Effect.none
            )

        Shared.Msg.ToastMsg toastMsg ->
            let
                ( newTray, newTmesg ) =
                    Toast.update toastMsg model.tray
            in
            ( { model | tray = newTray }
            , Effect.sendCmd <| Cmd.map Shared.Msg.ToastMsg newTmesg
            )

        Shared.Msg.AddToast message type_ ->
            let
                ( newTray, tmesg ) =
                    Toast.add model.tray <|
                        Toast.expireIn 5000 { message = message, toastType = type_ }
            in
            ( { model | tray = newTray }
            , Effect.sendCmd <| Cmd.map Shared.Msg.ToastMsg tmesg
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
