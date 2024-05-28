port module Effect exposing
    ( Effect
    , none, batch
    , sendCmd, sendMsg
    , pushRoute, replaceRoute, loadExternalUrl
    , signIn, signOut
    , map, toCmd
    , saveUser, clearUser
    , pushPath, replacePath
    , delayMsg, sendToast, sendToastMsg
    )

{-|

@docs Effect
@docs none, batch
@docs sendCmd, sendMsg
@docs pushRoute, replaceRoute, loadExternalUrl
@docs signIn, signOut
@docs map, toCmd
@docs saveUser, clearUser
@docs pushPath, replacePath

-}

import Browser.Navigation
import Components.Toast as To
import Dict exposing (Dict)
import Html.Attributes exposing (value)
import Json.Encode as Encode
import Json.Encode.Extra as Encode
import Process
import Route
import Route.Path
import Shared.Json exposing (encodeUser)
import Shared.Model
import Shared.Msg
import Task
import Toast
import Url exposing (Url)


type Effect msg
    = -- BASICS
      None
    | Batch (List (Effect msg))
    | SendCmd (Cmd msg)
      -- ROUTING
    | PushUrl String
    | ReplaceUrl String
    | LoadExternalUrl String
      -- SHARED
    | SendSharedMsg Shared.Msg.Msg
    | SendToLocalStorage { key : String, value : Encode.Value }



-- PORTS


port sendToLocalStorage : { key : String, value : Encode.Value } -> Cmd msg


saveUser : Shared.Model.User -> Effect msg
saveUser user =
    SendToLocalStorage
        { key = "user"
        , value = encodeUser user
        }


clearUser : Effect msg
clearUser =
    SendToLocalStorage
        { key = "user"
        , value = Encode.null
        }



-- BASICS


{-| Don't send any effect.
-}
none : Effect msg
none =
    None


{-| Send multiple effects at once.
-}
batch : List (Effect msg) -> Effect msg
batch =
    Batch


{-| Send a normal `Cmd msg` as an effect, something like `Http.get` or `Random.generate`.
-}
sendCmd : Cmd msg -> Effect msg
sendCmd =
    SendCmd


{-| Send a message as an effect. Useful when emitting events from UI components.
-}
sendMsg : msg -> Effect msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity
        |> SendCmd



-- ROUTING


{-| Set the new route, and make the back button go back to the current route.
-}
pushRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
pushRoute route =
    PushUrl (Route.toString route)


{-| Set given path as route (without any query params or hash), and make the back button go back to the current route.
-}
pushPath :
    Route.Path.Path
    -> Effect msg
pushPath path =
    PushUrl (Route.toString { path = path, query = Dict.empty, hash = Nothing })


{-| Set the new route, but replace the previous one, so clicking the back
button **won't** go back to the previous route.
-}
replaceRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
replaceRoute route =
    ReplaceUrl (Route.toString route)


{-| Set given path as route (without any query params or hash), but replace the previous route,
so clicking the back button **won't** go back to the previous route
-}
replacePath :
    Route.Path.Path
    -> Effect msg
replacePath path =
    ReplaceUrl (Route.toString { path = path, query = Dict.empty, hash = Nothing })


{-| Redirect users to a new URL, somewhere external your web application.
-}
loadExternalUrl : String -> Effect msg
loadExternalUrl =
    LoadExternalUrl



-- INTERNALS


{-| Elm Land depends on this function to connect pages and layouts
together into the overall app.
-}
map : (msg1 -> msg2) -> Effect msg1 -> Effect msg2
map fn effect =
    case effect of
        None ->
            None

        Batch list ->
            Batch (List.map (map fn) list)

        SendCmd cmd ->
            SendCmd (Cmd.map fn cmd)

        PushUrl url ->
            PushUrl url

        ReplaceUrl url ->
            ReplaceUrl url

        LoadExternalUrl url ->
            LoadExternalUrl url

        SendSharedMsg sharedMsg ->
            SendSharedMsg sharedMsg

        SendToLocalStorage value ->
            SendToLocalStorage value


{-| Elm Land depends on this function to perform your effects.
-}
toCmd :
    { key : Browser.Navigation.Key
    , url : Url
    , shared : Shared.Model.Model
    , fromSharedMsg : Shared.Msg.Msg -> msg
    , batch : List msg -> msg
    , toCmd : msg -> Cmd msg
    }
    -> Effect msg
    -> Cmd msg
toCmd options effect =
    case effect of
        None ->
            Cmd.none

        Batch list ->
            Cmd.batch (List.map (toCmd options) list)

        SendCmd cmd ->
            cmd

        PushUrl url ->
            Browser.Navigation.pushUrl options.key url

        ReplaceUrl url ->
            Browser.Navigation.replaceUrl options.key url

        LoadExternalUrl url ->
            Browser.Navigation.load url

        SendSharedMsg sharedMsg ->
            Task.succeed sharedMsg
                |> Task.perform options.fromSharedMsg

        SendToLocalStorage value ->
            sendToLocalStorage value



-- SHARED


signIn : Shared.Model.User -> Effect msg
signIn user =
    SendSharedMsg <| Shared.Msg.SignIn user


signOut : Effect msg
signOut =
    SendSharedMsg Shared.Msg.SignOut


sendToast : String -> To.ToastType -> Effect msg
sendToast message toastType =
    SendSharedMsg <| Shared.Msg.AddToast message toastType


sendToastMsg : Toast.Msg -> Effect msg
sendToastMsg msg =
    SendSharedMsg <| Shared.Msg.ToastMsg msg


delayMsg : msg -> Effect msg
delayMsg msg =
    Task.perform (always msg) (Process.sleep 500)
        |> sendCmd
