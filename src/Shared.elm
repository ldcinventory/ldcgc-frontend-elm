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
import Shared.Model
import Shared.Msg



-- FLAGS


type alias Flags =
    { user : Maybe Shared.Model.User
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed Flags
        |> Decode.andMap (Json.Decode.field "user" (Json.Decode.maybe userDecoder))


userDecoder : Json.Decode.Decoder Shared.Model.User
userDecoder =
    Json.Decode.succeed Shared.Model.User
        |> Decode.andMap (Json.Decode.field "signatureToken" Json.Decode.string)
        |> Decode.andMap (Json.Decode.field "headerPayloadToken" Json.Decode.string)
        |> Decode.andMap (Json.Decode.field "id" Json.Decode.int)
        -- |> Decode.andMap(Json.Decode.field "name" Json.Decode.string)
        |> Decode.andMap (Json.Decode.field "role" Json.Decode.string)
        |> Decode.andMap (Json.Decode.field "email" Json.Decode.string)



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult _ =
    let
        flags : Flags
        flags =
            flagsResult
                |> Result.withDefault { user = Nothing }
    in
    ( { user = flags.user }
    , Effect.none
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        Shared.Msg.SignIn user ->
            ( { model
                | user = Just user
              }
            , Effect.batch
                [ Effect.pushRoute
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



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
