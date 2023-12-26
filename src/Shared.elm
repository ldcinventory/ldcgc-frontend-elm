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
    { signatureToken : Maybe String
    , headerPayloadToken : Maybe String
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed Flags
        |> Decode.andMap (Json.Decode.field "signatureToken" (Json.Decode.maybe Json.Decode.string))
        |> Decode.andMap (Json.Decode.field "headerPayloadToken" (Json.Decode.maybe Json.Decode.string))



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult _ =
    let
        { signatureToken, headerPayloadToken } =
            flagsResult
                |> Result.withDefault
                    { signatureToken = Nothing
                    , headerPayloadToken = Nothing
                    }
    in
    ( { signatureToken = signatureToken, headerPayloadToken = headerPayloadToken }
    , Effect.none
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        Shared.Msg.SignIn { signatureToken, headerPayloadToken } ->
            ( { model
                | signatureToken = Just signatureToken
                , headerPayloadToken = Just headerPayloadToken
              }
            , Effect.batch
                [ Effect.pushRoute
                    { path = Route.Path.Home_
                    , query = Dict.empty
                    , hash = Nothing
                    }

                -- FIXME: only save in localStorage if `Remember me?` is ✅
                , Effect.saveUser
                    { signatureToken = signatureToken
                    , headerPayloadToken = headerPayloadToken
                    }
                ]
            )

        Shared.Msg.SignOut ->
            ( { model | signatureToken = Nothing, headerPayloadToken = Nothing }
              -- TODO: we have an endpoint for this!
            , Effect.clearUser
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
