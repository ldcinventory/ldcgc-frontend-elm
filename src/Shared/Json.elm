module Shared.Json exposing (decodeRole, decodeUser, encodeUser)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as Encode
import Shared.Model exposing (Role(..), User)


encodeRole : Role -> Value
encodeRole role =
    case role of
        Admin ->
            Encode.string "admin"

        Manager ->
            Encode.string "volunteer"

        UserRole ->
            Encode.string "user"


encodeUser : User -> Value
encodeUser user =
    Encode.object
        [ ( "signatureToken", Encode.string user.tokens.signatureToken )
        , ( "headerPayloadToken", Encode.string user.tokens.headerPayloadToken )
        , ( "id", Encode.int user.id )
        , ( "role", encodeRole user.role )
        , ( "email", Encode.string user.email )
        , ( "volunteer"
          , Encode.maybe
                (\name ->
                    Encode.object
                        [ ( "name", Encode.string name )
                        ]
                )
                user.name
          )
        ]


decodeRole : Decoder Role
decodeRole =
    Decode.string
        |> Decode.andThen
            (\role ->
                case role of
                    "admin" ->
                        Decode.succeed Admin

                    "manager" ->
                        Decode.succeed Manager

                    "user" ->
                        Decode.succeed UserRole

                    str ->
                        Decode.fail <| "Invalid role found: " ++ str
            )


decodeUser : Decoder User
decodeUser =
    Decode.succeed User
        |> Decode.andMap tokensDecoder
        |> Decode.andMap (Decode.field "id" Decode.int)
        |> Decode.andMap (Decode.optionalField "volunteer" (Decode.field "name" Decode.string))
        |> Decode.andMap (Decode.field "role" decodeRole)
        |> Decode.andMap (Decode.field "email" Decode.string)


tokensDecoder : Decoder Shared.Model.Tokens
tokensDecoder =
    Decode.succeed Shared.Model.Tokens
        |> Decode.andMap (Decode.field "signatureToken" Decode.string)
        |> Decode.andMap (Decode.field "headerPayloadToken" Decode.string)
