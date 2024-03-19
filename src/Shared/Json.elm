module Shared.Json exposing
    ( decodeAbsence
    , decodeAvailability
    , decodeRole
    , decodeUser
    , encodeUser
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as Encode
import Shared.Model exposing (Role(..), User)
import Time


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


decodeAbsence : Decoder Shared.Model.Absence
decodeAbsence =
    Decode.succeed Shared.Model.Absence
        |> Decode.andMap (Decode.field "id" Decode.int)
        |> Decode.andMap (Decode.field "dateFrom" Decode.string)
        |> Decode.andMap (Decode.field "dateTo" Decode.string)
        |> Decode.andMap (Decode.field "builderAssistantId" Decode.string)


decodeAvailability : Decoder Time.Weekday
decodeAvailability =
    Decode.string
        |> Decode.andThen
            (\role ->
                case role of
                    "MONDAY" ->
                        Decode.succeed Time.Mon

                    "TUESDAY" ->
                        Decode.succeed Time.Tue

                    "WEDNESDAY" ->
                        Decode.succeed Time.Wed

                    "THURSDAY" ->
                        Decode.succeed Time.Thu

                    "FRIDAY" ->
                        Decode.succeed Time.Fri

                    "SATURDAY" ->
                        Decode.succeed Time.Sat

                    "SUNDAY" ->
                        Decode.succeed Time.Sun

                    str ->
                        Decode.fail <| "Invalid availability found: " ++ str
            )
