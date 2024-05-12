module Shared.Json exposing
    ( decodeAbsence
    , decodeAvailability
    , decodeRole
    , decodeUser
    , encodeConsumable
    , encodeUser
    , encodeVolunteerDetail
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as Encode
import Set.Any as Set
import Shared.Model exposing (Role(..), User)
import Time exposing (Weekday(..))


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
        |> Decode.required "id" Decode.int
        |> Decode.optionalAt [ "volunteer", "name" ] (Decode.maybe Decode.string) Nothing
        |> Decode.required "role" decodeRole
        |> Decode.required "email" Decode.string


tokensDecoder : Decoder Shared.Model.Tokens
tokensDecoder =
    Decode.succeed Shared.Model.Tokens
        |> Decode.required "signatureToken" Decode.string
        |> Decode.required "headerPayloadToken" Decode.string


decodeAbsence : Decoder Shared.Model.Absence
decodeAbsence =
    Decode.succeed Shared.Model.Absence
        |> Decode.required "id" Decode.int
        |> Decode.required "dateFrom" Decode.string
        |> Decode.required "dateTo" Decode.string
        |> Decode.required "builderAssistantId" Decode.string


encodeWeekday : Weekday -> Encode.Value
encodeWeekday weekday =
    case weekday of
        Mon ->
            Encode.string "MONDAY"

        Tue ->
            Encode.string "TUESDAY"

        Wed ->
            Encode.string "WEDNESDAY"

        Thu ->
            Encode.string "THURSDAY"

        Fri ->
            Encode.string "FRIDAY"

        Sat ->
            Encode.string "SATURDAY"

        Sun ->
            Encode.string "SUNDAY"


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


encodeVolunteerDetail : Shared.Model.VolunteerDetail -> Encode.Value
encodeVolunteerDetail volunteerDetails =
    Encode.object
        [ ( "id", Encode.int volunteerDetails.id )
        , ( "name", Encode.string volunteerDetails.name )
        , ( "lastName", Encode.string volunteerDetails.lastName )
        , ( "builderAssistantId", Encode.string volunteerDetails.builderAssistantId )
        , ( "absences", Encode.list Encode.string [] )
        , ( "availability", Set.encode encodeWeekday volunteerDetails.availability )
        ]


encodeBrand : Shared.Model.Brand -> Encode.Value
encodeBrand brand =
    Encode.object
        [ ( "id", Encode.int brand.id )
        , ( "name", Encode.string brand.name )
        , ( "locked", Encode.bool brand.locked )
        ]


encodeResourceType : Shared.Model.ResourceType -> Encode.Value
encodeResourceType rt =
    Encode.object
        [ ( "id", Encode.int rt.id )
        , ( "name", Encode.string rt.name )
        , ( "locked", Encode.bool rt.locked )
        ]


encodeLocation : Shared.Model.Location -> Encode.Value
encodeLocation loc =
    Encode.object
        [ ( "id", Encode.int loc.id )
        , ( "name", Encode.string loc.name )
        , ( "description", Encode.string loc.description )
        ]


encodeGroup : Shared.Model.Group -> Encode.Value
encodeGroup group =
    Encode.object
        [ ( "id", Encode.int group.id )
        , ( "name", Encode.string group.name )
        , ( "description", Encode.maybe Encode.string group.description )
        , ( "urlImage", Encode.maybe Encode.string group.urlImage )
        , ( "phoneNumber", Encode.string group.phoneNumber )
        , ( "location", encodeLocation group.location )
        ]


encodeConsumable : Shared.Model.Consumable -> Encode.Value
encodeConsumable details =
    Encode.object
        [ ( "id", Encode.int details.id )
        , ( "name", Encode.string details.name )
        , ( "brand", encodeBrand details.brand )
        , ( "resourceType", encodeResourceType details.resourceType )
        , ( "barcode", Encode.string details.barcode )
        , ( "model", Encode.string details.model )
        , ( "description", Encode.string details.description )
        , ( "quantityEachItem", Encode.float details.quantityEachItem )
        , ( "stock", Encode.float details.stock )
        , ( "minStock", Encode.float details.minStock )
        , ( "location", encodeLocation details.location )
        , ( "group", encodeGroup details.group )
        ]
