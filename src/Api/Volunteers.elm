module Api.Volunteers exposing
    ( delete
    , errorToString
    , get
    , getDetail
    , put
    , toEnglishWeekday
    )

import Effect exposing (Effect)
import Http exposing (Error(..))
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Regex exposing (Regex)
import Set.Any as Set
import Shared.Json
import Shared.Model exposing (Absence, Paginator, Volunteer, VolunteerDetail)
import Time exposing (Weekday(..))
import Url.Builder as Url


decoder : Decode.Decoder (Paginator Volunteer)
decoder =
    Decode.field "data" <|
        (Decode.succeed Paginator
            |> Decode.required "numElements" Decode.int
            |> Decode.required "totalPages" Decode.int
            |> Decode.required "elementsThisPage" Decode.int
            |> Decode.required "elements" (Decode.list volunteerDecoder)
        )


messageDecoder : Decode.Decoder String
messageDecoder =
    Decode.field "message" Decode.string


errorDecoder : Decode.Decoder Error
errorDecoder =
    Decode.field "data"
        (Decode.succeed Error
            |> Decode.required "message" Decode.string
            |> Decode.required "status" Decode.int
            |> Decode.required "httpStatus" Decode.string
        )


type alias Error =
    { message : String
    , status : Int
    , httpStatus : String
    }


volunteerDecoder : Decode.Decoder Volunteer
volunteerDecoder =
    Decode.succeed Volunteer
        |> Decode.required "id" Decode.int
        |> Decode.required "name" Decode.string
        |> Decode.required "lastName" Decode.string
        |> Decode.required "builderAssistantId" Decode.string
        |> Decode.optional "isActive" (Decode.maybe Decode.bool) Nothing


get :
    { onResponse : Result Http.Error (Paginator Volunteer) -> msg
    , tokens : Shared.Model.Tokens
    , apiUrl : String
    , pageIndex : Int
    , filterString : String
    }
    -> Effect msg
get options =
    let
        builderAssistantIdRegex : Regex
        builderAssistantIdRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^\\d{7}$"

        cmd : Cmd msg
        cmd =
            Http.request
                { method = "GET"
                , url =
                    Url.relative [ options.apiUrl, "volunteers" ]
                        [ Url.string "size" "10"
                        , Url.string "pageIndex" <| String.fromInt options.pageIndex
                        , if Regex.contains builderAssistantIdRegex options.filterString then
                            Url.string "builderAssistantId" options.filterString

                          else
                            Url.string "filterString" options.filterString
                        ]
                , headers =
                    [ Http.header "x-signature-token" options.tokens.signatureToken
                    , Http.header "x-header-payload-token" options.tokens.headerPayloadToken
                    ]
                , body = Http.emptyBody
                , expect = Http.expectJson options.onResponse decoder
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


delete :
    { onResponse : Result Http.Error String -> msg
    , tokens : Shared.Model.Tokens
    , apiUrl : String
    , builderAssistantId : String
    }
    -> Effect msg
delete { onResponse, tokens, apiUrl, builderAssistantId } =
    let
        cmd : Cmd msg
        cmd =
            Http.request
                { method = "DELETE"
                , url = Url.relative [ apiUrl, "volunteers", builderAssistantId ] []
                , headers =
                    [ Http.header "x-signature-token" tokens.signatureToken
                    , Http.header "x-header-payload-token" tokens.headerPayloadToken
                    ]
                , body = Http.emptyBody
                , expect = Http.expectStringResponse onResponse handleHttpResponse
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


handleHttpResponse : Http.Response String -> Result Http.Error String
handleHttpResponse response =
    case response of
        Http.BadUrl_ _ ->
            Err <| BadBody "Unexpected URL format"

        Http.Timeout_ ->
            Err <| BadBody "Request timed out, please try again"

        Http.NetworkError_ ->
            Err <| BadBody "Could not connect, please try again"

        Http.BadStatus_ {- statusCode -} _ body ->
            case Decode.decodeString errorDecoder body of
                Ok { message } ->
                    Err <| BadBody message

                Err _ ->
                    Err <| BadBody "Something unexpected happened"

        Http.GoodStatus_ {- headers -} _ body ->
            case Decode.decodeString messageDecoder body of
                Ok success ->
                    Ok success

                Err _ ->
                    Err <| BadBody "Something unexpected happened"



-- Volunteer Detail


getDetail :
    { onResponse : Result Http.Error VolunteerDetail -> msg
    , tokens : Shared.Model.Tokens
    , apiUrl : String
    , builderAssistantId : String
    }
    -> Effect msg
getDetail options =
    let
        cmd : Cmd msg
        cmd =
            Http.request
                { method = "GET"
                , url =
                    Url.relative [ options.apiUrl, "volunteers", options.builderAssistantId ] []
                , headers =
                    [ Http.header "x-signature-token" options.tokens.signatureToken
                    , Http.header "x-header-payload-token" options.tokens.headerPayloadToken
                    ]
                , body = Http.emptyBody
                , expect = Http.expectJson options.onResponse volunteerDetailDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


put :
    { onResponse : Result Http.Error String -> msg
    , tokens : Shared.Model.Tokens
    , apiUrl : String
    , jsonBody : Encode.Value
    , builderAssistantId : String
    }
    -> Effect msg
put { onResponse, tokens, apiUrl, builderAssistantId, jsonBody } =
    let
        cmd : Cmd msg
        cmd =
            Http.request
                { method = "PUT"
                , url = Url.relative [ apiUrl, "volunteers", builderAssistantId ] []
                , headers =
                    [ Http.header "x-signature-token" tokens.signatureToken
                    , Http.header "x-header-payload-token" tokens.headerPayloadToken
                    ]
                , body = Http.jsonBody jsonBody
                , expect = Http.expectStringResponse onResponse handleHttpResponse
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


toEnglishWeekday : Weekday -> String
toEnglishWeekday weekday =
    case weekday of
        Mon ->
            "Mon"

        Tue ->
            "Tue"

        Wed ->
            "Wed"

        Thu ->
            "Thu"

        Fri ->
            "Fri"

        Sat ->
            "Sat"

        Sun ->
            "Sun"


volunteerDetailDecoder : Decode.Decoder VolunteerDetail
volunteerDetailDecoder =
    Decode.field "data"
        (Decode.succeed VolunteerDetail
            |> Decode.required "id" Decode.int
            |> Decode.required "name" Decode.string
            |> Decode.required "lastName" Decode.string
            |> Decode.required "builderAssistantId" Decode.string
            |> Decode.optional "isActive" (Decode.maybe Decode.bool) Nothing
            |> Decode.optional "absences" (Decode.list Shared.Json.decodeAbsence) []
            |> Decode.required "availability" (Set.decode toEnglishWeekday Shared.Json.decodeAvailability)
        )


errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Timeout ->
            "Unable to reach the server, try again"

        NetworkError ->
            "Unable to reach the server, check your network connection"

        BadStatus 500 ->
            "The server had a problem, try again later"

        BadStatus 400 ->
            "Verify your information and try again"

        BadStatus _ ->
            "Unknown error"

        BadBody errorMessage ->
            errorMessage
