module Api.Consumables exposing
    ( Error
    , delete
    , errorToString
    , get
    , getDetail
    , put
    )

import Effect exposing (Effect)
import Http exposing (Error(..))
import Iso8601
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Shared.Json exposing (decodePaginator)
import Shared.Model
    exposing
        ( Brand
        , Consumable
        , Group
        , Location
        , Paginator
        , ResourceType
        )
import Time exposing (Weekday(..))
import Url.Builder as Url


brandDecoder : Decode.Decoder Brand
brandDecoder =
    Decode.succeed Brand
        |> Decode.required "id" Decode.int
        |> Decode.required "name" Decode.string
        |> Decode.optional "locked" Decode.bool False


resourceTypeDecoder : Decode.Decoder ResourceType
resourceTypeDecoder =
    Decode.succeed ResourceType
        |> Decode.required "id" Decode.int
        |> Decode.required "name" Decode.string
        |> Decode.optional "locked" Decode.bool False


locationDecoder : Decode.Decoder Location
locationDecoder =
    Decode.succeed Location
        |> Decode.required "id" Decode.int
        |> Decode.required "name" Decode.string
        |> Decode.required "description" Decode.string


groupDecoder : Decode.Decoder Group
groupDecoder =
    Decode.succeed Group
        |> Decode.required "id" Decode.int
        |> Decode.required "name" Decode.string
        |> Decode.optional "description" (Decode.maybe Decode.string) Nothing
        |> Decode.optional "urlImage" (Decode.maybe Decode.string) Nothing
        |> Decode.required "phoneNumber" Decode.string
        |> Decode.required "location" locationDecoder


consumableDecoder : Decode.Decoder Consumable
consumableDecoder =
    Decode.succeed Consumable
        |> Decode.required "id" Decode.int
        |> Decode.required "barcode" Decode.string
        |> Decode.required "resourceType" resourceTypeDecoder
        |> Decode.required "brand" brandDecoder
        |> Decode.required "price" Decode.float
        |> Decode.required "purchaseDate" Iso8601.decoder
        |> Decode.required "name" Decode.string
        |> Decode.required "model" Decode.string
        |> Decode.required "description" Decode.string
        |> Decode.optional "urlImages" (Decode.list Decode.string) []
        |> Decode.required "quantityEachItem" Decode.float
        |> Decode.required "stock" Decode.float
        |> Decode.required "minStock" Decode.float
        |> Decode.required "location" locationDecoder
        |> Decode.required "group" groupDecoder


get :
    { onResponse : Result Http.Error (Paginator Consumable) -> msg
    , tokens : Shared.Model.Tokens
    , apiUrl : String
    , pageIndex : Int
    , filterString : String
    , hasStock : Bool
    }
    -> Effect msg
get options =
    let
        cmd : Cmd msg
        cmd =
            Http.request
                { method = "GET"
                , url =
                    Url.relative [ options.apiUrl, "resources/consumables/loose" ]
                        [ Url.string "size" "10"
                        , Url.string "pageIndex" <| String.fromInt options.pageIndex
                        , Url.string "filterString" options.filterString
                        , Url.string "hasStock" <|
                            if options.hasStock then
                                "true"

                            else
                                "false"
                        ]
                , headers =
                    [ Http.header "x-signature-token" options.tokens.signatureToken
                    , Http.header "x-header-payload-token" options.tokens.headerPayloadToken
                    ]
                , body = Http.emptyBody
                , expect = Http.expectJson options.onResponse <| decodePaginator consumableDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


delete :
    { onResponse : Result Http.Error String -> msg
    , tokens : Shared.Model.Tokens
    , apiUrl : String
    , consumableId : Int
    }
    -> Effect msg
delete { onResponse, tokens, apiUrl, consumableId } =
    let
        cmd : Cmd msg
        cmd =
            Http.request
                { method = "DELETE"
                , url = Url.relative [ apiUrl, "resources/consumables", String.fromInt consumableId ] []
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


getDetail :
    { onResponse : Result Http.Error Consumable -> msg
    , tokens : Shared.Model.Tokens
    , apiUrl : String
    , consumableId : String
    }
    -> Effect msg
getDetail options =
    let
        cmd : Cmd msg
        cmd =
            Http.request
                { method = "GET"
                , url = Url.relative [ options.apiUrl, "resources/consumables", options.consumableId ] []
                , headers =
                    [ Http.header "x-signature-token" options.tokens.signatureToken
                    , Http.header "x-header-payload-token" options.tokens.headerPayloadToken
                    ]
                , body = Http.emptyBody
                , expect = Http.expectJson options.onResponse (Decode.field "data" consumableDecoder)
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


put :
    { onResponse : Result Http.Error Consumable -> msg
    , tokens : Shared.Model.Tokens
    , apiUrl : String
    , jsonBody : Encode.Value
    , consumableId : String
    }
    -> Effect msg
put { onResponse, tokens, apiUrl, consumableId, jsonBody } =
    let
        cmd : Cmd msg
        cmd =
            Http.request
                { method = "PUT"
                , url = Url.relative [ apiUrl, "resources/consumables", consumableId ] []
                , headers =
                    [ Http.header "x-signature-token" tokens.signatureToken
                    , Http.header "x-header-payload-token" tokens.headerPayloadToken
                    ]
                , body = Http.jsonBody jsonBody
                , expect = Http.expectJson onResponse (Decode.field "data" consumableDecoder)
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


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
