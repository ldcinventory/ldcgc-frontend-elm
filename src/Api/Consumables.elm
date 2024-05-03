module Api.Consumables exposing (delete, errorToString, get)

import Effect exposing (Effect)
import Http exposing (Error(..))
import Iso8601
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Shared.Model exposing (Consumable, Paginator)
import Time exposing (Weekday(..))
import Url.Builder as Url


decoder : Decode.Decoder (Paginator Consumable)
decoder =
    Decode.field "data" <|
        (Decode.succeed Paginator
            |> Decode.required "numElements" Decode.int
            |> Decode.required "totalPages" Decode.int
            |> Decode.required "elementsThisPage" Decode.int
            |> Decode.required "elements" (Decode.list consumableDecoder)
        )


consumableDecoder : Decode.Decoder Consumable
consumableDecoder =
    Decode.succeed Consumable
        |> Decode.required "id" Decode.int
        |> Decode.required "barcode" Decode.string
        |> Decode.required "price" Decode.float
        |> Decode.required "purchaseDate" Iso8601.decoder
        |> Decode.required "name" Decode.string
        |> Decode.required "model" Decode.string
        |> Decode.required "description" Decode.string
        |> Decode.required "urlImages" (Decode.list Decode.string)
        |> Decode.required "quantityEachItem" Decode.float
        |> Decode.required "stock" Decode.float
        |> Decode.required "minStock" Decode.float


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
