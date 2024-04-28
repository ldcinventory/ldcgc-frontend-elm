module Api.Consumables exposing (errorToString, get)

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
    }
    -> Effect msg
get options =
    let
        -- builderAssistantIdRegex : Regex
        -- builderAssistantIdRegex =
        --     Maybe.withDefault Regex.never <|
        --         Regex.fromString "^\\d{7}$"
        cmd : Cmd msg
        cmd =
            Http.request
                { method = "GET"
                , url =
                    Url.relative [ options.apiUrl, "resources/consumables" ]
                        [ Url.string "size" "10"
                        , Url.string "pageIndex" <| String.fromInt options.pageIndex

                        -- , if Regex.contains builderAssistantIdRegex options.filterString then
                        --     Url.string "builderAssistantId" options.filterString
                        --   else
                        --     Url.string "filterString" options.filterString
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
