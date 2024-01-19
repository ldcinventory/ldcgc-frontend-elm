module Api.Volunteers exposing (get)

import Effect exposing (Effect)
import Http exposing (Error(..))
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Shared.Model exposing (Volunteer, Volunteers)
import Url.Builder as Url


decoder : Decode.Decoder Volunteers
decoder =
    Decode.field "data" <|
        Decode.list volunteerDecoder


volunteerDecoder : Decode.Decoder Volunteer
volunteerDecoder =
    Decode.succeed Volunteer
        |> Decode.andMap (Decode.field "id" Decode.int)
        |> Decode.andMap (Decode.field "name" Decode.string)
        |> Decode.andMap (Decode.field "lastName" Decode.string)
        |> Decode.andMap (Decode.field "builderAssistantId" Decode.string)
        |> Decode.andMap (Decode.field "isActive" Decode.bool)


get :
    { onResponse : Result Http.Error Volunteers -> msg
    , tokens : Shared.Model.Tokens
    , apiUrl : String
    , pageIndex : Int
    }
    -> Effect msg
get options =
    let
        cmd : Cmd msg
        cmd =
            Http.request
                { method = "GET"
                , url =
                    Url.relative [ options.apiUrl, "volunteers" ]
                        [ Url.string "size" "10"
                        , Url.string "pageIndex" <| String.fromInt options.pageIndex
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
