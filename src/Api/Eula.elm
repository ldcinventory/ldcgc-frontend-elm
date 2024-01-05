module Api.Eula exposing (Data, errorToString, get, put)

import Effect exposing (Effect)
import Http exposing (Error(..))
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Encode as Encode


type Action
    = Accept
    | Pending
    | Reject
    | Remove
    | Delete


actionToString : Action -> String
actionToString action =
    case action of
        Accept ->
            "ACCEPT"

        Pending ->
            "PENDING"

        Reject ->
            "REJECT"

        Remove ->
            "REMOVE"

        Delete ->
            "DELETE"


actionDecoder : Decode.Decoder Action
actionDecoder =
    Decode.string
        |> Decode.andThen
            (\action ->
                case action of
                    "ACCEPT" ->
                        Decode.succeed Accept

                    "PENDING" ->
                        Decode.succeed Pending

                    "REJECT" ->
                        Decode.succeed Reject

                    "REMOVE" ->
                        Decode.succeed Remove

                    "DELETE" ->
                        Decode.succeed Delete

                    _ ->
                        Decode.fail "Unknown action"
            )


{-| The data we expect if the sign in attempt was successful.
-}
type alias Data =
    { message : String
    , docUrl : String
    , availableActions : List Action
    }


{-| How to create a `Data` value from JSON
-}
decoder : Decode.Decoder Data
decoder =
    Decode.succeed Data
        |> Decode.andMap (Decode.field "message" Decode.string)
        |> Decode.andMap (Decode.field "data" (Decode.field "url" Decode.string))
        |> Decode.andMap (Decode.field "data" (Decode.field "actionsAvailable" (Decode.list actionDecoder)))


errorDecoder : Decode.Decoder String
errorDecoder =
    Decode.field "message" Decode.string


{-| Sends a GET request to our `/api/eula` endpoint,
to notify the backend that the user has been signed out.
-}
get :
    { onResponse : Result Http.Error Data -> msg
    , signatureToken : String
    , headerPayloadToken : String
    , apiUrl : String
    }
    -> Effect msg
get options =
    let
        cmd : Cmd msg
        cmd =
            Http.request
                { method = "GET"
                , url = options.apiUrl ++ "/eula"
                , headers =
                    [ Http.header "x-signature-token" options.signatureToken
                    , Http.header "x-header-payload-token" options.headerPayloadToken
                    ]
                , body = Http.emptyBody
                , expect = Http.expectStringResponse options.onResponse handleHttpResponse
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


{-| Sends a PUT request to our `/api/eula` endpoint,
to notify the backend that the user has been signed out.
-}
put :
    { onResponse : Result Http.Error Data -> msg
    , signatureToken : String
    , headerPayloadToken : String
    , apiUrl : String
    , action : Action
    }
    -> Effect msg
put options =
    let
        cmd : Cmd msg
        cmd =
            Http.request
                { method = "PUT"
                , url = options.apiUrl ++ "/eula"
                , headers =
                    [ Http.header "x-signature-token" options.signatureToken
                    , Http.header "x-header-payload-token" options.headerPayloadToken
                    ]
                , body =
                    Http.jsonBody
                        (Encode.object
                            [ ( "action"
                              , Encode.string <| actionToString options.action
                              )
                            ]
                        )
                , expect = Http.expectStringResponse options.onResponse handleHttpResponse
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


handleHttpResponse : Http.Response String -> Result Http.Error Data
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
                Ok errors ->
                    Err <| BadBody errors

                Err _ ->
                    Err <| BadBody "Something unexpected happened"

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
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
