module Api.SignOut exposing (Data, errorToString, post)

import Effect exposing (Effect)
import Http exposing (Error(..))
import Json.Decode as Decode


{-| The data we expect if the sign in attempt was successful.
-}
type alias Data =
    String


{-| How to create a `Data` value from JSON
-}
decoder : Decode.Decoder Data
decoder =
    Decode.field "message" Decode.string


{-| Sends a POST request to our `/api/accounts/logout` endpoint,
to notify the backend that the user has been signed out.
-}
post :
    { onResponse : Result Http.Error Data -> msg
    , signatureToken : String
    , headerPayloadToken : String
    , apiUrl : String
    }
    -> Effect msg
post options =
    let
        cmd : Cmd msg
        cmd =
            Http.request
                { method = "POST"
                , url = options.apiUrl ++ "/accounts/logout"
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
            case Decode.decodeString decoder body of
                Ok errors ->
                    Err <| BadBody errors

                Err _ ->
                    Err <| BadBody "Something unexpected happened"

        Http.GoodStatus_ { headers } body ->
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
