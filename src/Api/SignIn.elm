module Api.SignIn exposing (Data, Error, post)

import Effect exposing (Effect)
import Http
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Encode as Encode


{-| The data we expect if the sign in attempt was successful.
-}
type alias Data =
    { signatureToken : String
    , headerPayloadToken : String
    }


type alias Error =
    { message : String
    }


{-| How to create a `Data` value from JSON
-}
decoder : Decode.Decoder Data
decoder =
    Decode.succeed Data
        |> Decode.andMap (Decode.field "x-signature-token" Decode.string)
        |> Decode.andMap (Decode.field "x-header-payload-token" Decode.string)


{-| Sends a POST request to our `/api/sign-in` endpoint, which
returns our JWT token if a user was found with that email
and password.
-}
post :
    { onResponse : Result (List Error) Data -> msg
    , email : String
    , password : String
    }
    -> Effect msg
post { email, password, onResponse } =
    let
        body : Encode.Value
        body =
            Encode.object
                [ ( "email", Encode.string email )
                , ( "password", Encode.string password )
                ]

        cmd : Cmd msg
        cmd =
            Http.request
                { method = "POST"
                , headers = []

                -- FIXME: Skipping EULA for now...
                -- , headers = [ Http.header "skip-eula" "true" ]
                , url = "http://localhost:8080/api/accounts/login"
                , body = Http.jsonBody body
                , expect = Http.expectStringResponse onResponse handleHttpResponse
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd



-- HTTP Custom Error Handling


handleHttpResponse : Http.Response String -> Result (List Error) Data
handleHttpResponse response =
    case response of
        Http.BadUrl_ _ ->
            Err
                [ { message = "Unexpected URL format"
                  }
                ]

        Http.Timeout_ ->
            Err
                [ { message = "Request timed out, please try again"
                  }
                ]

        Http.NetworkError_ ->
            Err
                [ { message = "Could not connect, please try again"
                  }
                ]

        Http.BadStatus_ {- statusCode -} _ body ->
            case Decode.decodeString errorsDecoder body of
                Ok errors ->
                    Err errors

                Err _ ->
                    Err
                        [ { message = "Something unexpected happened"
                          }
                        ]

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Ok data ->
                    Ok data

                Err _ ->
                    Err
                        [ { message = "Something unexpected happened decoding good status JSON"
                          }
                        ]


errorsDecoder : Decode.Decoder (List Error)
errorsDecoder =
    Decode.field "data" (Decode.map List.singleton errorDecoder)


errorDecoder : Decode.Decoder Error
errorDecoder =
    Decode.map Error
        (Decode.field "message" Decode.string)
