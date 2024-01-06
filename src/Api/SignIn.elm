module Api.SignIn exposing (Error, post)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Http
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Shared.Model
import Url.Builder as Url


type alias User =
    { id : Int
    , name : Maybe String
    , role : String
    , email : String
    }


type alias Error =
    { message : String
    }


userDecoder : Decode.Decoder User
userDecoder =
    Decode.field "data"
        (Decode.succeed User
            |> Decode.andMap (Decode.field "id" Decode.int)
            |> Decode.andMap (Decode.optionalField "volunteer" (Decode.field "name" Decode.string))
            |> Decode.andMap (Decode.field "role" Decode.string)
            |> Decode.andMap (Decode.field "email" Decode.string)
        )


{-| Sends a POST request to our `/api/sign-in` endpoint, which
returns our JWT token if a user was found with that email
and password.
-}
post :
    { onResponse : Result (List Error) Shared.Model.AppUser -> msg
    , email : String
    , password : String
    , apiUrl : String
    }
    -> Effect msg
post { email, password, onResponse, apiUrl } =
    let
        body : Encode.Value
        body =
            Encode.object
                [ ( "email", Encode.string email )
                , ( "password", Encode.string password )
                ]

        cmd : Cmd msg
        cmd =
            Http.post
                { url = Url.relative [ apiUrl, "accounts/login" ] []
                , body = Http.jsonBody body
                , expect = Http.expectStringResponse onResponse handleHttpResponse
                }
    in
    Effect.sendCmd cmd



-- HTTP Custom Error Handling


getTokensFromHeaders : Dict String String -> Maybe Shared.Model.Tokens
getTokensFromHeaders headers =
    Maybe.map2 Shared.Model.Tokens
        (Dict.get "x-signature-token" headers)
        (Dict.get "x-header-payload-token" headers)


handleHttpResponse : Http.Response String -> Result (List Error) Shared.Model.AppUser
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

        Http.BadStatus_ { headers, statusCode } body ->
            case statusCode of
                -- if Forbidden 403 -> Redirect to EULA acceptance page
                403 ->
                    case getTokensFromHeaders headers of
                        Just tokens ->
                            Ok <| Shared.Model.NotEulaAccepted tokens

                        Nothing ->
                            Err
                                [ { message = "Got no `headers` from the login backend response!"
                                  }
                                ]

                _ ->
                    case Decode.decodeString errorsDecoder body of
                        Ok errors ->
                            Err errors

                        Err _ ->
                            Err
                                [ { message = "BadStatus: Something unexpected happened"
                                  }
                                ]

        Http.GoodStatus_ { headers } body ->
            case Decode.decodeString userDecoder body of
                Ok user ->
                    case getTokensFromHeaders headers of
                        Just tokens ->
                            Ok <|
                                Shared.Model.ValidatedUser <|
                                    Shared.Model.User
                                        tokens
                                        user.id
                                        user.name
                                        user.role
                                        user.email

                        Nothing ->
                            Err
                                [ { message = "Got no `headers` from the login backend response!"
                                  }
                                ]

                Err _ ->
                    Err
                        [ { message = "Something unexpected happened"
                          }
                        ]


errorsDecoder : Decode.Decoder (List Error)
errorsDecoder =
    Decode.field "data" (Decode.map List.singleton errorDecoder)


errorDecoder : Decode.Decoder Error
errorDecoder =
    Decode.map Error
        (Decode.field "message" Decode.string)
