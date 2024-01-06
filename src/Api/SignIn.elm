module Api.SignIn exposing (Action(..), Error, EulaData, errorToString, getEula, post, putEula)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Http exposing (Error(..))
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Shared.Model
import Url.Builder as Url


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
type alias EulaData =
    { message : String
    , docUrl : String
    , availableActions : List Action
    }


{-| How to create a `Data` value from JSON
-}
eulaDecoder : Decode.Decoder EulaData
eulaDecoder =
    Decode.succeed EulaData
        |> Decode.andMap (Decode.field "message" Decode.string)
        |> Decode.andMap (Decode.field "data" (Decode.field "url" Decode.string))
        |> Decode.andMap (Decode.field "data" (Decode.field "actionsAvailable" (Decode.list actionDecoder)))


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


{-| Sends a GET request to our `/api/eula` endpoint,
to notify the backend that the user has been signed out.
-}
getEula :
    { onResponse : Result Http.Error EulaData -> msg
    , tokens : Shared.Model.Tokens
    , apiUrl : String
    }
    -> Effect msg
getEula options =
    let
        cmd : Cmd msg
        cmd =
            Http.request
                { method = "GET"
                , url = Url.relative [ options.apiUrl, "eula" ] []
                , headers =
                    [ Http.header "x-signature-token" options.tokens.signatureToken
                    , Http.header "x-header-payload-token" options.tokens.headerPayloadToken
                    ]
                , body = Http.emptyBody
                , expect = Http.expectJson options.onResponse eulaDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    Effect.sendCmd cmd


{-| Sends a PUT request to our `/api/eula` endpoint,
to notify the backend that the user has been signed out.
-}
putEula :
    { onResponse : Result Http.Error String -> msg
    , tokens : Shared.Model.Tokens
    , apiUrl : String
    , action : Action
    }
    -> Effect msg
putEula options =
    let
        cmd : Cmd msg
        cmd =
            Http.request
                { method = "PUT"
                , url =
                    Url.relative
                        [ options.apiUrl, "eula" ]
                        [ Url.string "action" <| actionToString options.action ]
                , headers =
                    [ Http.header "x-signature-token" options.tokens.signatureToken
                    , Http.header "x-header-payload-token" options.tokens.headerPayloadToken
                    ]
                , body = Http.emptyBody
                , expect = Http.expectJson options.onResponse (Decode.field "message" Decode.string)
                , timeout = Nothing
                , tracker = Nothing
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
