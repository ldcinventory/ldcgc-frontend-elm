module Api.SignIn exposing (Data, post)

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
    { onResponse : Result Http.Error Data -> msg
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
            Http.post
                { url = "http://localhost:8080/api/accounts/login"
                , body = Http.jsonBody body
                , expect = Http.expectJson onResponse decoder
                }
    in
    Effect.sendCmd cmd
