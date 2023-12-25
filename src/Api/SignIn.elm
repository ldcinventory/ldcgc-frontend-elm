module Api.SignIn exposing (Data, post)

import Effect exposing (Effect)
import Http
import Json.Decode as Decode
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
    Decode.map2 Data
        (Decode.field "x-signature-token" Decode.string)
        (Decode.field "x-header-payload-token" Decode.string)


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
post options =
    let
        body : Encode.Value
        body =
            Encode.object
                [ ( "email", Encode.string options.email )
                , ( "password", Encode.string options.password )
                ]

        cmd : Cmd msg
        cmd =
            Http.post
                { url = "http://localhost:8080/api/account/login"
                , body = Http.jsonBody body
                , expect = Http.expectJson options.onResponse decoder
                }
    in
    Effect.sendCmd cmd
