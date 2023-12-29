module Api.Me exposing (User, get)

import Effect exposing (Effect)
import Http
import Json.Decode as Decode
import Json.Decode.Extra as Decode


type alias User =
    { id : String

    -- , name : String FIXME: name is nested in volunteer.name/lastName!
    , role : String
    , email : String
    }


decoder : Decode.Decoder User
decoder =
    Decode.succeed User
        |> Decode.andMap (Decode.field "id" Decode.string)
        -- (Decode. "name" Decode.string)
        |> Decode.andMap (Decode.field "role" Decode.string)
        |> Decode.andMap (Decode.field "email" Decode.string)


get :
    { onResponse : Result Http.Error User -> msg
    , signatureToken : String
    , headerPayloadToken : String
    }
    -> Effect msg
get options =
    Effect.sendCmd <|
        Http.request
            { method = "GET"
            , headers =
                [ Http.header "x-signature-token" options.signatureToken
                , Http.header "x-header-payload-token" options.headerPayloadToken
                , Http.header "skip-eula" "true"
                ]
            , url = "http://localhost:8080/api/users/me"
            , body = Http.emptyBody
            , expect = Http.expectJson options.onResponse decoder
            , timeout = Nothing
            , tracker = Nothing
            }
