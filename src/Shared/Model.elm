module Shared.Model exposing (AppUser(..), Model, Tokens, User)

{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}


type alias Model =
    { user : Maybe User
    , apiUrl : String
    }


type AppUser
    = NotEulaAccepted Tokens
    | ValidatedUser User


type alias Tokens =
    { signatureToken : String
    , headerPayloadToken : String
    }


type alias User =
    { tokens : Tokens
    , id : Int
    , name : Maybe String
    , role : String
    , email : String
    }
