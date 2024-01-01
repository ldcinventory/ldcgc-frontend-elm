module Shared.Model exposing (Model, User)

{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}


type alias Model =
    { user : Maybe User
    , apiUrl : String
    }


type alias User =
    { signatureToken : String
    , headerPayloadToken : String
    , id : Int

    -- FIXME: , name : String
    , role : String
    , email : String
    }
