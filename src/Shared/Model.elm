module Shared.Model exposing (..)

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


type Role
    = Admin
    | UserRole
    | Manager


type alias User =
    { tokens : Tokens
    , id : Int
    , name : Maybe String
    , role : Role
    , email : String
    }


type alias Volunteers =
    List Volunteer


type alias Volunteer =
    { id : Int
    , name : String
    , lastName : String
    , builderAssistantId : String
    , isActive : Bool
    }
