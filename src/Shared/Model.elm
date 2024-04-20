module Shared.Model exposing (..)

import Components.Toast as To
import Set.Any exposing (AnySet)
import Time exposing (Weekday)
import Toast


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type alias Model =
    { user : Maybe User
    , apiUrl : String
    , tray : Toast.Tray To.Toast
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
    { numVolunteers : Int
    , totalPages : Int
    , elementsThisPage : Int
    , list : List Volunteer
    }


type alias Volunteer =
    { id : Int
    , name : String
    , lastName : String
    , builderAssistantId : String
    , isActive : Bool
    }


type alias Absence =
    { id : Int
    , dateFrom : String
    , dateTo : String
    , builderAssistantId : String
    }


type alias VolunteerDetail =
    { id : Int
    , name : String
    , lastName : String
    , builderAssistantId : String
    , isActive : Bool
    , absences : List Absence
    , availability : AnySet String Weekday
    }
