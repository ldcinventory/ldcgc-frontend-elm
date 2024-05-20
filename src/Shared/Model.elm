module Shared.Model exposing (..)

import Components.Toast as To
import Set.Any exposing (AnySet)
import Time exposing (Posix, Weekday)
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
    , time : Posix
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


type alias Paginator a =
    { numItems : Int
    , totalPages : Int
    , elementsThisPage : Int
    , list : List a
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


type alias Brand =
    { id : Int
    , name : String
    , locked : Bool
    }


type alias ResourceType =
    { id : Int
    , name : String
    , locked : Bool
    }


type alias Location =
    { id : Int
    , name : String
    , description : String

    -- , parent : Location
    -- , locations : List Location
    }


type alias Group =
    { id : Int
    , name : String
    , description : Maybe String
    , urlImage : Maybe String
    , phoneNumber : String
    , location : Location
    }


type alias Consumable =
    { id : Int
    , barcode : String
    , resourceType : ResourceType
    , brand : Brand
    , price : Float
    , purchaseDate : Posix
    , name : String
    , model : String
    , description : String
    , urlImages : List String
    , quantityEachItem : Float
    , stock : Float
    , minStock : Float
    , location : Location
    , group : Group
    }


type alias ConsumableRegister =
    { id : Int
    , consumableBarcode : String
    , consumableName : String
    , consumableUrlImages : List String
    , volunteerBuilderAssistantId : String
    , volunteerName : String
    , volunteerLastName : String
    , stockAmountRequest : Float
    , stockAmountReturn : Maybe Float
    , consumableStockType : String
    , registerFrom : Posix
    , registerTo : Maybe Posix
    , closedRegister : Bool
    , processingStockChanges : Bool
    }


type alias ToolRegister =
    { id : Int
    , toolBarcode : String
    , toolName : String
    , toolUrlImages : List String
    , volunteerBuilderAssistantId : String
    , volunteerName : String
    , volunteerLastName : String
    , registerFrom : Posix
    , registerTo : Maybe Posix
    }
