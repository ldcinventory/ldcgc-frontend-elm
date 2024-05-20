module Shared.Msg exposing (Msg(..))

import Components.Toast as To
import Shared.Model
import Time
import Toast


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type Msg
    = SignIn Shared.Model.User
    | SignOut
    | SetCurrentTime Time.Posix
    | ToastMsg Toast.Msg
    | AddToast String To.ToastType
