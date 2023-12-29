module Auth exposing (User, onPageLoad, viewLoadingPage)

import Auth.Action
import Dict
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


type alias User =
    { signatureToken : String
    , headerPayloadToken : String
    }


{-| Called before an auth-only page is loaded.
-}
onPageLoad : Shared.Model -> Route () -> Auth.Action.Action User
onPageLoad shared route =
    case ( shared.signatureToken, shared.headerPayloadToken ) of
        ( Just signatureToken, Just headerPayloadToken ) ->
            Auth.Action.loadPageWithUser
                { signatureToken = signatureToken
                , headerPayloadToken = headerPayloadToken
                }

        _ ->
            Auth.Action.pushRoute
                { path = Route.Path.SignIn
                , query = Dict.fromList [ ( "from", route.url.path ) ]
                , hash = Nothing
                }


{-| Renders whenever `Auth.Action.showLoadingPage` is returned from `onPageLoad`.
-}
viewLoadingPage : Shared.Model -> Route () -> View Never
viewLoadingPage _ _ =
    View.fromString "Loading..."
