module Auth exposing (User, onPageLoad, viewLoadingPage)

import Auth.Action
import Dict
import Jwt
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model
import Time
import View exposing (View)


type alias User =
    Shared.Model.User


redirectToSignIn : Route () -> Auth.Action.Action user
redirectToSignIn { url } =
    Auth.Action.pushRoute
        { path = Route.Path.SignIn
        , query = Dict.fromList [ ( "from", url.path ) ]
        , hash = Nothing
        }


{-| Called before an auth-only page is loaded.
-}
onPageLoad : Shared.Model -> Route () -> Auth.Action.Action User
onPageLoad shared route =
    case shared.user of
        Just user ->
            let
                expiry : Result Jwt.JwtError Time.Posix
                expiry =
                    Result.map Time.millisToPosix << Jwt.getTokenExpirationMillis <| user.tokens.headerPayloadToken ++ "." ++ user.tokens.signatureToken
            in
            case expiry of
                Ok expiryTime ->
                    if Time.posixToMillis expiryTime > Time.posixToMillis shared.time then
                        Auth.Action.loadPageWithUser user

                    else
                        -- If the token has expired, redirect to the sign-in page.
                        redirectToSignIn route

                Err _ ->
                    redirectToSignIn route

        Nothing ->
            redirectToSignIn route


{-| Renders whenever `Auth.Action.showLoadingPage` is returned from `onPageLoad`.
-}
viewLoadingPage : Shared.Model -> Route () -> View Never
viewLoadingPage _ _ =
    View.fromString "Loading..."
