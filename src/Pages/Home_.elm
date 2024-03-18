module Pages.Home_ exposing (Model, Msg, page)

import Auth
import Effect exposing (Effect)
import Html
import Html.Attributes as Attr
import Jwt
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Time
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init user
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (toLayout user)


toLayout : Auth.User -> Model -> Layouts.Layout msg
toLayout user model =
    Layouts.Sidebar
        { title = "Dashboard"
        , user = user
        }



-- INIT


type alias Model =
    {}


init : Auth.User -> () -> ( Model, Effect Msg )
init user () =
    let
        _ =
            -- FIXME: delete this debug log when we actually read the expiry of the token
            Debug.log "expiry" << Result.map Time.millisToPosix << Jwt.getTokenExpirationMillis <| user.tokens.headerPayloadToken ++ "." ++ user.tokens.signatureToken
    in
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Dashboard"
    , body =
        [ Html.div
            [ Attr.class "text-4xl text-center mt-8 font-barcode"
            ]
            [ Html.text "Hello, world!"
            ]
        ]
    }
