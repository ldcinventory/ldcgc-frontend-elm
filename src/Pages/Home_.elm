module Pages.Home_ exposing (Model, Msg, page)

import Auth
import Effect exposing (Effect)
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Tailwind.Utilities as Tw
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init
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


init : () -> ( Model, Effect Msg )
init () =
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
    { title = "LDC GC Homepage"
    , body =
        [ Html.div
            [ Attr.style "font-family" "'Libre Barcode 128'"
            , Attr.css [ Tw.text_4xl, Tw.text_center, Tw.mt_8 ]
            ]
            [ Html.text "Hello, world!"
            ]
        ]
    }
