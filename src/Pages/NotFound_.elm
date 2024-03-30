module Pages.NotFound_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html
import Html.Attributes as Attr
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
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
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view _ =
    { title = "Page not found"
    , body =
        [ Html.section
            [ Attr.class "bg-white dark:bg-gray-900"
            ]
            [ Html.div
                [ Attr.class "h-screen py-8 px-4 mx-auto max-w-screen-xl lg:py-16 lg:px-6"
                ]
                [ Html.div
                    [ Attr.class "mx-auto max-w-screen-sm text-center"
                    ]
                    [ Html.h1
                        [ Attr.class "mb-4 text-7xl tracking-tight font-extrabold lg:text-9xl text-primary-600 dark:text-primary-500"
                        ]
                        [ Html.text "404" ]
                    , Html.p
                        [ Attr.class "mb-4 text-3xl tracking-tight font-bold text-gray-900 md:text-4xl dark:text-white"
                        ]
                        [ Html.text "Something's missing." ]
                    , Html.p
                        [ Attr.class "mb-4 text-lg font-light text-gray-500 dark:text-gray-400"
                        ]
                        [ Html.text "Sorry, we can't find that page. You'll find lots to explore on the home page." ]
                    , Html.a
                        [ Route.Path.href <| Route.Path.Home_
                        , Attr.class "inline-flex text-white bg-primary-600 hover:bg-primary-800 focus:ring-4 focus:outline-none focus:ring-primary-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center dark:focus:ring-primary-900 my-4"
                        ]
                        [ Html.text "Back to Homepage" ]
                    ]
                ]
            ]
        ]
    }
