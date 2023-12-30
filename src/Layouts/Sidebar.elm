module Layouts.Sidebar exposing (Model, Msg, Props, layout)

import Auth
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path
import Shared
import Tailwind.Utilities as Tw
import View exposing (View)


type alias Props =
    { title : String
    , user : Auth.User
    }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props _ route =
    Layout.new
        { init = init
        , update = update
        , view = view props route
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view :
    Props
    -> Route ()
    ->
        { toContentMsg : Msg -> contentMsg
        , content : View contentMsg
        , model : Model
        }
    -> View contentMsg
view props route { toContentMsg, model, content } =
    { title = content.title ++ " | LDC GC"
    , body =
        [ Html.div [ Attr.css [ Tw.flex, Tw.h_screen ] ]
            [ viewSidebar
                { user = props.user
                , route = route
                }
            , viewMainContent
                { title = props.title
                , content = content
                }
            ]
        ]
    }


viewSidebar : { user : Auth.User, route : Route () } -> Html msg
viewSidebar { user, route } =
    Html.aside
        [ Attr.class "is-flex is-flex-direction-column p-2"
        , Attr.style "min-width" "200px"
        , Attr.style "border-right" "solid 1px #eee"
        ]
        [ viewAppNameAndLogo
        , viewSidebarLinks route
        , viewSignOutButton user
        ]


viewAppNameAndLogo : Html msg
viewAppNameAndLogo =
    Html.div [ Attr.class "is-flex p-3" ]
        [ Html.figure []
            [ Html.img
                [ Attr.src "https://bulma.io/images/placeholders/24x24.png"
                , Attr.alt "My Cool App's logo"
                ]
                []
            ]
        , Html.span [ Attr.class "has-text-weight-bold pl-2" ]
            [ Html.text "My Cool App" ]
        ]


viewSidebarLinks : Route () -> Html msg
viewSidebarLinks route =
    let
        viewSidebarLink : ( String, Route.Path.Path ) -> Html msg
        viewSidebarLink ( label, path ) =
            Html.li []
                [ Html.a
                    [ Attr.fromUnstyled <| Route.Path.href path
                    , Attr.classList
                        [ ( "is-active", route.path == path )
                        ]
                    ]
                    [ Html.text label ]
                ]
    in
    Html.div [ Attr.class "menu is-flex-grow-1" ]
        [ Html.ul [ Attr.class "menu-list" ]
            (List.map viewSidebarLink
                [ ( "Dashboard", Route.Path.Home_ )
                , ( "Volunteers", Route.Path.Volunteers )
                , ( "Tools", Route.Path.Tools )
                ]
            )
        ]


viewSignOutButton : Auth.User -> Html msg
viewSignOutButton user =
    Html.button [ Attr.class "button is-text is-fullwidth" ]
        [ Html.div [ Attr.class "is-flex is-align-items-center" ]
            [ Html.figure [ Attr.class "image is-24x24" ]
                [ Html.text user.email
                ]
            , Html.span [ Attr.class "pl-2" ] [ Html.text "Sign out" ]
            ]
        ]


viewMainContent : { title : String, content : View msg } -> Html msg
viewMainContent { title, content } =
    Html.main_ [ Attr.class "is-flex is-flex-direction-column is-flex-grow-1" ]
        [ Html.section [ Attr.class "hero is-info" ]
            [ Html.div [ Attr.class "hero-body" ]
                [ Html.h1 [ Attr.class "title" ] [ Html.text title ]
                ]
            ]
        , Html.div [ Attr.class "p-4" ] content.body
        ]
