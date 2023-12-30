module Layouts.Sidebar exposing (Model, Msg, Props, layout)

import Auth
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path
import Shared
import Tailwind.Theme as Tw
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
        [ Attr.css [ Tw.flex, Tw.flex_col, Tw.p_2, Tw.border_r, Tw.border_color Tw.gray_200 ]
        , Attr.style "min-width" "200px"
        ]
        [ viewAppNameAndLogo
        , viewSidebarLinks route
        , viewSignOutButton user
        ]


viewAppNameAndLogo : Html msg
viewAppNameAndLogo =
    Html.div [ Attr.css [ Tw.flex, Tw.p_3, Tw.flex_col, Tw.items_center ] ]
        [ Html.figure []
            [ Html.img
                [ Attr.src "/logo.png"
                , Attr.alt "LDC GC Logo"
                , Attr.width 100
                ]
                []
            ]
        , Html.span
            [ Attr.css [ Tw.font_bold, Tw.pl_2 ] ]
            [ Html.text "LDC GC" ]
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
                        [ ( "font-bold", route.path == path )
                        ]
                    ]
                    [ Html.text label ]
                ]
    in
    Html.div [ Attr.css [ Tw.flex, Tw.grow ] ]
        [ Html.ul [ Attr.css [ Tw.list_none ] ]
            (List.map viewSidebarLink
                [ ( "Dashboard", Route.Path.Home_ )
                , ( "Volunteers", Route.Path.Volunteers )
                , ( "Tools", Route.Path.Tools )
                ]
            )
        ]


viewSignOutButton : Auth.User -> Html msg
viewSignOutButton user =
    Html.button [ Attr.css [ Tw.w_full ] ]
        [ Html.div [ Attr.css [ Tw.flex, Tw.items_center ] ]
            [ Html.div [] [ Html.text user.email ]
            , Html.span [ Attr.css [ Tw.pl_2 ] ] [ Html.text "Sign out" ]
            ]
        ]


viewMainContent : { title : String, content : View msg } -> Html msg
viewMainContent { title, content } =
    Html.main_
        [ Attr.css
            [ Tw.flex, Tw.flex_col, Tw.grow ]
        ]
        [ Html.section
            [ Attr.css [ Tw.p_4 ] ]
            [ Html.div
                [ Attr.css [ Tw.font_extrabold, Tw.text_2xl ] ]
                [ Html.text title ]
            ]
        , Html.div [ Attr.css [ Tw.p_4 ] ] content.body
        ]
