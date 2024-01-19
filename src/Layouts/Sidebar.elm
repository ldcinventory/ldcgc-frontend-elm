module Layouts.Sidebar exposing (Model, Msg, Props, layout)

import Api.SignOut
import Auth
import Components.Button as Button
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Extra as Html
import Http exposing (Error(..))
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


type alias Props =
    { title : String
    , user : Auth.User
    }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init
        , update = update props shared
        , view = view props route
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { errors : List Http.Error
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { errors = []
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = UserClickedSignOut
    | SignOutApiResponded (Result Http.Error Api.SignOut.Data)


update : Props -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update props shared msg model =
    case msg of
        UserClickedSignOut ->
            ( model
            , Api.SignOut.post
                { onResponse = SignOutApiResponded
                , tokens = props.user.tokens
                , apiUrl = shared.apiUrl
                }
            )

        SignOutApiResponded (Ok _) ->
            ( model
            , Effect.signOut
            )

        SignOutApiResponded (Err httpError) ->
            ( { model | errors = httpError :: model.errors }
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
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
view props route { toContentMsg, content, model } =
    { title = content.title ++ " | LDC GC"
    , body =
        [ Html.div
            [ Attr.class "flex h-screen bg-color-gray-50 dark:bg-gray-900 dark:text-white" ]
            [ viewSidebar
                { user = props.user
                , route = route
                }
                |> Html.map toContentMsg
            , viewMainContent
                { title = props.title
                , content = content
                }
            , Html.div [ Attr.class "text-red-500" ] <|
                List.map
                    (Api.SignOut.errorToString >> Html.text)
                    model.errors
            ]
        ]
    }


viewSidebar : { user : Auth.User, route : Route () } -> Html Msg
viewSidebar { user, route } =
    Html.aside
        [ Attr.class "flex flex-col p-2 border-r border-gray-200 min-w-[200px] dark:border dark:border-gray-700"
        ]
        [ viewAppNameAndLogo
        , viewSidebarLinks route
        , viewSignOutButton user
        ]


viewAppNameAndLogo : Html msg
viewAppNameAndLogo =
    Html.div
        [ Attr.class "flex items-center p-3 flex-col"
        ]
        [ Html.figure []
            [ Html.img
                [ Attr.src "/logo.png"
                , Attr.alt "LDC GC Logo"
                , Attr.width 100
                ]
                []
            ]
        , Html.span
            [ Attr.class "font-bold pl-2" ]
            [ Html.text "LDC GC" ]
        ]


viewSidebarLinks : Route () -> Html msg
viewSidebarLinks route =
    let
        viewSidebarLink : ( String, Route.Path.Path ) -> Html msg
        viewSidebarLink ( label, path ) =
            Html.li []
                [ Html.a
                    [ Route.Path.href path
                    , Attr.classList
                        [ ( "font-bold", route.path == path )
                        ]
                    ]
                    [ Html.text label ]
                ]
    in
    Html.div [ Attr.class "flex grow" ]
        [ Html.ul
            [ Attr.class "list-none" ]
            (List.map viewSidebarLink
                [ ( "Dashboard", Route.Path.Home_ )
                , ( "Volunteers", Route.Path.Volunteers )
                , ( "Tools", Route.Path.Tools )
                ]
            )
        ]


viewSignOutButton : Auth.User -> Html Msg
viewSignOutButton user =
    Html.div
        [ Attr.class "w-full" ]
        [ Html.div [ Attr.class "flex items-center flex-col gap-2" ]
            [ user.name
                |> Html.viewMaybe
                    (\name ->
                        Html.div [] [ Html.text <| "Welcome, " ++ name ++ "! 👋🏻" ]
                    )
            , Html.div [] [ Html.text user.email ]
            , Button.primary
                { disabled = False
                , onClick = Just UserClickedSignOut
                , content = Html.text "Sign out"
                , attrs = [ Attr.attribute "data-test" "signout-button" ]
                }
            ]
        ]


viewMainContent : { title : String, content : View msg } -> Html msg
viewMainContent { title, content } =
    Html.main_
        [ Attr.class "flex grow flex-col"
        ]
        [ Html.section
            [ Attr.class "p-4" ]
            [ Html.div
                [ Attr.class "font-extrabold text-2xl" ]
                [ Html.text title ]
            ]
        , Html.div [ Attr.class "p-4 h-full" ] content.body
        ]
