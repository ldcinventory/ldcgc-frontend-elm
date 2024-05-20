module Layouts.Sidebar exposing (Model, Msg, Props, layout)

import Api.SignOut
import Auth
import Browser.Events
import Components.Dropdown as Dropdown
import Components.Icons as Icon
import Components.Toast as To
import Effect exposing (Effect)
import Html exposing (Html, a, aside, button, div, li, span, text, ul)
import Html.Attributes as Attr
import Html.Attributes.Extra as Attr
import Html.Events as Events
import Html.Extra as Html
import Http exposing (Error(..))
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path
import Shared
import Svg.Attributes as SvgAttr
import Toast
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
        , view = view props shared route
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { errors : List Http.Error
    , isSidebarOpen : Bool
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { errors = []
      , isSidebarOpen = False
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = UserClickedSignOut
    | UpdateToastMsg Toast.Msg
    | OpenSidebar
    | OnClickOutside
    | SignOutApiResponded (Result Http.Error Api.SignOut.Data)


update : Props -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update props shared msg model =
    case msg of
        OpenSidebar ->
            ( { model | isSidebarOpen = True }
            , Effect.none
            )

        OnClickOutside ->
            ( { model | isSidebarOpen = False }
            , Effect.none
            )

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

        UpdateToastMsg toastMsg ->
            ( model, Effect.sendToastMsg toastMsg )


subscriptions : Model -> Sub Msg
subscriptions { isSidebarOpen } =
    if isSidebarOpen then
        Browser.Events.onMouseDown (Dropdown.outsideTarget OnClickOutside "sidebar-toggle")

    else
        Sub.none



-- VIEW


viewNotification : Shared.Model -> Html Msg
viewNotification model =
    Html.div [ Attr.class "absolute z-40 p-4 sm:ml-64 h-full" ]
        [ Toast.render
            (To.view (UpdateToastMsg << Toast.remove << .id))
            model.tray
            (Toast.config UpdateToastMsg)
        ]


view :
    Props
    -> Shared.Model
    -> Route ()
    ->
        { toContentMsg : Msg -> contentMsg
        , content : View contentMsg
        , model : Model
        }
    -> View contentMsg
view props shared _ { toContentMsg, content, model } =
    { title = content.title ++ " | LDC GC"
    , body =
        [ Html.div
            [ Attr.class "flex h-screen bg-color-gray-50 dark:bg-gray-900 dark:text-white" ]
            [ viewSidebar model
                |> Html.map toContentMsg
            , viewMainContent
                { title = props.title
                , content = content
                }
            , viewNotification shared
                |> Html.map toContentMsg
            , Html.div [ Attr.class "text-red-500" ] <|
                List.map
                    (Api.SignOut.errorToString >> Html.text)
                    model.errors
            ]
        ]
    }


viewSidebar : Model -> Html Msg
viewSidebar model =
    Html.div
        [ Attr.class "absolute ml-0" ]
        [ button
            [ Attr.attribute "data-drawer-target" "default-sidebar"
            , Attr.attribute "data-drawer-toggle" "default-sidebar"
            , Attr.attribute "aria-controls" "default-sidebar"
            , Attr.id "sidebar-toggle"
            , Attr.type_ "button"
            , Attr.class "inline-flex items-center p-2 mt-2 ms-3 text-sm text-gray-500 rounded-lg sm:hidden hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-gray-200 dark:text-gray-400 dark:hover:bg-gray-700 dark:focus:ring-gray-600"
            , Events.onClick OpenSidebar
            ]
            [ span
                [ Attr.class "sr-only"
                ]
                [ text "Open sidebar" ]
            , Icon.sidebar
            ]
        , aside
            [ Attr.id "default-sidebar"
            , Attr.class "fixed top-0 left-0 z-40 w-64 h-screen transition-transform sm:translate-x-0"
            , Attr.attribute "aria-label" "Sidebar"
            , Attr.attributeIf model.isSidebarOpen <| Attr.attribute "role" "dialog"
            , Attr.classList
                [ ( "-translate-x-full", not model.isSidebarOpen )
                , ( "transform-none", model.isSidebarOpen )
                ]
            , if model.isSidebarOpen then
                Attr.attribute "aria-modal" "true"

              else
                Attr.attribute "aria-hidden" "true"
            ]
            [ div
                [ Attr.class "h-full px-3 py-4 overflow-y-auto bg-gray-50 dark:bg-gray-800"
                ]
                [ ul
                    [ Attr.class "space-y-2 font-medium"
                    ]
                    [ li []
                        [ a
                            [ Route.Path.href <| Route.Path.Home_
                            , Attr.class "flex items-center p-2 text-gray-900 rounded-lg dark:text-white hover:bg-gray-100 dark:hover:bg-gray-700 group"
                            ]
                            [ Icon.dashboard
                            , span
                                [ Attr.class "ms-3"
                                ]
                                [ text "Dashboard" ]
                            ]
                        ]
                    , li []
                        [ a
                            [ Route.Path.href <| Route.Path.Tools
                            , Attr.class "flex items-center p-2 text-gray-900 rounded-lg dark:text-white hover:bg-gray-100 dark:hover:bg-gray-700 group"
                            ]
                            [ Icon.tools [ SvgAttr.class "text-gray-400 dark:text-gray-400" ]
                            , span
                                [ Attr.class "flex-1 ms-3 whitespace-nowrap"
                                ]
                                [ text "Tools" ]
                            ]
                        ]
                    , li []
                        [ a
                            [ Route.Path.href <| Route.Path.Consumables
                            , Attr.class "flex items-center p-2 text-gray-900 rounded-lg dark:text-white hover:bg-gray-100 dark:hover:bg-gray-700 group"
                            ]
                            [ Icon.clock [ SvgAttr.class "text-gray-400 dark:text-gray-400" ]
                            , span
                                [ Attr.class "flex-1 ms-3 whitespace-nowrap"
                                ]
                                [ text "Consumables" ]
                            ]
                        ]
                    , li []
                        [ a
                            [ Route.Path.href <| Route.Path.Registers_Consumables
                            , Attr.class "flex items-center p-2 text-gray-900 rounded-lg dark:text-white hover:bg-gray-100 dark:hover:bg-gray-700 group"
                            ]
                            [ Icon.register
                            , span
                                [ Attr.class "flex-1 ms-3 whitespace-nowrap"
                                ]
                                [ text "Registers" ]
                            ]
                        ]
                    , li []
                        [ a
                            [ Route.Path.href <| Route.Path.Volunteers
                            , Attr.class "flex items-center p-2 text-gray-900 rounded-lg dark:text-white hover:bg-gray-100 dark:hover:bg-gray-700 group"
                            ]
                            [ Icon.volunteers
                            , span
                                [ Attr.class "flex-1 ms-3 whitespace-nowrap"
                                ]
                                [ text "Volunteers" ]
                            ]
                        ]
                    , li []
                        [ a
                            [ Events.onClick UserClickedSignOut
                            , Attr.href "#"
                            , Attr.class "flex cursor-pointer items-center p-2 text-gray-900 rounded-lg dark:text-white hover:bg-gray-100 dark:hover:bg-gray-700 group"
                            ]
                            [ Icon.signOut
                            , span
                                [ Attr.class "flex-1 ms-3 whitespace-nowrap"
                                ]
                                [ text "Sign Out" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewMainContent : { title : String, content : View msg } -> Html msg
viewMainContent { title, content } =
    Html.main_
        [ Attr.class "flex grow flex-col"
        ]
        [ Html.section
            [ Attr.class "p-4 pl-14 sm:ml-0 md:ml-64" ]
            [ Html.h1
                [ Attr.class "font-extrabold text-2xl" ]
                [ Html.text title ]
            ]
        , Html.div [ Attr.class "p-0 sm:p-4 sm:ml-0 md:ml-64 h-full" ] content.body
        ]
