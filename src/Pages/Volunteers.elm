module Pages.Volunteers exposing (Model, Msg, page)

import Api.Volunteers
import Auth
import Browser.Events
import Components.Button as Button
import Components.Dropdown as Dropdown
import Components.Icons as Icon
import Components.Pagination as Pagination
import Components.Spinner as Spinner
import Components.Toast as To
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Extra as Html
import Http
import Layouts
import List.Extra as List
import Page exposing (Page)
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model exposing (Role(..), Volunteer, Volunteers)
import Task
import Toast
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared _ =
    Page.new
        { init = init user shared
        , update = update user shared
        , subscriptions = subscriptions
        , view = view user
        }
        |> Page.withLayout (toLayout user)


toLayout : Auth.User -> Model -> Layouts.Layout msg
toLayout user _ =
    Layouts.Sidebar
        { title = "Volunteers"
        , user = user
        }



-- INIT


type alias Model =
    { volunteers : WebData Volunteers
    , pageIndex : Int
    , filterString : String
    , openMenuOption : Maybe Int
    , deleteVolunteerModal : Maybe Volunteer
    , tray : Toast.Tray To.Toast
    }


init : Auth.User -> Shared.Model -> () -> ( Model, Effect Msg )
init user shared () =
    ( { volunteers = Loading
      , pageIndex = 0
      , filterString = ""
      , openMenuOption = Nothing
      , deleteVolunteerModal = Nothing
      , tray = Toast.tray
      }
    , Api.Volunteers.get
        { onResponse = VolunteersApiResponded
        , tokens = user.tokens
        , apiUrl = shared.apiUrl
        , pageIndex = 0
        , filterString = ""
        }
    )



-- UPDATE


type Msg
    = PageChanged Int
    | OnClickOutside
    | EditVolunteer
    | ToastMsg Toast.Msg
    | AddToast String To.ToastType
    | FilterStringChanged String
    | DelayedFilterStringChanged String
    | MenuOptionToggle Int
    | DeleteVolunteer Volunteer
    | RequestDeleteVolunteer (Maybe Volunteer)
    | DeleteVolunteerResponse Volunteer (Result Http.Error String)
    | VolunteersApiResponded (Result Http.Error Volunteers)


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        ToastMsg toastMsg ->
            let
                ( newTray, newTmesg ) =
                    Toast.update toastMsg model.tray
            in
            ( { model | tray = newTray }
            , Effect.sendCmd <| Cmd.map ToastMsg newTmesg
            )

        EditVolunteer ->
            ( model
            , Effect.sendMsg <| AddToast "Not implemented yet!" To.Warning
            )

        AddToast message type_ ->
            let
                ( newTray, tmesg ) =
                    Toast.add model.tray <|
                        Toast.expireIn 5000 { message = message, toastType = type_ }
            in
            ( { model | tray = newTray }
            , Effect.sendCmd <| Cmd.map ToastMsg tmesg
            )

        VolunteersApiResponded (Err httpError) ->
            ( { model | volunteers = Failure httpError }
            , Effect.sendMsg <| AddToast "The volunteers API responded with an error." To.Danger
            )

        VolunteersApiResponded (Ok volunteers) ->
            ( { model | volunteers = Success volunteers }
            , Effect.none
            )

        OnClickOutside ->
            ( { model | openMenuOption = Nothing }
            , Effect.none
            )

        PageChanged pageIndex ->
            ( { model | pageIndex = pageIndex }
            , Api.Volunteers.get
                { onResponse = VolunteersApiResponded
                , tokens = user.tokens
                , apiUrl = shared.apiUrl
                , pageIndex = pageIndex
                , filterString = model.filterString
                }
            )

        RequestDeleteVolunteer volunteer ->
            ( { model
                | deleteVolunteerModal = volunteer
                , openMenuOption = Nothing
              }
            , Effect.none
            )

        DeleteVolunteer volunteer ->
            ( { model | deleteVolunteerModal = Nothing }
            , Api.Volunteers.delete
                { onResponse = DeleteVolunteerResponse volunteer
                , tokens = user.tokens
                , apiUrl = shared.apiUrl
                , builderAssistantId = volunteer.builderAssistantId
                }
            )

        DeleteVolunteerResponse volunteer (Ok _) ->
            ( { model
                | volunteers =
                    RemoteData.map
                        (\volunteers -> { volunteers | list = List.remove volunteer volunteers.list })
                        model.volunteers
              }
            , Effect.sendMsg <| AddToast "Volunteer deleted correctly." To.Success
            )

        DeleteVolunteerResponse _ (Err _) ->
            ( model
            , Effect.sendMsg <| AddToast "Something wrong happened while deleting the volunteer." To.Danger
            )

        FilterStringChanged filterString ->
            ( { model
                | filterString = filterString
                , pageIndex = 0
              }
            , delayMsg <| DelayedFilterStringChanged filterString
            )

        DelayedFilterStringChanged filterString ->
            if filterString == model.filterString then
                ( model
                , Api.Volunteers.get
                    { onResponse = VolunteersApiResponded
                    , tokens = user.tokens
                    , apiUrl = shared.apiUrl
                    , pageIndex = model.pageIndex
                    , filterString = filterString
                    }
                )

            else
                ( model, Effect.none )

        MenuOptionToggle id ->
            ( { model
                | openMenuOption =
                    if Just id == model.openMenuOption then
                        Nothing

                    else
                        Just id
              }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { openMenuOption } =
    if openMenuOption == Nothing then
        Sub.none

    else
        Browser.Events.onMouseDown (Dropdown.outsideTarget OnClickOutside "volunteer-dropdown")


delayMsg : msg -> Effect msg
delayMsg msg =
    Task.perform (always msg) (Process.sleep 500)
        |> Effect.sendCmd



-- VIEW


viewVolunteer : Model -> Auth.User -> Volunteer -> Html Msg
viewVolunteer model user volunteer =
    let
        isAdmin =
            user.role == Admin
    in
    Html.tr
        [ Attr.class "border-b dark:border-gray-700"
        ]
        [ Html.td
            [ Attr.class "px-4 py-3"
            ]
            [ Html.text <| String.fromInt volunteer.id ]
        , Html.th
            [ Attr.scope "row"
            , Attr.class "px-4 py-3 font-medium text-gray-900 whitespace-nowrap dark:text-white"
            ]
            [ Html.text volunteer.name ]
        , Html.td
            [ Attr.class "px-4 py-3"
            ]
            [ Html.text volunteer.lastName ]
        , Html.td
            [ Attr.class "px-4 py-3 font-barcode text-2xl"
            ]
            [ Html.text volunteer.builderAssistantId ]
        , Html.td
            [ Attr.class "px-4 py-3"
            ]
            [ Html.viewIf isAdmin <|
                Dropdown.view
                    { open = model.openMenuOption == Just volunteer.id
                    , toggle = MenuOptionToggle volunteer.id
                    , options =
                        [ Html.ul
                            [ Attr.class "py-1 text-sm text-gray-700 dark:text-gray-200"
                            , Attr.attribute "aria-labelledby" "edit-dropdown-button"
                            ]
                            [ Html.li []
                                [ Html.a
                                    [ Route.Path.href <|
                                        Route.Path.Volunteers_BuilderAssistantId_
                                            { builderAssistantId = volunteer.builderAssistantId }
                                    , Attr.class "block py-2 px-4 hover:bg-gray-100 dark:hover:bg-gray-600 dark:hover:text-white"
                                    ]
                                    [ Html.text "Show" ]
                                ]
                            , Html.li []
                                [ Html.a
                                    [ Attr.href "#"
                                    , Events.onClick EditVolunteer
                                    , Attr.class "block py-2 px-4 hover:bg-gray-100 dark:hover:bg-gray-600 dark:hover:text-white"
                                    ]
                                    [ Html.text "Edit" ]
                                ]
                            ]
                        , Html.div
                            [ Attr.class "py-1"
                            ]
                            [ Html.a
                                [ Attr.href "#"
                                , Events.onClick <| RequestDeleteVolunteer <| Just volunteer
                                , Attr.class "block py-2 px-4 text-sm text-gray-700 hover:bg-gray-100 dark:hover:bg-gray-600 dark:text-gray-200 dark:hover:text-white"
                                ]
                                [ Html.text "Delete" ]
                            ]
                        ]
                    , dropdownId = "volunteer-dropdown"
                    }
            ]
        ]


viewDeleteModal : Volunteer -> Html Msg
viewDeleteModal volunteer =
    Html.div
        [ Attr.id "deleteModal"
        , Attr.tabindex -1
        , Attr.class "flex overflow-y-auto overflow-x-hidden fixed top-0 right-0 left-0 z-50 justify-center items-center w-full md:inset-0 h-modal md:h-full"
        ]
        [ Html.div
            [ Attr.class "relative p-4 w-full max-w-md h-full md:h-auto"
            ]
            [ {- Modal content -}
              Html.div
                [ Attr.class "relative p-4 text-center bg-white rounded-lg shadow dark:bg-gray-800 sm:p-5"
                ]
                [ Html.button
                    [ Attr.type_ "button"
                    , Attr.class "text-gray-400 absolute top-2.5 right-2.5 bg-transparent hover:bg-gray-200 hover:text-gray-900 rounded-lg text-sm p-1.5 ml-auto inline-flex items-center dark:hover:bg-gray-600 dark:hover:text-white"
                    , Attr.attribute "data-modal-toggle" "deleteModal"
                    , Events.onClick <| RequestDeleteVolunteer Nothing
                    ]
                    [ Icon.close
                    , Html.span
                        [ Attr.class "sr-only"
                        ]
                        [ Html.text "Close modal" ]
                    ]
                , Icon.trash
                , Html.p
                    [ Attr.class "mb-4 text-gray-500 dark:text-gray-300"
                    ]
                    [ Html.text "Are you sure you want to delete the volunteer "
                    , Html.strong [] [ Html.text <| volunteer.name ++ " " ++ volunteer.lastName ]
                    , Html.text "?"
                    ]
                , Html.div
                    [ Attr.class "flex justify-center items-center space-x-4"
                    ]
                    [ Button.secondary
                        { onClick = RequestDeleteVolunteer Nothing
                        , content = "No, cancel"
                        , attrs = [ Attr.attribute "data-modal-toggle" "deleteModal" ]
                        }
                    , Button.danger
                        { onClick = DeleteVolunteer volunteer
                        , content = "Yes, I'm sure"
                        , attrs = [ Attr.type_ "submit" ]
                        }
                    ]
                ]
            ]
        ]


viewNotification : Model -> Html Msg
viewNotification model =
    Html.div [ Attr.class "absolute z-40 p-4 sm:ml-64 h-full" ]
        [ Toast.render
            (To.view (ToastMsg << Toast.remove << .id))
            model.tray
            (Toast.config ToastMsg)
        ]


view : Auth.User -> Model -> View Msg
view user model =
    { title = "Volunteers"
    , body =
        case model.volunteers of
            NotAsked ->
                [ Html.text "Loading..."
                ]

            Loading ->
                [ Spinner.view [ Attr.class "h-full w-full" ]
                ]

            Failure _ ->
                [ viewNotification model ]

            Success volunteers ->
                [ viewNotification model
                , Html.section
                    [ Attr.class "bg-gray-50 dark:bg-gray-900 p-0 sd:p-3 sm:p-5"
                    ]
                    [ Html.viewMaybe viewDeleteModal model.deleteVolunteerModal
                    , Html.div
                        [ Attr.class "mx-auto max-w-screen-xl px-0 sm:px-4 lg:px-12"
                        ]
                        [ Html.div
                            [ Attr.class "bg-white dark:bg-gray-800 relative shadow-md sm:rounded-lg overflow-hidden"
                            ]
                            [ Html.div
                                [ Attr.class "flex flex-col md:flex-row items-center justify-between space-y-3 md:space-y-0 md:space-x-4 p-4"
                                ]
                                [ Html.div
                                    [ Attr.class "w-full md:w-1/2"
                                    ]
                                    [ Html.form
                                        [ Attr.class "flex items-center"
                                        ]
                                        [ Html.label
                                            [ Attr.for "simple-search"
                                            , Attr.class "sr-only"
                                            ]
                                            [ Html.text "Search" ]
                                        , Html.div
                                            [ Attr.class "relative w-full"
                                            ]
                                            [ Html.div
                                                [ Attr.class "absolute inset-y-0 left-0 flex items-center pl-3 pointer-events-none"
                                                ]
                                                [ Icon.search
                                                ]
                                            , Html.input
                                                [ Attr.type_ "text"
                                                , Attr.id "simple-search"
                                                , Attr.class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-primary-500 focus:border-primary-500 block w-full pl-10 p-2 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-primary-500 dark:focus:border-primary-500"
                                                , Attr.placeholder "Search by name, last name or builder assistant id"
                                                , Events.onInput FilterStringChanged
                                                ]
                                                []
                                            ]
                                        ]
                                    ]
                                ]
                            , Html.div
                                [ Attr.class "overflow-x-auto"
                                ]
                                [ Html.table
                                    [ Attr.class "w-full text-sm text-left text-gray-500 dark:text-gray-400"
                                    ]
                                    [ Html.thead
                                        [ Attr.class "text-xs text-gray-700 uppercase bg-gray-50 dark:bg-gray-700 dark:text-gray-400"
                                        ]
                                        [ Html.tr []
                                            [ Html.th
                                                [ Attr.scope "col"
                                                , Attr.class "px-4 py-3"
                                                ]
                                                [ Html.text "Id" ]
                                            , Html.th
                                                [ Attr.scope "col"
                                                , Attr.class "px-4 py-3"
                                                ]
                                                [ Html.text "Name" ]
                                            , Html.th
                                                [ Attr.scope "col"
                                                , Attr.class "px-4 py-3"
                                                ]
                                                [ Html.text "Last name" ]
                                            , Html.th
                                                [ Attr.scope "col"
                                                , Attr.class "px-4 py-3"
                                                ]
                                                [ Html.text "Builder Assitant Id" ]
                                            , Html.th
                                                [ Attr.scope "col"
                                                , Attr.class "px-4 py-3"
                                                ]
                                                [ Html.span
                                                    [ Attr.class "sr-only"
                                                    ]
                                                    [ Html.text "Actions" ]
                                                ]
                                            ]
                                        ]
                                    , Html.tbody [] <|
                                        List.map (viewVolunteer model user) volunteers.list
                                    ]
                                ]
                            , Pagination.view
                                { itemsPerPage = 10
                                , currentPage = model.pageIndex + 1
                                , numItems = RemoteData.unwrap 0 .numVolunteers model.volunteers
                                , next = PageChanged <| model.pageIndex + 1
                                , prev = PageChanged <| model.pageIndex - 1
                                }
                            ]
                        ]
                    ]
                ]
    }
