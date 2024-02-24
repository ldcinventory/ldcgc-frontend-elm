module Pages.Volunteers exposing (Model, Msg, page)

import Api.Volunteers
import Auth
import Components.Button as Button
import Components.Dropdown as Dropdown
import Components.Icons as Icons
import Components.Spinner as Spinner
import Effect exposing (Effect)
import Html exposing (Html, a, button, div, form, h6, input, label, li, nav, section, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Extra as Html
import Http
import Layouts
import Page exposing (Page)
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Shared.Model exposing (Role(..), Volunteer, Volunteers)
import Task
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
    }


init : Auth.User -> Shared.Model -> () -> ( Model, Effect Msg )
init user shared () =
    ( { volunteers = Loading
      , pageIndex = 0
      , filterString = ""
      , openMenuOption = Nothing
      , deleteVolunteerModal = Nothing
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
    | FilterStringChanged String
    | DelayedFilterStringChanged String
      -- | TODO: OnClickOutside https://dev.to/margaretkrutikova/elm-dom-node-decoder-to-detect-click-outside-3ioh
    | MenuOptionToggle Int
    | DeleteVolunteer String
    | RequestDeleteVolunteer (Maybe Volunteer)
    | DeleteVolunteerResponse (Result Http.Error String)
    | VolunteersApiResponded (Result Http.Error Volunteers)


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        VolunteersApiResponded (Err httpError) ->
            ( { model | volunteers = Failure httpError }
            , Effect.none
            )

        VolunteersApiResponded (Ok volunteers) ->
            ( { model | volunteers = Success volunteers }
            , Effect.none
            )

        -- TODO: OnClickOutside ->
        --     ( { model | openMenuOption = Nothing }
        --     , Effect.none
        --     )
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

        DeleteVolunteer builderAssistantId ->
            ( { model | deleteVolunteerModal = Nothing }
            , Api.Volunteers.delete
                { onResponse = DeleteVolunteerResponse
                , tokens = user.tokens
                , apiUrl = shared.apiUrl
                , builderAssistantId = builderAssistantId
                }
            )

        DeleteVolunteerResponse (Ok response) ->
            -- TODO: show notification (toast) everything went fine!
            ( model
              -- Refresh the list to see the changes
            , Api.Volunteers.get
                { onResponse = VolunteersApiResponded
                , tokens = user.tokens
                , apiUrl = shared.apiUrl
                , pageIndex = model.pageIndex
                , filterString = model.filterString
                }
            )

        DeleteVolunteerResponse (Err httpError) ->
            ( { model | volunteers = Failure httpError }, Effect.none )

        FilterStringChanged filterString ->
            ( { model | filterString = filterString }
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
subscriptions =
    always Sub.none


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
    tr
        [ Attr.class "border-b dark:border-gray-700"
        ]
        [ td
            [ Attr.class "px-4 py-3"
            ]
            [ text <| String.fromInt volunteer.id ]
        , th
            [ Attr.scope "row"
            , Attr.class "px-4 py-3 font-medium text-gray-900 whitespace-nowrap dark:text-white"
            ]
            [ text volunteer.name ]
        , td
            [ Attr.class "px-4 py-3"
            ]
            [ text volunteer.lastName ]
        , td
            [ Attr.class "px-4 py-3 font-barcode text-2xl"
            ]
            [ text volunteer.builderAssistantId ]
        , td
            [ Attr.class "px-4 py-3"
            ]
            [ Html.viewIf isAdmin <|
                Dropdown.view
                    { open = model.openMenuOption == Just volunteer.id
                    , toggle = MenuOptionToggle volunteer.id
                    , onDelete = RequestDeleteVolunteer <| Just volunteer
                    }
            ]
        ]


viewDeleteModal : Volunteer -> Html Msg
viewDeleteModal volunteer =
    div
        [ Attr.id "deleteModal"
        , Attr.tabindex -1
        , Attr.class "flex overflow-y-auto overflow-x-hidden fixed top-0 right-0 left-0 z-50 justify-center items-center w-full md:inset-0 h-modal md:h-full"
        ]
        [ div
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
                    [ Icons.trash
                    , span
                        [ Attr.class "sr-only"
                        ]
                        [ text "Close modal" ]
                    ]
                , Icons.close
                , Html.p
                    [ Attr.class "mb-4 text-gray-500 dark:text-gray-300"
                    ]
                    [ text "Are you sure you want to delete this volunteer?" ]
                , div
                    [ Attr.class "flex justify-center items-center space-x-4"
                    ]
                    [ Button.secondary
                        { onClick = RequestDeleteVolunteer Nothing
                        , content = "No, cancel"
                        , attrs = [ Attr.attribute "data-modal-toggle" "deleteModal" ]
                        }
                    , Button.danger
                        { onClick = DeleteVolunteer volunteer.builderAssistantId
                        , content = "Yes, I'm sure"
                        , attrs = [ Attr.type_ "submit" ]
                        }
                    ]
                ]
            ]
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

            Failure httpError ->
                -- FIXME: do something with volunteer errors!
                -- TODO: add Notification component and show errors there...
                -- TODO: implement Flowbite Toast https://flowbite.com/docs/components/toast/
                -- TODO: implement builderId search! (filtering with regex?)
                [ Html.text "Something went wrong..."
                ]

            Success volunteers ->
                [ section
                    [ Attr.class "bg-gray-50 dark:bg-gray-900 p-3 sm:p-5"
                    ]
                    [ Html.viewMaybe viewDeleteModal model.deleteVolunteerModal
                    , div
                        [ Attr.class "mx-auto max-w-screen-xl px-4 lg:px-12"
                        ]
                        [ {- Start coding here -}
                          div
                            [ Attr.class "bg-white dark:bg-gray-800 relative shadow-md sm:rounded-lg overflow-hidden"
                            ]
                            [ div
                                [ Attr.class "flex flex-col md:flex-row items-center justify-between space-y-3 md:space-y-0 md:space-x-4 p-4"
                                ]
                                [ div
                                    [ Attr.class "w-full md:w-1/2"
                                    ]
                                    [ form
                                        [ Attr.class "flex items-center"
                                        ]
                                        [ label
                                            [ Attr.for "simple-search"
                                            , Attr.class "sr-only"
                                            ]
                                            [ text "Search" ]
                                        , div
                                            [ Attr.class "relative w-full"
                                            ]
                                            [ div
                                                [ Attr.class "absolute inset-y-0 left-0 flex items-center pl-3 pointer-events-none"
                                                ]
                                                [ Icons.search
                                                ]
                                            , input
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
                                , div
                                    [ Attr.class "w-full md:w-auto flex flex-col md:flex-row space-y-2 md:space-y-0 items-stretch md:items-center justify-end md:space-x-3 flex-shrink-0"
                                    ]
                                    [ div
                                        [ Attr.class "flex items-center space-x-3 w-full md:w-auto"
                                        ]
                                        [ button
                                            [ Attr.id "filterDropdownButton"
                                            , Attr.attribute "data-dropdown-toggle" "filterDropdown"
                                            , Attr.class "w-full md:w-auto flex items-center justify-center py-2 px-4 text-sm font-medium text-gray-900 focus:outline-none bg-white rounded-lg border border-gray-200 hover:bg-gray-100 hover:text-primary-700 focus:z-10 focus:ring-4 focus:ring-gray-200 dark:focus:ring-gray-700 dark:bg-gray-800 dark:text-gray-400 dark:border-gray-600 dark:hover:text-white dark:hover:bg-gray-700"
                                            , Attr.type_ "button"
                                            ]
                                            [ Icons.filter
                                            , text "Filter"
                                            , Icons.chevronDown
                                            ]
                                        , div
                                            [ Attr.id "filterDropdown"
                                            , Attr.class "z-10 hidden w-48 p-3 bg-white rounded-lg shadow dark:bg-gray-700"
                                            ]
                                            [ h6
                                                [ Attr.class "mb-3 text-sm font-medium text-gray-900 dark:text-white"
                                                ]
                                                [ text "Choose brand" ]
                                            , ul
                                                [ Attr.class "space-y-2 text-sm"
                                                , Attr.attribute "aria-labelledby" "filterDropdownButton"
                                                ]
                                                [ li
                                                    [ Attr.class "flex items-center"
                                                    ]
                                                    [ input
                                                        [ Attr.id "apple"
                                                        , Attr.type_ "checkbox"
                                                        , Attr.value ""
                                                        , Attr.class "w-4 h-4 bg-gray-100 border-gray-300 rounded text-primary-600 focus:ring-primary-500 dark:focus:ring-primary-600 dark:ring-offset-gray-700 focus:ring-2 dark:bg-gray-600 dark:border-gray-500"
                                                        ]
                                                        []
                                                    , label
                                                        [ Attr.for "apple"
                                                        , Attr.class "ml-2 text-sm font-medium text-gray-900 dark:text-gray-100"
                                                        ]
                                                        [ text "Apple (56)" ]
                                                    ]
                                                , li
                                                    [ Attr.class "flex items-center"
                                                    ]
                                                    [ input
                                                        [ Attr.id "fitbit"
                                                        , Attr.type_ "checkbox"
                                                        , Attr.value ""
                                                        , Attr.class "w-4 h-4 bg-gray-100 border-gray-300 rounded text-primary-600 focus:ring-primary-500 dark:focus:ring-primary-600 dark:ring-offset-gray-700 focus:ring-2 dark:bg-gray-600 dark:border-gray-500"
                                                        ]
                                                        []
                                                    , label
                                                        [ Attr.for "fitbit"
                                                        , Attr.class "ml-2 text-sm font-medium text-gray-900 dark:text-gray-100"
                                                        ]
                                                        [ text "Microsoft (16)" ]
                                                    ]
                                                , li
                                                    [ Attr.class "flex items-center"
                                                    ]
                                                    [ input
                                                        [ Attr.id "razor"
                                                        , Attr.type_ "checkbox"
                                                        , Attr.value ""
                                                        , Attr.class "w-4 h-4 bg-gray-100 border-gray-300 rounded text-primary-600 focus:ring-primary-500 dark:focus:ring-primary-600 dark:ring-offset-gray-700 focus:ring-2 dark:bg-gray-600 dark:border-gray-500"
                                                        ]
                                                        []
                                                    , label
                                                        [ Attr.for "razor"
                                                        , Attr.class "ml-2 text-sm font-medium text-gray-900 dark:text-gray-100"
                                                        ]
                                                        [ text "Razor (49)" ]
                                                    ]
                                                , li
                                                    [ Attr.class "flex items-center"
                                                    ]
                                                    [ input
                                                        [ Attr.id "nikon"
                                                        , Attr.type_ "checkbox"
                                                        , Attr.value ""
                                                        , Attr.class "w-4 h-4 bg-gray-100 border-gray-300 rounded text-primary-600 focus:ring-primary-500 dark:focus:ring-primary-600 dark:ring-offset-gray-700 focus:ring-2 dark:bg-gray-600 dark:border-gray-500"
                                                        ]
                                                        []
                                                    , label
                                                        [ Attr.for "nikon"
                                                        , Attr.class "ml-2 text-sm font-medium text-gray-900 dark:text-gray-100"
                                                        ]
                                                        [ text "Nikon (12)" ]
                                                    ]
                                                , li
                                                    [ Attr.class "flex items-center"
                                                    ]
                                                    [ input
                                                        [ Attr.id "benq"
                                                        , Attr.type_ "checkbox"
                                                        , Attr.value ""
                                                        , Attr.class "w-4 h-4 bg-gray-100 border-gray-300 rounded text-primary-600 focus:ring-primary-500 dark:focus:ring-primary-600 dark:ring-offset-gray-700 focus:ring-2 dark:bg-gray-600 dark:border-gray-500"
                                                        ]
                                                        []
                                                    , label
                                                        [ Attr.for "benq"
                                                        , Attr.class "ml-2 text-sm font-medium text-gray-900 dark:text-gray-100"
                                                        ]
                                                        [ text "BenQ (74)" ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            , div
                                [ Attr.class "overflow-x-auto"
                                ]
                                [ table
                                    [ Attr.class "w-full text-sm text-left text-gray-500 dark:text-gray-400"
                                    ]
                                    [ thead
                                        [ Attr.class "text-xs text-gray-700 uppercase bg-gray-50 dark:bg-gray-700 dark:text-gray-400"
                                        ]
                                        [ tr []
                                            [ th
                                                [ Attr.scope "col"
                                                , Attr.class "px-4 py-3"
                                                ]
                                                [ text "Id" ]
                                            , th
                                                [ Attr.scope "col"
                                                , Attr.class "px-4 py-3"
                                                ]
                                                [ text "Name" ]
                                            , th
                                                [ Attr.scope "col"
                                                , Attr.class "px-4 py-3"
                                                ]
                                                [ text "Last name" ]
                                            , th
                                                [ Attr.scope "col"
                                                , Attr.class "px-4 py-3"
                                                ]
                                                [ text "Builder Assitant Id" ]
                                            , th
                                                [ Attr.scope "col"
                                                , Attr.class "px-4 py-3"
                                                ]
                                                [ span
                                                    [ Attr.class "sr-only"
                                                    ]
                                                    [ text "Actions" ]
                                                ]
                                            ]
                                        ]
                                    , tbody [] <|
                                        List.map (viewVolunteer model user) volunteers
                                    ]
                                ]
                            , nav
                                -- TODO: extract pagination and use something like https://package.elm-lang.org/packages/jschomay/elm-paginate/latest/Paginate
                                [ Attr.class "flex flex-col md:flex-row justify-between items-start md:items-center space-y-3 md:space-y-0 p-4"
                                , Attr.attribute "aria-label" "Table navigation"
                                ]
                                [ span
                                    [ Attr.class "text-sm font-normal text-gray-500 dark:text-gray-400"
                                    ]
                                    [ text "Showing "
                                    , span
                                        [ Attr.class "font-semibold text-gray-900 dark:text-white"
                                        ]
                                        [ text "1-10" ]
                                    , text " of "
                                    , span
                                        [ Attr.class "font-semibold text-gray-900 dark:text-white"
                                        ]
                                        [ text "1000" ]
                                    ]
                                , ul
                                    [ Attr.class "inline-flex items-stretch -space-x-px"
                                    ]
                                    [ li []
                                        [ button
                                            [ Events.onClick <| PageChanged <| model.pageIndex - 1
                                            , Attr.class "flex items-center justify-center h-full py-1.5 px-3 ml-0 text-gray-500 bg-white rounded-l-lg border border-gray-300 hover:bg-gray-100 hover:text-gray-700 dark:bg-gray-800 dark:border-gray-700 dark:text-gray-400 dark:hover:bg-gray-700 dark:hover:text-white"
                                            ]
                                            [ span
                                                [ Attr.class "sr-only"
                                                ]
                                                [ text "Previous" ]
                                            , Icons.chevronLeft
                                            ]
                                        ]
                                    , li []
                                        [ a
                                            [ Attr.href "#"
                                            , Attr.attribute "aria-current" "page"
                                            , Attr.class "flex items-center justify-center text-sm z-10 py-2 px-3 leading-tight text-primary-600 bg-primary-50 border border-primary-300 hover:bg-primary-100 hover:text-primary-700 dark:border-gray-700 dark:bg-gray-700 dark:text-white"
                                            ]
                                            [ text "1" ]
                                        ]
                                    , li []
                                        [ a
                                            [ Attr.href "#"
                                            , Attr.class "flex items-center justify-center text-sm py-2 px-3 leading-tight text-gray-500 bg-white border border-gray-300 hover:bg-gray-100 hover:text-gray-700 dark:bg-gray-800 dark:border-gray-700 dark:text-gray-400 dark:hover:bg-gray-700 dark:hover:text-white"
                                            ]
                                            [ text "2" ]
                                        ]
                                    , li []
                                        [ a
                                            [ Attr.href "#"
                                            , Attr.class "flex items-center justify-center text-sm py-2 px-3 leading-tight text-gray-500 bg-white border border-gray-300 hover:bg-gray-100 hover:text-gray-700 dark:bg-gray-800 dark:border-gray-700 dark:text-gray-400 dark:hover:bg-gray-700 dark:hover:text-white"
                                            ]
                                            [ text "3" ]
                                        ]
                                    , li []
                                        [ a
                                            [ Attr.href "#"
                                            , Attr.class "flex items-center justify-center text-sm py-2 px-3 leading-tight text-gray-500 bg-white border border-gray-300 hover:bg-gray-100 hover:text-gray-700 dark:bg-gray-800 dark:border-gray-700 dark:text-gray-400 dark:hover:bg-gray-700 dark:hover:text-white"
                                            ]
                                            [ text "..." ]
                                        ]
                                    , li []
                                        [ a
                                            [ Attr.href "#"
                                            , Attr.class "flex items-center justify-center text-sm py-2 px-3 leading-tight text-gray-500 bg-white border border-gray-300 hover:bg-gray-100 hover:text-gray-700 dark:bg-gray-800 dark:border-gray-700 dark:text-gray-400 dark:hover:bg-gray-700 dark:hover:text-white"
                                            ]
                                            [ text "100" ]
                                        ]
                                    , li []
                                        [ button
                                            [ Attr.class "flex items-center justify-center h-full py-1.5 px-3 leading-tight text-gray-500 bg-white rounded-r-lg border border-gray-300 hover:bg-gray-100 hover:text-gray-700 dark:bg-gray-800 dark:border-gray-700 dark:text-gray-400 dark:hover:bg-gray-700 dark:hover:text-white"
                                            , Events.onClick <| PageChanged <| model.pageIndex + 1
                                            ]
                                            [ span
                                                [ Attr.class "sr-only"
                                                ]
                                                [ text "Next" ]
                                            , Icons.chevronRight
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
    }
