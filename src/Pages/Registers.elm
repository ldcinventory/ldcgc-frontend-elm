module Pages.Registers exposing (Model, Msg, page)

import Api.Registers.Consumables as Api
import Auth
import Components.Button as Button
import Components.Dropdown as Dropdown
import Components.Icons as Icon
import Components.Pagination as Pagination
import Components.Spinner as Spinner
import Components.Toast as Toast
import Date
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Extra as Html
import Http
import Layouts
import List.Extra as List
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Shared.Model exposing (ConsumableRegister, Paginator, Role(..))
import Time
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
        { title = "Registers"
        , user = user
        }



-- INIT


type alias Model =
    { consumables : WebData (Paginator ConsumableRegister)
    , pageIndex : Int
    , filterString : String
    , openMenuOption : Maybe Int
    , deleteModal : Maybe ConsumableRegister
    }


init : Auth.User -> Shared.Model -> () -> ( Model, Effect Msg )
init user shared () =
    ( { consumables = Loading
      , pageIndex = 0
      , filterString = ""
      , openMenuOption = Nothing
      , deleteModal = Nothing
      }
    , Api.get
        { onResponse = ConsumableRegistersApiResponded
        , tokens = user.tokens
        , apiUrl = shared.apiUrl
        , pageIndex = 0
        , filterString = ""
        }
    )



-- UPDATE


type Msg
    = PageChanged Int
    | MenuOptionToggle Int
    | DeleteConsumableRegister ConsumableRegister
    | FilterStringChanged String
    | DelayedFilterStringChanged String
    | RequestDeleteConsumableRegister (Maybe ConsumableRegister)
    | DeleteConsumableRegisterResponse ConsumableRegister (Result Http.Error String)
    | ConsumableRegistersApiResponded (Result Http.Error (Paginator ConsumableRegister))


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        PageChanged pageIndex ->
            ( { model | pageIndex = pageIndex }
            , Api.get
                { onResponse = ConsumableRegistersApiResponded
                , tokens = user.tokens
                , apiUrl = shared.apiUrl
                , pageIndex = pageIndex
                , filterString = model.filterString
                }
            )

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

        ConsumableRegistersApiResponded (Err httpError) ->
            ( { model | consumables = Failure httpError }
            , Effect.sendToast ("The consumables API responded with an error: " ++ Api.errorToString httpError) Toast.Danger
            )

        ConsumableRegistersApiResponded (Ok consumables) ->
            ( { model | consumables = Success consumables }
            , Effect.none
            )

        RequestDeleteConsumableRegister consumable ->
            ( { model
                | deleteModal = consumable
                , openMenuOption = Nothing
              }
            , Effect.none
            )

        DeleteConsumableRegister consumable ->
            ( { model | deleteModal = Nothing }
            , Api.delete
                { onResponse = DeleteConsumableRegisterResponse consumable
                , tokens = user.tokens
                , apiUrl = shared.apiUrl
                , consumableId = consumable.id
                }
            )

        FilterStringChanged filterString ->
            ( { model
                | filterString = filterString
                , pageIndex = 0
              }
            , Effect.delayMsg <| DelayedFilterStringChanged filterString
            )

        DelayedFilterStringChanged filterString ->
            if filterString == model.filterString then
                ( model
                , Api.get
                    { onResponse = ConsumableRegistersApiResponded
                    , tokens = user.tokens
                    , apiUrl = shared.apiUrl
                    , pageIndex = model.pageIndex
                    , filterString = filterString
                    }
                )

            else
                ( model, Effect.none )

        DeleteConsumableRegisterResponse consumable (Ok _) ->
            ( { model
                | consumables =
                    RemoteData.map
                        (\consumables -> { consumables | list = List.remove consumable consumables.list })
                        model.consumables
              }
            , Effect.sendToast "ConsumableRegister deleted correctly." Toast.Success
            )

        DeleteConsumableRegisterResponse _ (Err _) ->
            ( model
            , Effect.sendToast "Something wrong happened while deleting the consumable." Toast.Danger
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


viewEmpty : Html Msg
viewEmpty =
    Html.tr
        [ Attr.class "border-b dark:border-gray-700"
        ]
        [ Html.td
            [ Attr.class "px-4 py-3 text-center"
            , Attr.colspan 4
            ]
            [ Html.text "No consumables found" ]
        ]


viewConsumableRegister : Model -> Auth.User -> ConsumableRegister -> Html Msg
viewConsumableRegister model user consumable =
    let
        isAdmin : Bool
        isAdmin =
            user.role == Admin
    in
    Html.tr
        [ Attr.class "border-b dark:border-gray-700"
        ]
        [ Html.td
            [ Attr.class "px-4 py-3"
            ]
            [ Html.text <| String.fromInt consumable.id ]
        , Html.td
            [ Attr.class "px-4 py-3 font-barcode text-2xl"
            ]
            [ Html.text consumable.consumableBarcode ]
        , Html.th
            [ Attr.scope "row"
            , Attr.class "px-4 py-3 font-medium text-gray-900 whitespace-nowrap dark:text-white"
            ]
            [ Html.text consumable.consumableName ]
        , Html.td
            [ Attr.class "px-4 py-3"
            ]
            [ Html.text consumable.volunteerName ]
        , Html.td
            [ Attr.class "px-4 py-3 max-w-[200px] truncate text-ellipsis"
            ]
            [ Html.text consumable.volunteerLastName ]
        , Html.td
            [ Attr.class "px-4 py-3"
            ]
            [ Html.text <| (String.slice 0 5 <| String.fromFloat consumable.stockAmountRequest) ]
        , Html.td
            [ Attr.class "px-4 py-3"
            ]
            [ Html.text <| Date.format "dd/MM/yyyy" <| Date.fromPosix Time.utc consumable.registerFrom ]
        , Html.td
            [ Attr.class "px-4 py-3"
            ]
            [ Html.viewIf isAdmin <|
                Dropdown.view
                    { open = model.openMenuOption == Just consumable.id
                    , toggle = MenuOptionToggle consumable.id
                    , options =
                        [ Html.ul
                            [ Attr.class "py-1 text-sm text-gray-700 dark:text-gray-200"
                            , Attr.attribute "aria-labelledby" "edit-dropdown-button"
                            ]
                            [ Html.li []
                                [ Html.a
                                    [ -- Route.Path.href <|
                                      -- Route.Path.ConsumableRegisters_ConsumableRegisterId_
                                      --     { consumableId = String.fromInt consumable.id }
                                      Attr.class "block py-2 px-4 hover:bg-gray-100 dark:hover:bg-gray-600 dark:hover:text-white"
                                    ]
                                    [ Html.text "Show" ]
                                ]
                            , Html.li []
                                [ Html.a
                                    [ Attr.href <| "/consumables/" ++ String.fromInt consumable.id ++ "?edit=true"
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
                                , Events.onClick <| RequestDeleteConsumableRegister <| Just consumable
                                , Attr.class "block py-2 px-4 text-sm text-gray-700 hover:bg-gray-100 dark:hover:bg-gray-600 dark:text-gray-200 dark:hover:text-white"
                                ]
                                [ Html.text "Delete" ]
                            ]
                        ]
                    , dropdownId = "volunteer-dropdown"
                    }
            ]
        ]


view : Auth.User -> Model -> View Msg
view user model =
    { title = "ConsumableRegisters"
    , body =
        case model.consumables of
            NotAsked ->
                [ Html.text "Loading..."
                ]

            Loading ->
                [ Spinner.view [ Attr.class "h-full w-full" ]
                ]

            Failure httpError ->
                [ Html.div [ Attr.class "text-red-500" ]
                    [ Html.text <| "Error: " ++ Api.errorToString httpError ]
                ]

            Success consumables ->
                [ Html.div
                    [ Attr.class "border-b border-gray-200 dark:border-gray-700"
                    ]
                    [ Html.ul
                        [ Attr.class "flex flex-wrap -mb-px text-sm font-medium text-center text-gray-500 dark:text-gray-400"
                        ]
                        [ Html.li
                            [ Attr.class "tab-tools"
                            ]
                            [ Html.a
                                [ Attr.href "#"
                                , Attr.class "gap-2 inline-flex items-center justify-center p-4 border-b-2 border-transparent rounded-t-lg hover:text-gray-600 hover:border-gray-300 dark:hover:text-gray-300 group"
                                ]
                                [ Icon.tools
                                , Html.text "Tools"
                                ]
                            ]
                        , Html.li
                            [ Attr.class "tab-consumables"
                            ]
                            [ Html.a
                                [ Attr.href "#"
                                , Attr.class "gap-2 inline-flex items-center justify-center p-4 text-primary-600 border-b-2 border-primary-600 rounded-t-lg active dark:text-primary-500 dark:border-primary-500 group"
                                , Attr.attribute "aria-current" "page"
                                ]
                                -- FIXME: fix color of icon
                                [ Icon.clock
                                , Html.text "Consumable"
                                ]
                            ]
                        ]
                    , Html.section
                        [ Attr.class "bg-gray-50 dark:bg-gray-900 p-0 sd:p-3 sm:p-5"
                        ]
                        [ Html.viewMaybe viewDeleteModal model.deleteModal
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
                                                    , Attr.placeholder "Search by name, brand, category, model or description"
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
                                                    [ Html.text "Barcode" ]
                                                , Html.th
                                                    [ Attr.scope "col"
                                                    , Attr.class "px-4 py-3"
                                                    ]
                                                    [ Html.text "Consumable" ]
                                                , Html.th
                                                    [ Attr.scope "col"
                                                    , Attr.class "px-4 py-3"
                                                    ]
                                                    [ Html.text "Volunteer" ]
                                                , Html.th
                                                    [ Attr.scope "col"
                                                    , Attr.class "px-4 py-3"
                                                    ]
                                                    [ Html.text "Volunteer lastname" ]
                                                , Html.th
                                                    [ Attr.scope "col"
                                                    , Attr.class "px-4 py-3"
                                                    ]
                                                    [ Html.text "Stock requested" ]
                                                , Html.th
                                                    [ Attr.scope "col"
                                                    , Attr.class "px-4 py-3"
                                                    ]
                                                    [ Html.text "Register from" ]
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
                                            case consumables.list of
                                                [] ->
                                                    [ viewEmpty ]

                                                xs ->
                                                    List.map (viewConsumableRegister model user) xs
                                        ]
                                    ]
                                , Pagination.view
                                    { itemsPerPage = 10
                                    , currentPage = model.pageIndex + 1
                                    , numItems = RemoteData.unwrap 0 .numItems model.consumables
                                    , totalPages = RemoteData.unwrap 0 .totalPages model.consumables
                                    , elementsThisPage = RemoteData.unwrap 0 .elementsThisPage model.consumables
                                    , next = PageChanged <| model.pageIndex + 1
                                    , prev = PageChanged <| model.pageIndex - 1
                                    }
                                ]
                            ]
                        ]
                    ]
                ]
    }


viewDeleteModal : ConsumableRegister -> Html Msg
viewDeleteModal consumable =
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
                    , Events.onClick <| RequestDeleteConsumableRegister Nothing
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
                    , Html.strong [] [ Html.text <| consumable.consumableName ]
                    , Html.text "?"
                    ]
                , Html.div
                    [ Attr.class "flex justify-center items-center space-x-4"
                    ]
                    [ Button.secondary
                        { onClick = RequestDeleteConsumableRegister Nothing
                        , content = "No, cancel"
                        , attrs = [ Attr.attribute "data-modal-toggle" "deleteModal" ]
                        }
                    , Button.danger
                        { onClick = DeleteConsumableRegister consumable
                        , content = "Yes, I'm sure"
                        , attrs = [ Attr.type_ "submit" ]
                        }
                    ]
                ]
            ]
        ]
