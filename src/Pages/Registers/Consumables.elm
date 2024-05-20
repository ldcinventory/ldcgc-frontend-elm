module Pages.Registers.Consumables exposing (Model, Msg, page)

import Api.Registers.Consumables as ConsumablesApi
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
import Svg.Attributes as SvgAttr
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
    { items : WebData (Paginator ConsumableRegister)
    , pageIndex : Int
    , filterString : String
    , openMenuOption : Maybe Int
    , deleteModal : Maybe ConsumableRegister
    }


init : Auth.User -> Shared.Model -> () -> ( Model, Effect Msg )
init user shared () =
    ( { items = Loading
      , pageIndex = 0
      , filterString = ""
      , openMenuOption = Nothing
      , deleteModal = Nothing
      }
    , ConsumablesApi.get
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
            , ConsumablesApi.get
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
            ( { model | items = Failure httpError }
            , Effect.sendToast
                ("The consumables API responded with an error: "
                    ++ ConsumablesApi.errorToString httpError
                )
                Toast.Danger
            )

        ConsumableRegistersApiResponded (Ok consumables) ->
            ( { model | items = Success consumables }
            , Effect.none
            )

        RequestDeleteConsumableRegister Nothing ->
            ( { model
                | deleteModal = Nothing
                , openMenuOption = Nothing
              }
            , Effect.none
            )

        RequestDeleteConsumableRegister (Just consumable) ->
            ( { model
                | deleteModal = Just consumable
                , openMenuOption = Nothing
              }
            , Effect.none
            )

        DeleteConsumableRegister consumable ->
            ( { model | deleteModal = Nothing }
            , ConsumablesApi.delete
                { onResponse = DeleteConsumableRegisterResponse consumable
                , tokens = user.tokens
                , apiUrl = shared.apiUrl
                , registerId = consumable.id
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
                , ConsumablesApi.get
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
                | items =
                    RemoteData.map
                        (\consumables ->
                            { consumables | list = List.remove consumable consumables.list }
                        )
                        model.items
              }
            , Effect.sendToast "Consumable register deleted correctly." Toast.Success
            )

        DeleteConsumableRegisterResponse _ (Err _) ->
            ( model
            , Effect.sendToast "Something wrong happened while deleting the consumable register." Toast.Danger
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


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
            [ Html.text <|
                case consumable.registerTo of
                    Just date ->
                        Date.format "dd/MM/yyyy" <| Date.fromPosix Time.utc date

                    Nothing ->
                        "Not returned"
            ]
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
                                    -- FIXME: call PUT endoint here to Return the consumable
                                    [ -- Route.Path.href <|
                                      -- Route.Path.ConsumableRegisters_ConsumableRegisterId_
                                      --     { consumableId = String.fromInt consumable.id }
                                      Attr.class "block py-2 px-4 hover:bg-gray-100 dark:hover:bg-gray-600 dark:hover:text-white"
                                    ]
                                    [ Html.text "Return" ]
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
        Html.div
            [ Attr.class "border-b border-gray-200 dark:border-gray-700"
            ]
            [ Html.ul
                [ Attr.class "flex flex-wrap -mb-px text-sm font-medium text-center text-gray-500 dark:text-gray-400"
                ]
                [ Html.li
                    [ Attr.class "tab-tools"
                    ]
                    [ Html.a
                        [ Attr.href "/registers/tools"
                        , Attr.class "gap-2 inline-flex items-center justify-center p-4 rounded-t-lg border-b-2 group border-transparent hover:text-gray-600 hover:border-gray-300 dark:hover:text-gray-300"
                        ]
                        [ Icon.tools [ SvgAttr.class "text-gray-400 dark:text-gray-400" ]
                        , Html.text "Tools"
                        ]
                    ]
                , Html.li
                    [ Attr.class "tab-consumables"
                    ]
                    [ Html.a
                        [ Attr.href "/registers/consumables"
                        , Attr.class "gap-2 inline-flex items-center justify-center p-4 border-b-2 group text-primary-600 border-primary-600 active dark:text-primary-500 dark:border-primary-500"
                        , Attr.attribute "aria-current" "page"
                        ]
                        [ Icon.clock [ SvgAttr.class "text-primary-600 hover:text-primary-600" ]
                        , Html.text "Consumable"
                        ]
                    ]
                ]
            ]
            :: (case model.items of
                    NotAsked ->
                        [ Html.text "Loading..."
                        ]

                    Loading ->
                        [ Spinner.view [ Attr.class "h-full w-full" ]
                        ]

                    Failure httpError ->
                        [ Html.div [ Attr.class "text-red-500" ]
                            [ Html.text <| "Error: " ++ ConsumablesApi.errorToString httpError ]
                        ]

                    Success consumables ->
                        [ Html.section
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
                                                        , Attr.placeholder "Search by consumable name"
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
                                                        [ Html.text "Register to" ]
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
                                                        [ Html.tr
                                                            [ Attr.class "border-b dark:border-gray-700"
                                                            ]
                                                            [ Html.td
                                                                [ Attr.class "px-4 py-3 text-center"
                                                                , Attr.colspan 4
                                                                ]
                                                                [ Html.text "No consumables found" ]
                                                            ]
                                                        ]

                                                    xs ->
                                                        List.map (viewConsumableRegister model user) xs
                                            ]
                                        ]
                                    , Pagination.view
                                        { itemsPerPage = 10
                                        , currentPage = model.pageIndex + 1
                                        , numItems = consumables.numItems
                                        , totalPages = consumables.totalPages
                                        , elementsThisPage = consumables.elementsThisPage
                                        , next = PageChanged <| model.pageIndex + 1
                                        , prev = PageChanged <| model.pageIndex - 1
                                        }
                                    ]
                                ]
                            ]
                        ]
               )
    }


viewDeleteModal : ConsumableRegister -> Html Msg
viewDeleteModal item =
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
                    [ Html.text "Are you sure you want to delete the register "
                    , Html.strong []
                        [ Html.text <| item.consumableName
                        ]
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
                        { onClick = DeleteConsumableRegister item
                        , content = "Yes, I'm sure"
                        , attrs = [ Attr.type_ "submit" ]
                        }
                    ]
                ]
            ]
        ]
