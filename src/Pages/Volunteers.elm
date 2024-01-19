module Pages.Volunteers exposing (Model, Msg, page)

import Api.Volunteers
import Auth
import Components.Spinner as Spinner
import Effect exposing (Effect)
import Html exposing (Html, a, button, div, form, h6, input, label, li, nav, section, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Layouts
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Shared.Model exposing (Volunteer, Volunteers)
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared _ =
    Page.new
        { init = init user shared
        , update = update user shared
        , subscriptions = subscriptions
        , view = view
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
    }


init : Auth.User -> Shared.Model -> () -> ( Model, Effect Msg )
init user shared () =
    ( { volunteers = Loading, pageIndex = 0 }
    , Api.Volunteers.get
        { onResponse = VolunteersApiResponded
        , tokens = user.tokens
        , apiUrl = shared.apiUrl
        , pageIndex = 0
        }
    )



-- UPDATE


type Msg
    = VolunteersApiResponded (Result Http.Error Volunteers)
    | PageChanged Int


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

        PageChanged pageIndex ->
            ( { model | pageIndex = pageIndex, volunteers = Loading }
            , Api.Volunteers.get
                { onResponse = VolunteersApiResponded
                , tokens = user.tokens
                , apiUrl = shared.apiUrl
                , pageIndex = model.pageIndex
                }
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none



-- VIEW


viewVolunteer : Volunteer -> Html Msg
viewVolunteer volunteer =
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
            [ text <|
                if volunteer.isActive then
                    "✅"

                else
                    "❌"
            ]
        , td
            [ Attr.class "px-4 py-3 flex items-center justify-end"
            ]
            [ button
                -- TODO: turn these into actual dropdowns...
                [ Attr.id "apple-imac-27-dropdown-button"
                , Attr.attribute "data-dropdown-toggle" "apple-imac-27-dropdown"
                , Attr.class "inline-flex items-center p-0.5 text-sm font-medium text-center text-gray-500 hover:text-gray-800 rounded-lg focus:outline-none dark:text-gray-400 dark:hover:text-gray-100"
                , Attr.type_ "button"
                ]
                [ svg
                    [ SvgAttr.class "w-5 h-5"
                    , Attr.attribute "aria-hidden" "true"
                    , SvgAttr.fill "currentColor"
                    , SvgAttr.viewBox "0 0 20 20"
                    ]
                    [ path
                        [ SvgAttr.d "M6 10a2 2 0 11-4 0 2 2 0 014 0zM12 10a2 2 0 11-4 0 2 2 0 014 0zM16 12a2 2 0 100-4 2 2 0 000 4z"
                        ]
                        []
                    ]
                ]
            , div
                [ Attr.id "apple-imac-27-dropdown"
                , Attr.class "hidden z-10 w-44 bg-white rounded divide-y divide-gray-100 shadow dark:bg-gray-700 dark:divide-gray-600"
                ]
                [ ul
                    [ Attr.class "py-1 text-sm text-gray-700 dark:text-gray-200"
                    , Attr.attribute "aria-labelledby" "apple-imac-27-dropdown-button"
                    ]
                    [ li []
                        [ a
                            [ Attr.href "#"
                            , Attr.class "block py-2 px-4 hover:bg-gray-100 dark:hover:bg-gray-600 dark:hover:text-white"
                            ]
                            [ text "Show" ]
                        ]
                    , li []
                        [ a
                            [ Attr.href "#"
                            , Attr.class "block py-2 px-4 hover:bg-gray-100 dark:hover:bg-gray-600 dark:hover:text-white"
                            ]
                            [ text "Edit" ]
                        ]
                    ]
                , div
                    [ Attr.class "py-1"
                    ]
                    [ a
                        [ Attr.href "#"
                        , Attr.class "block py-2 px-4 text-sm text-gray-700 hover:bg-gray-100 dark:hover:bg-gray-600 dark:text-gray-200 dark:hover:text-white"
                        ]
                        [ text "Delete" ]
                    ]
                ]
            ]
        ]


view : Model -> View Msg
view model =
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
                [ Html.text "Something went wrong..."
                ]

            Success volunteers ->
                [ section
                    [ Attr.class "bg-gray-50 dark:bg-gray-900 p-3 sm:p-5"
                    ]
                    [ div
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
                                                [ svg
                                                    [ Attr.attribute "aria-hidden" "true"
                                                    , SvgAttr.class "w-5 h-5 text-gray-500 dark:text-gray-400"
                                                    , SvgAttr.fill "currentColor"
                                                    , SvgAttr.viewBox "0 0 20 20"
                                                    ]
                                                    [ path
                                                        [ SvgAttr.fillRule "evenodd"
                                                        , SvgAttr.d "M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z"
                                                        , SvgAttr.clipRule "evenodd"
                                                        ]
                                                        []
                                                    ]
                                                ]
                                            , input
                                                [ Attr.type_ "text"
                                                , Attr.id "simple-search"
                                                , Attr.class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-primary-500 focus:border-primary-500 block w-full pl-10 p-2 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-primary-500 dark:focus:border-primary-500"
                                                , Attr.placeholder "Search"
                                                , Attr.required True
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
                                            [ svg
                                                [ Attr.attribute "aria-hidden" "true"
                                                , SvgAttr.class "h-4 w-4 mr-2 text-gray-400"
                                                , SvgAttr.viewBox "0 0 20 20"
                                                , SvgAttr.fill "currentColor"
                                                ]
                                                [ path
                                                    [ SvgAttr.fillRule "evenodd"
                                                    , SvgAttr.d "M3 3a1 1 0 011-1h12a1 1 0 011 1v3a1 1 0 01-.293.707L12 11.414V15a1 1 0 01-.293.707l-2 2A1 1 0 018 17v-5.586L3.293 6.707A1 1 0 013 6V3z"
                                                    , SvgAttr.clipRule "evenodd"
                                                    ]
                                                    []
                                                ]
                                            , text "Filter"
                                            , svg
                                                [ SvgAttr.class "-mr-1 ml-1.5 w-5 h-5"
                                                , SvgAttr.fill "currentColor"
                                                , SvgAttr.viewBox "0 0 20 20"
                                                , Attr.attribute "aria-hidden" "true"
                                                ]
                                                [ path
                                                    [ SvgAttr.clipRule "evenodd"
                                                    , SvgAttr.fillRule "evenodd"
                                                    , SvgAttr.d "M5.293 7.293a1 1 0 011.414 0L10 10.586l3.293-3.293a1 1 0 111.414 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414z"
                                                    ]
                                                    []
                                                ]
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
                                                [ text "Active" ]
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
                                        List.map viewVolunteer volunteers
                                    ]
                                ]
                            , nav
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
                                            , svg
                                                [ SvgAttr.class "w-5 h-5"
                                                , Attr.attribute "aria-hidden" "true"
                                                , SvgAttr.fill "currentColor"
                                                , SvgAttr.viewBox "0 0 20 20"
                                                ]
                                                [ path
                                                    [ SvgAttr.fillRule "evenodd"
                                                    , SvgAttr.d "M12.707 5.293a1 1 0 010 1.414L9.414 10l3.293 3.293a1 1 0 01-1.414 1.414l-4-4a1 1 0 010-1.414l4-4a1 1 0 011.414 0z"
                                                    , SvgAttr.clipRule "evenodd"
                                                    ]
                                                    []
                                                ]
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
                                            , svg
                                                [ SvgAttr.class "w-5 h-5"
                                                , Attr.attribute "aria-hidden" "true"
                                                , SvgAttr.fill "currentColor"
                                                , SvgAttr.viewBox "0 0 20 20"
                                                ]
                                                [ path
                                                    [ SvgAttr.fillRule "evenodd"
                                                    , SvgAttr.d "M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
                                                    , SvgAttr.clipRule "evenodd"
                                                    ]
                                                    []
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
    }
