module Components.Dropdown exposing (view)

import Html exposing (Html, a, button, div, li, text, ul)
import Html.Attributes as Attr
import Html.Events as Events
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr


view :
    { open : Bool
    , toggle : msg
    , onDelete : msg -- FIXME: temporary, this is supper crappy...
    }
    -> Html msg
view { open, toggle, onDelete } =
    Html.div
        [ Attr.class "flex items-center justify-end"
        ]
        [ button
            [ Attr.id "apple-imac-27-dropdown-button"
            , Attr.attribute "data-dropdown-toggle" "apple-imac-27-dropdown"
            , Attr.class "inline-flex items-center p-0.5 text-sm font-medium text-center text-gray-500 hover:text-gray-800 rounded-lg focus:outline-none dark:text-gray-400 dark:hover:text-gray-100"
            , Attr.type_ "button"
            , Events.onClick toggle
            ]
            [ svg
                [ SvgAttr.class "w-5 h-5"
                , Attr.attribute "aria-hidden" <|
                    if open then
                        "false"

                    else
                        "true"
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
            , Attr.classList
                [ ( "hidden", not open )
                , ( "block", open )
                ]
            , Attr.class "absolute z-10 w-44 bg-white rounded divide-y divide-gray-100 shadow dark:bg-gray-700 dark:divide-gray-600"

            -- FIXME: style="position: absolute; inset: 0px auto auto 0px; margin: 0px; transform: translate3d(1008px, 156px, 0px);"
            -- Check Flowbite.js to see how they calculate the translated3d for menu options...
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
                    , Events.onClick onDelete
                    , Attr.class "block py-2 px-4 text-sm text-gray-700 hover:bg-gray-100 dark:hover:bg-gray-600 dark:text-gray-200 dark:hover:text-white"
                    ]
                    [ text "Delete" ]
                ]
            ]
        ]
