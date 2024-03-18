module Components.Button exposing (danger, primary, secondary)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Attributes.Extra as Attr
import Html.Events as Events


primary :
    { content : Html msg
    , disabled : Bool
    , onClick : Maybe msg
    , attrs : List (Html.Attribute msg)
    }
    -> Html msg
primary config =
    Html.button
        ([ Attr.class """
            w-full
            text-white
            bg-primary-600
            font-medium
            rounded-lg
            text-sm
            px-5
            py-2.5
            text-center
            focus:ring-4
            focus:outline-none
            focus:ring-primary-300
            hover:bg-primary-700
            disabled:opacity-50
            dark:bg-primary-600
            dark:hover:bg-primary-700
            dark:focus:ring-primary-800
            """
         , Attr.disabled config.disabled
         , Attr.attributeMaybe Events.onClick config.onClick
         ]
            ++ config.attrs
        )
        [ config.content ]


secondary :
    { content : String
    , onClick : msg
    , attrs : List (Html.Attribute msg)
    }
    -> Html msg
secondary { content, onClick, attrs } =
    Html.button
        ([ Attr.type_ "button"
         , Attr.class "py-2 px-3 text-sm font-medium text-gray-500 bg-white rounded-lg border border-gray-200 hover:bg-gray-100 focus:ring-4 focus:outline-none focus:ring-primary-300 hover:text-gray-900 focus:z-10 dark:bg-gray-700 dark:text-gray-300 dark:border-gray-500 dark:hover:text-white dark:hover:bg-gray-600 dark:focus:ring-gray-600"
         , Events.onClick onClick
         ]
            ++ attrs
        )
        [ Html.text content ]


danger :
    { content : String
    , onClick : msg
    , attrs : List (Html.Attribute msg)
    }
    -> Html msg
danger { content, onClick, attrs } =
    Html.button
        ([ Attr.class "py-2 px-3 text-sm font-medium text-center text-white bg-red-600 rounded-lg hover:bg-red-700 focus:ring-4 focus:outline-none focus:ring-red-300 dark:bg-red-500 dark:hover:bg-red-600 dark:focus:ring-red-900"
         , Events.onClick onClick
         ]
            ++ attrs
        )
        [ Html.text content ]
