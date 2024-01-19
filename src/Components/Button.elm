module Components.Button exposing (primary)

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
    -> Html.Html msg
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
