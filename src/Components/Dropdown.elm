module Components.Dropdown exposing (outsideTarget, view)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Svg
import Svg.Attributes as SvgAttr


outsideTarget : msg -> String -> Decode.Decoder msg
outsideTarget closeMsg dropdownId =
    Decode.field "target" (isOutsideDropdown dropdownId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed closeMsg

                else
                    Decode.fail "inside dropdown"
            )


isOutsideDropdown : String -> Decoder Bool
isOutsideDropdown dropdownId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if dropdownId == id then
                        -- found match by id
                        Decode.succeed False

                    else
                        -- try next decoder
                        Decode.fail "continue"
                )
        , Decode.lazy (\_ -> isOutsideDropdown dropdownId |> Decode.field "parentNode")

        -- fallback if all previous decoders failed
        , Decode.succeed True
        ]


view :
    { open : Bool
    , toggle : msg
    , options : List (Html msg)
    , dropdownId : String
    }
    -> Html msg
view { open, toggle, options, dropdownId } =
    Html.div
        [ Attr.class "flex items-center justify-end"
        , Attr.id dropdownId
        ]
        [ Html.button
            [ Attr.attribute "data-dropdown-toggle" "dropdown"
            , Attr.class "inline-flex items-center p-0.5 text-sm font-medium text-center text-gray-500 hover:text-gray-800 rounded-lg focus:outline-none dark:text-gray-400 dark:hover:text-gray-100"
            , Attr.type_ "button"
            , Events.onClick toggle
            ]
            [ Svg.svg
                [ SvgAttr.class "w-5 h-5"
                , Attr.attribute "aria-hidden" <|
                    if open then
                        "false"

                    else
                        "true"
                , SvgAttr.fill "currentColor"
                , SvgAttr.viewBox "0 0 20 20"
                ]
                [ Svg.path
                    [ SvgAttr.d "M6 10a2 2 0 11-4 0 2 2 0 014 0zM12 10a2 2 0 11-4 0 2 2 0 014 0zM16 12a2 2 0 100-4 2 2 0 000 4z"
                    ]
                    []
                ]
            ]
        , Html.div
            [ Attr.classList
                [ ( "hidden", not open )
                , ( "block", open )
                ]
            , Attr.class "absolute z-10 w-44 bg-white rounded divide-y divide-gray-100 shadow dark:bg-gray-700 dark:divide-gray-600"
            ]
            options
        ]
