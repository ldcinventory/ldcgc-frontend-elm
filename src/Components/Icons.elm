module Components.Icons exposing (..)

import Html.Attributes as Attr
import Svg exposing (Svg, path, svg)
import Svg.Attributes as SvgAttr


sidebar : Svg msg
sidebar =
    svg
        [ SvgAttr.class "w-6 h-6"
        , Attr.attribute "aria-hidden" "true"
        , SvgAttr.fill "currentColor"
        , SvgAttr.viewBox "0 0 20 20"
        ]
        [ path
            [ SvgAttr.clipRule "evenodd"
            , SvgAttr.fillRule "evenodd"
            , SvgAttr.d "M2 4.75A.75.75 0 012.75 4h14.5a.75.75 0 010 1.5H2.75A.75.75 0 012 4.75zm0 10.5a.75.75 0 01.75-.75h7.5a.75.75 0 010 1.5h-7.5a.75.75 0 01-.75-.75zM2 10a.75.75 0 01.75-.75h14.5a.75.75 0 010 1.5H2.75A.75.75 0 012 10z"
            ]
            []
        ]


dashboard : Svg msg
dashboard =
    svg
        [ SvgAttr.class "w-5 h-5 text-gray-500 transition duration-75 dark:text-gray-400 group-hover:text-gray-900 dark:group-hover:text-white"
        , Attr.attribute "aria-hidden" "true"
        , SvgAttr.fill "currentColor"
        , SvgAttr.viewBox "0 0 22 21"
        ]
        [ path
            [ SvgAttr.d "M16.975 11H10V4.025a1 1 0 0 0-1.066-.998 8.5 8.5 0 1 0 9.039 9.039.999.999 0 0 0-1-1.066h.002Z"
            ]
            []
        , path
            [ SvgAttr.d "M12.5 0c-.157 0-.311.01-.565.027A1 1 0 0 0 11 1.02V10h8.975a1 1 0 0 0 1-.935c.013-.188.028-.374.028-.565A8.51 8.51 0 0 0 12.5 0Z"
            ]
            []
        ]


tools : List (Svg.Attribute msg) -> Svg msg
tools attrs =
    svg
        ([ SvgAttr.class "flex-shrink-0 w-5 h-5 transition duration-75  group-hover:text-gray-900 dark:group-hover:text-white"
         , Attr.attribute "aria-hidden" "true"
         , SvgAttr.fill "currentColor"
         , SvgAttr.viewBox "0 0 18 18"
         ]
            ++ attrs
        )
        [ path
            [ SvgAttr.d "M6.143 0H1.857A1.857 1.857 0 0 0 0 1.857v4.286C0 7.169.831 8 1.857 8h4.286A1.857 1.857 0 0 0 8 6.143V1.857A1.857 1.857 0 0 0 6.143 0Zm10 0h-4.286A1.857 1.857 0 0 0 10 1.857v4.286C10 7.169 10.831 8 11.857 8h4.286A1.857 1.857 0 0 0 18 6.143V1.857A1.857 1.857 0 0 0 16.143 0Zm-10 10H1.857A1.857 1.857 0 0 0 0 11.857v4.286C0 17.169.831 18 1.857 18h4.286A1.857 1.857 0 0 0 8 16.143v-4.286A1.857 1.857 0 0 0 6.143 10Zm10 0h-4.286A1.857 1.857 0 0 0 10 11.857v4.286c0 1.026.831 1.857 1.857 1.857h4.286A1.857 1.857 0 0 0 18 16.143v-4.286A1.857 1.857 0 0 0 16.143 10Z"
            ]
            []
        ]


volunteers : Svg msg
volunteers =
    svg
        [ SvgAttr.class "flex-shrink-0 w-5 h-5 text-gray-500 transition duration-75 dark:text-gray-400 group-hover:text-gray-900 dark:group-hover:text-white"
        , Attr.attribute "aria-hidden" "true"
        , SvgAttr.fill "currentColor"
        , SvgAttr.viewBox "0 0 20 18"
        ]
        [ path
            [ SvgAttr.d "M14 2a3.963 3.963 0 0 0-1.4.267 6.439 6.439 0 0 1-1.331 6.638A4 4 0 1 0 14 2Zm1 9h-1.264A6.957 6.957 0 0 1 15 15v2a2.97 2.97 0 0 1-.184 1H19a1 1 0 0 0 1-1v-1a5.006 5.006 0 0 0-5-5ZM6.5 9a4.5 4.5 0 1 0 0-9 4.5 4.5 0 0 0 0 9ZM8 10H5a5.006 5.006 0 0 0-5 5v2a1 1 0 0 0 1 1h11a1 1 0 0 0 1-1v-2a5.006 5.006 0 0 0-5-5Z"
            ]
            []
        ]


signOut : Svg msg
signOut =
    svg
        [ SvgAttr.class "flex-shrink-0 w-5 h-5 text-gray-500 transition duration-75 dark:text-gray-400 group-hover:text-gray-900 dark:group-hover:text-white"
        , Attr.attribute "aria-hidden" "true"
        , SvgAttr.fill "none"
        , SvgAttr.viewBox "0 0 16 16"
        ]
        [ path
            [ SvgAttr.stroke "currentColor"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.d "M4 8h11m0 0-4-4m4 4-4 4m-5 3H3a2 2 0 0 1-2-2V3a2 2 0 0 1 2-2h3"
            ]
            []
        ]


trash : Svg msg
trash =
    svg
        [ SvgAttr.class "text-gray-400 dark:text-gray-500 w-11 h-11 mb-3.5 mx-auto"
        , Attr.attribute "aria-hidden" "true"
        , SvgAttr.fill "currentColor"
        , SvgAttr.viewBox "0 0 20 20"
        ]
        [ path
            [ SvgAttr.fillRule "evenodd"
            , SvgAttr.d "M9 2a1 1 0 00-.894.553L7.382 4H4a1 1 0 000 2v10a2 2 0 002 2h8a2 2 0 002-2V6a1 1 0 100-2h-3.382l-.724-1.447A1 1 0 0011 2H9zM7 8a1 1 0 012 0v6a1 1 0 11-2 0V8zm5-1a1 1 0 00-1 1v6a1 1 0 102 0V8a1 1 0 00-1-1z"
            , SvgAttr.clipRule "evenodd"
            ]
            []
        ]


search : Svg msg
search =
    svg
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


chevronLeft : Svg msg
chevronLeft =
    svg
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


chevronRight : Svg msg
chevronRight =
    svg
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


filter : Svg msg
filter =
    svg
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


chevronDown : Svg msg
chevronDown =
    svg
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


check : Svg msg
check =
    svg
        [ SvgAttr.class "w-5 h-5"
        , Attr.attribute "aria-hidden" "true"
        , SvgAttr.fill "currentColor"
        , SvgAttr.viewBox "0 0 20 20"
        ]
        [ path
            [ SvgAttr.d "M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5Zm3.707 8.207-4 4a1 1 0 0 1-1.414 0l-2-2a1 1 0 0 1 1.414-1.414L9 10.586l3.293-3.293a1 1 0 0 1 1.414 1.414Z"
            ]
            []
        ]


error : Svg msg
error =
    svg
        [ SvgAttr.class "w-5 h-5"
        , Attr.attribute "aria-hidden" "true"
        , SvgAttr.fill "currentColor"
        , SvgAttr.viewBox "0 0 20 20"
        ]
        [ path
            [ SvgAttr.d "M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5Zm3.707 11.793a1 1 0 1 1-1.414 1.414L10 11.414l-2.293 2.293a1 1 0 0 1-1.414-1.414L8.586 10 6.293 7.707a1 1 0 0 1 1.414-1.414L10 8.586l2.293-2.293a1 1 0 0 1 1.414 1.414L11.414 10l2.293 2.293Z"
            ]
            []
        ]


warning : Svg msg
warning =
    svg
        [ SvgAttr.class "w-5 h-5"
        , Attr.attribute "aria-hidden" "true"
        , SvgAttr.fill "currentColor"
        , SvgAttr.viewBox "0 0 20 20"
        ]
        [ path
            [ SvgAttr.d "M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5ZM10 15a1 1 0 1 1 0-2 1 1 0 0 1 0 2Zm1-4a1 1 0 0 1-2 0V6a1 1 0 0 1 2 0v5Z"
            ]
            []
        ]


close : Svg msg
close =
    svg
        [ SvgAttr.class "w-3 h-3"
        , Attr.attribute "aria-hidden" "true"
        , SvgAttr.fill "none"
        , SvgAttr.viewBox "0 0 14 14"
        ]
        [ path
            [ SvgAttr.stroke "currentColor"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.d "m1 1 6 6m0 0 6 6M7 7l6-6M7 7l-6 6"
            ]
            []
        ]


clock : List (Svg.Attribute msg) -> Svg msg
clock attrs =
    svg
        ([ SvgAttr.class "flex-shrink-0 w-5 h-5 transition duration-75  group-hover:text-gray-900 dark:group-hover:text-white"
         , Attr.attribute "aria-hidden" "true"
         , SvgAttr.width "24"
         , SvgAttr.height "24"
         , SvgAttr.fill "currentColor"
         , SvgAttr.viewBox "0 0 24 24"
         ]
            ++ attrs
        )
        [ path
            [ SvgAttr.fillRule "evenodd"
            , SvgAttr.d "M2 12C2 6.477 6.477 2 12 2s10 4.477 10 10-4.477 10-10 10S2 17.523 2 12Zm11-4a1 1 0 1 0-2 0v4a1 1 0 0 0 .293.707l3 3a1 1 0 0 0 1.414-1.414L13 11.586V8Z"
            , SvgAttr.clipRule "evenodd"
            ]
            []
        ]


register : Svg msg
register =
    svg
        [ SvgAttr.class "flex-shrink-0 w-5 h-5 text-gray-500 transition duration-75 dark:text-gray-400 group-hover:text-gray-900 dark:group-hover:text-white"
        , SvgAttr.width "24"
        , SvgAttr.height "24"
        , SvgAttr.fill "currentColor"
        , SvgAttr.viewBox "0 0 24 24"
        ]
        [ path
            [ SvgAttr.fillRule "evenodd"
            , SvgAttr.d "M2 6a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V6Zm4.996 2a1 1 0 0 0 0 2h.01a1 1 0 1 0 0-2h-.01ZM11 8a1 1 0 1 0 0 2h6a1 1 0 1 0 0-2h-6Zm-4.004 3a1 1 0 1 0 0 2h.01a1 1 0 1 0 0-2h-.01ZM11 11a1 1 0 1 0 0 2h6a1 1 0 1 0 0-2h-6Zm-4.004 3a1 1 0 1 0 0 2h.01a1 1 0 1 0 0-2h-.01ZM11 14a1 1 0 1 0 0 2h6a1 1 0 1 0 0-2h-6Z"
            , SvgAttr.clipRule "evenodd"
            ]
            []
        ]


plus : Svg msg
plus =
    svg
        [ SvgAttr.class "h-3.5 w-3.5 mr-2"
        , SvgAttr.fill "currentColor"
        , SvgAttr.viewBox "0 0 20 20"
        , Attr.attribute "aria-hidden" "true"
        ]
        [ path
            [ SvgAttr.clipRule "evenodd"
            , SvgAttr.fillRule "evenodd"
            , SvgAttr.d "M10 3a1 1 0 011 1v5h5a1 1 0 110 2h-5v5a1 1 0 11-2 0v-5H4a1 1 0 110-2h5V4a1 1 0 011-1z"
            ]
            []
        ]
