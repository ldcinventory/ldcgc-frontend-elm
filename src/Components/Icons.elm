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


tools : Svg msg
tools =
    svg
        [ SvgAttr.class "flex-shrink-0 w-5 h-5 text-gray-500 transition duration-75 dark:text-gray-400 group-hover:text-gray-900 dark:group-hover:text-white"
        , Attr.attribute "aria-hidden" "true"
        , SvgAttr.fill "currentColor"
        , SvgAttr.viewBox "0 0 18 18"
        ]
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
        [ Attr.attribute "aria-hidden" "true"
        , SvgAttr.class "w-5 h-5"
        , SvgAttr.fill "currentColor"
        , SvgAttr.viewBox "0 0 20 20"
        ]
        [ path
            [ SvgAttr.fillRule "evenodd"
            , SvgAttr.d "M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
            , SvgAttr.clipRule "evenodd"
            ]
            []
        ]


close : Svg msg
close =
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
