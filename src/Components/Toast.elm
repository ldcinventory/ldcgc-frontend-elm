module Components.Toast exposing (Toast, ToastType(..), view)

import Components.Icons as Icon
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Toast


type ToastType
    = Success
    | Danger
    | Warning


type alias Toast =
    { message : String
    , toastType : ToastType
    }


view : (Toast.Info Toast -> msg) -> List (Html.Attribute msg) -> Toast.Info Toast -> Html msg
view closeToast _ toast =
    Html.div
        [ Attr.id "toast"
        , Attr.class "flex items-center w-full max-w-xs p-4 mb-4 text-gray-500 bg-white rounded-lg shadow dark:text-gray-400 dark:bg-gray-800"
        , Attr.attribute "role" "alert"
        ]
        [ case toast.content.toastType of
            Success ->
                Html.div
                    [ Attr.class "inline-flex items-center justify-center flex-shrink-0 w-8 h-8 text-green-500 bg-green-100 rounded-lg dark:bg-green-800 dark:text-green-200"
                    ]
                    [ Icon.check
                    , Html.span [ Attr.class "sr-only" ] [ Html.text "Check icon" ]
                    ]

            Danger ->
                Html.div
                    [ Attr.class "inline-flex items-center justify-center flex-shrink-0 w-8 h-8 text-red-500 bg-red-100 rounded-lg dark:bg-red-800 dark:text-red-200"
                    ]
                    [ Icon.error
                    , Html.span
                        [ Attr.class "sr-only"
                        ]
                        [ Html.text "Error icon" ]
                    ]

            Warning ->
                Html.div
                    [ Attr.class "inline-flex items-center justify-center flex-shrink-0 w-8 h-8 text-orange-500 bg-orange-100 rounded-lg dark:bg-orange-700 dark:text-orange-200"
                    ]
                    [ Icon.warning
                    , Html.span
                        [ Attr.class "sr-only"
                        ]
                        [ Html.text "Warning icon" ]
                    ]
        , Html.div
            [ Attr.class "ms-3 text-sm font-normal"
            ]
            [ Html.text toast.content.message ]
        , Html.button
            [ Attr.type_ "button"
            , Attr.class "ms-auto -mx-1.5 -my-1.5 bg-white text-gray-400 hover:text-gray-900 rounded-lg focus:ring-2 focus:ring-gray-300 p-1.5 hover:bg-gray-100 inline-flex items-center justify-center h-8 w-8 dark:text-gray-500 dark:hover:text-white dark:bg-gray-800 dark:hover:bg-gray-700"
            , Attr.attribute "data-dismiss-target" "#toast"
            , Attr.attribute "aria-label" "Close"
            , Events.onClick (closeToast toast)
            ]
            [ Html.span
                [ Attr.class "sr-only"
                ]
                [ Html.text "Close" ]
            , Icon.close
            ]
        ]
