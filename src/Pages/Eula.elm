module Pages.Eula exposing (Model, Msg, page)

import Api.Eula exposing (Action(..))
import Auth
import Components.Spinner as Spinner
import Effect exposing (Effect)
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Layouts
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Shared.Model exposing (User)
import Svg
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
        { title = "EULA"
        , user = user
        }



-- INIT


type alias Model =
    { data : WebData Api.Eula.Data
    }


init : User -> Shared.Model -> () -> ( Model, Effect Msg )
init user shared () =
    ( Model Loading
    , Api.Eula.get
        { onResponse = EulaGetResponded
        , tokens = user.tokens
        , apiUrl = shared.apiUrl
        }
    )



-- UPDATE


type Msg
    = PerformEulaAction Action
    | EulaGetResponded (Result Http.Error Api.Eula.Data)
    | EulaPutResponded (Result Http.Error Api.Eula.Data)


update : User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        EulaGetResponded result ->
            ( { model | data = RemoteData.fromResult result }
            , Effect.none
            )

        PerformEulaAction action ->
            ( model
            , Api.Eula.put
                { onResponse = EulaPutResponded
                , action = action
                , apiUrl = shared.apiUrl
                , tokens = user.tokens
                }
            )

        EulaPutResponded result ->
            -- FIXME: if user REJECTs EULA, sign out automatically!
            ( { model | data = RemoteData.fromResult result }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Eula"
    , body =
        [ Html.div
            [ Attr.id "default-modal"
            , Attr.tabindex -1
            , Attr.attribute "aria-hidden" "true"
            , Attr.class "overflow-y-auto overflow-x-hidden flex z-50 justify-center items-center w-full md:inset-0 h-full max-h-full"
            ]
            [ Html.div
                [ Attr.class "relative p-4 w-full max-w-2xl h-full max-h-full"
                ]
                [ {- Modal content -}
                  Html.div
                    [ Attr.class "relative bg-white rounded-lg shadow dark:bg-gray-700 max-h-full"
                    ]
                    [ {- Modal header -}
                      Html.div
                        [ Attr.class "flex items-center justify-between p-4 md:p-5 border-b rounded-t dark:border-gray-600"
                        ]
                        [ Html.h3
                            [ Attr.class "text-xl font-semibold text-gray-900 dark:text-white"
                            ]
                            [ Html.text "EULA Terms of Service" ]
                        , Html.button
                            [ Attr.type_ "button"
                            , Attr.class "text-gray-400 bg-transparent hover:bg-gray-200 hover:text-gray-900 rounded-lg text-sm w-8 h-8 ms-auto inline-flex justify-center items-center dark:hover:bg-gray-600 dark:hover:text-white"
                            , Attr.attribute "data-modal-hide" "default-modal"
                            ]
                            [ Svg.svg
                                [ SvgAttr.class "w-3 h-3"
                                , Attr.attribute "aria-hidden" "true"
                                , SvgAttr.fill "none"
                                , SvgAttr.viewBox "0 0 14 14"
                                ]
                                [ Svg.path
                                    [ SvgAttr.stroke "currentColor"
                                    , SvgAttr.strokeLinecap "round"
                                    , SvgAttr.strokeLinejoin "round"
                                    , SvgAttr.strokeWidth "2"
                                    , SvgAttr.d "m1 1 6 6m0 0 6 6M7 7l6-6M7 7l-6 6"
                                    ]
                                    []
                                ]
                            , Html.span
                                [ Attr.class "sr-only"
                                ]
                                [ Html.text "Close modal" ]
                            ]
                        ]
                    , {- Modal body -}
                      Html.div
                        [ Attr.class "flex p-4 md:p-5 space-y-4 h-full max-h-full justify-center items-center"
                        ]
                        [ case model.data of
                            Loading ->
                                Spinner.view [ Attr.class "h-full" ]

                            Success { docUrl } ->
                                Html.iframe
                                    [ Attr.src docUrl
                                    , Attr.class "w-full h-[700px] max-h-full"
                                    ]
                                    []

                            Failure httpError ->
                                Html.div [ Attr.class "text-red-500" ]
                                    [ Html.text <| "Error: " ++ Api.Eula.errorToString httpError ]

                            NotAsked ->
                                Html.text "document Not asked"
                        ]
                    , {- Modal footer -}
                      Html.div
                        [ Attr.class "flex items-center p-4 md:p-5 border-t border-gray-200 rounded-b dark:border-gray-600"
                        ]
                        [ Html.button
                            [ Attr.attribute "data-modal-hide" "default-modal"
                            , Attr.type_ "button"
                            , Attr.class "text-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:outline-none focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center dark:bg-blue-600 dark:hover:bg-blue-700 dark:focus:ring-blue-800"
                            , Events.onClick <| PerformEulaAction Accept
                            ]
                            [ Html.text "I accept" ]
                        , Html.button
                            [ Attr.attribute "data-modal-hide" "default-modal"
                            , Attr.type_ "button"
                            , Attr.class "ms-3 text-gray-500 bg-white hover:bg-gray-100 focus:ring-4 focus:outline-none focus:ring-blue-300 rounded-lg border border-gray-200 text-sm font-medium px-5 py-2.5 hover:text-gray-900 focus:z-10 dark:bg-gray-700 dark:text-gray-300 dark:border-gray-500 dark:hover:text-white dark:hover:bg-gray-600 dark:focus:ring-gray-600"
                            , Events.onClick <| PerformEulaAction Reject
                            ]
                            [ Html.text "Decline" ]
                        ]
                    ]
                ]
            ]
        ]
    }
