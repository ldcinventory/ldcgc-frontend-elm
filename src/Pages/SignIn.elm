module Pages.SignIn exposing (Model, Msg, page)

import Api.SignIn exposing (Action(..), EulaData)
import Components.Spinner as Spinner
import Effect exposing (Effect)
import Html
import Html.Attributes as Attr
import Html.Attributes.Extra as Attr
import Html.Events as Events
import Html.Extra as Html
import Http
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Shared.Model exposing (AppUser(..))
import Svg
import Svg.Attributes as SvgAttr
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
        , update = update shared
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { email : String
    , password : String
    , isSubmittingForm : Bool
    , eulaData : WebData Api.SignIn.EulaData
    , errors : List Api.SignIn.Error
    , tokens : Maybe Shared.Model.Tokens
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { email = ""
      , password = ""
      , isSubmittingForm = False
      , eulaData = NotAsked
      , errors = []
      , tokens = Nothing
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = UserSubmittedForm
    | CloseModal
    | PerformEulaAction Action Shared.Model.Tokens
    | UserUpdatedInput Field String
    | EulaGetResponded (Result Http.Error EulaData)
    | EulaPutResponded (Result Http.Error String)
    | SignInApiResponded (Result (List Api.SignIn.Error) Shared.Model.AppUser)


type Field
    = Email
    | Password


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        CloseModal ->
            ( { model | eulaData = NotAsked }
            , Effect.none
            )

        UserUpdatedInput Email value ->
            ( { model | email = value }
            , Effect.none
            )

        UserUpdatedInput Password value ->
            ( { model | password = value }
            , Effect.none
            )

        UserSubmittedForm ->
            ( { model
                | isSubmittingForm = True
                , errors = []
              }
            , Api.SignIn.post
                { onResponse = SignInApiResponded
                , email = model.email
                , password = model.password
                , apiUrl = shared.apiUrl
                }
            )

        SignInApiResponded (Ok (ValidatedUser user)) ->
            ( { model | isSubmittingForm = False }
            , Effect.signIn user
            )

        SignInApiResponded (Ok (NotEulaAccepted tokens)) ->
            ( { model
                | isSubmittingForm = False
                , eulaData = Loading
                , tokens = Just tokens
              }
            , Api.SignIn.getEula
                { onResponse = EulaGetResponded
                , apiUrl = shared.apiUrl
                , tokens = tokens
                }
            )

        SignInApiResponded (Err errors) ->
            ( { model | isSubmittingForm = False, errors = errors }
            , Effect.none
            )

        EulaGetResponded result ->
            ( { model | eulaData = RemoteData.fromResult result }
            , Effect.none
            )

        PerformEulaAction action tokens ->
            ( { model | eulaData = NotAsked }
            , Api.SignIn.putEula
                { onResponse = EulaPutResponded
                , action = action
                , apiUrl = shared.apiUrl
                , tokens = tokens
                }
            )

        EulaPutResponded (Ok _) ->
            ( model
            , Effect.none
            )

        EulaPutResponded (Err httpError) ->
            ( { model | errors = [ { message = Api.SignIn.errorToString httpError } ] }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "LDC Login"
    , body =
        [ Html.div
            [ Attr.id "default-modal"
            , Attr.tabindex -1
            , Attr.attribute "aria-hidden" <|
                if RemoteData.isSuccess model.eulaData then
                    "true"

                else
                    "false"
            , Attr.classList [ ( "hidden", not <| RemoteData.isSuccess model.eulaData ) ]
            , Attr.class "overflow-y-auto overflow-x-hidden fixed top-0 right-0 left-0 z-50 justify-center items-center w-full md:inset-0 h-full max-h-full"
            ]
            [ Html.div
                [ Attr.class "flex items-center justify-center p-4 w-full h-full"
                ]
                [ {- Modal content -}
                  Html.div
                    [ Attr.class "relative bg-white rounded-lg shadow dark:bg-gray-700 w-full max-w-2xl h-auto max-h-full"
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
                            , Events.onClick CloseModal
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
                        [ case model.eulaData of
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
                                    [ Html.text <| "Error: " ++ Api.SignIn.errorToString httpError ]

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
                            , model.tokens
                                |> Attr.attributeMaybe
                                    (Events.onClick << PerformEulaAction Accept)
                            ]
                            [ Html.text "I accept" ]
                        , Html.button
                            [ Attr.attribute "data-modal-hide" "default-modal"
                            , Attr.type_ "button"
                            , Attr.class "ms-3 text-gray-500 bg-white hover:bg-gray-100 focus:ring-4 focus:outline-none focus:ring-blue-300 rounded-lg border border-gray-200 text-sm font-medium px-5 py-2.5 hover:text-gray-900 focus:z-10 dark:bg-gray-700 dark:text-gray-300 dark:border-gray-500 dark:hover:text-white dark:hover:bg-gray-600 dark:focus:ring-gray-600"
                            , model.tokens
                                |> Attr.attributeMaybe
                                    (Events.onClick << PerformEulaAction Reject)
                            ]
                            [ Html.text "Decline" ]
                        ]
                    ]
                ]
            ]
        , Html.section
            [ Attr.class "bg-color-gray-50 dark:bg-gray-900"
            ]
            [ Html.div
                [ Attr.class "flex flex-col items-center justify-center px-6 py-8 mx-auto lg:py-0 md:h-screen"
                ]
                [ Html.a
                    [ Attr.href "#"
                    , Attr.class "flex items-center mb-6 text-2xl font-semibold text-gray-900 dark:text-white"
                    ]
                    [ Html.img
                        [ Attr.class "w-8 h-8 mr-2"
                        , Attr.src "/logo.png"
                        , Attr.alt "logo"
                        ]
                        []
                    , Html.text "LDC GC"
                    ]
                , Html.div
                    [ Attr.class "w-full bg-color-white rounded-lg shadow dark:border md:mt-0 sm:max-w-md xl:p-0 dark:bg-gray-800 dark:border-gray-700"
                    ]
                    [ Html.div
                        [ Attr.class "p-6 space-y-4 md:space-y-6 sm:p-9"
                        ]
                        [ Html.h1
                            [ Attr.class """
                                text-xl
                                font-bold
                                leading-tight
                                tracking-tight
                                text-gray-900
                                md:text-2xl
                                dark:text-white
                                """
                            ]
                            [ Html.text "Sign in to your account" ]
                        , Html.form
                            [ Attr.class "space-y-4 md:space-y-6"
                            , Events.onSubmit UserSubmittedForm
                            , Attr.action "#"
                            ]
                            [ Html.div []
                                [ Html.label
                                    [ Attr.for "email"
                                    , Attr.class """
                                        block
                                        mb-2
                                        text-sm
                                        font-medium
                                        text-gray-900
                                        dark:text-white
                                        """
                                    ]
                                    [ Html.text "Your email" ]
                                , Html.input
                                    [ Attr.type_ "email"
                                    , Attr.name "email"
                                    , Attr.id "email"
                                    , Attr.class """
                                        bg-color-gray-50
                                        border
                                        border-gray-300
                                        text-gray-900
                                        rounded-lg
                                        block
                                        w-full
                                        p-2.5
                                        focus:ring-primary-600
                                        focus:border-primary-600
                                        sm:text-sm
                                        dark:bg-gray-700
                                        dark:border-gray-600
                                        dark:placeholder-gray-400
                                        dark:text-white
                                        dark:focus:ring-blue-500
                                        dark:focus:border-blue-500
                                        """
                                    , Attr.placeholder "name@domain.com"
                                    , Attr.attribute "data-test" "login-input-email"
                                    , Attr.required True
                                    , Attr.value model.email
                                    , Events.onInput (UserUpdatedInput Email)
                                    ]
                                    []
                                ]
                            , Html.div []
                                [ Html.label
                                    [ Attr.for "password"
                                    , Attr.class """
                                        block
                                        mb-2
                                        text-sm
                                        font-medium
                                        text-gray-900
                                        dark:text-white
                                        """
                                    ]
                                    [ Html.text "Password" ]
                                , Html.input
                                    [ Attr.type_ "password"
                                    , Attr.name "password"
                                    , Attr.id "password"
                                    , Attr.placeholder "••••••••"
                                    , Attr.class """
                                        bg-color-gray-50
                                        border
                                        border-gray-300
                                        text-gray-900
                                        rounded-lg
                                        block
                                        w-full
                                        p-2.5
                                        focus:ring-primary-600
                                        focus:border-primary-600
                                        sm:text-sm
                                        dark:bg-gray-700
                                        dark:border-gray-600
                                        dark:placeholder-gray-400
                                        dark:text-white
                                        dark:focus:ring-blue-500
                                        dark:focus:border-blue-500
                                        """
                                    , Attr.required True
                                    , Attr.attribute "data-test" "login-input-password"
                                    , Attr.value model.password
                                    , Events.onInput (UserUpdatedInput Password)
                                    ]
                                    []
                                ]
                            , List.head model.errors
                                |> Html.viewMaybe
                                    (\error ->
                                        Html.div
                                            [ Attr.class "text-sm text-red-500"
                                            ]
                                            [ Html.text error.message ]
                                    )
                            , Html.div
                                [ Attr.class "flex items-center justify-between"
                                ]
                                [ Html.div
                                    [ Attr.class "flex items-start"
                                    ]
                                    [ Html.div
                                        [ Attr.class "flex items-center h-5"
                                        ]
                                        [ Html.input
                                            [ Attr.id "remember"
                                            , Attr.attribute "aria-describedby" "remember"
                                            , Attr.type_ "checkbox"
                                            , Attr.checked True
                                            , Attr.class """
                                                w-4
                                                h-4
                                                border
                                                border-gray-300
                                                rounded
                                                bg-color-gray-50
                                                focus:ring-2
                                                focus:ring-color-blue-300
                                                dark:bg-gray-700
                                                dark:border-gray-600
                                                dark:focus:ring-primary-600
                                                dark:ring-offset-gray-800
                                                """
                                            ]
                                            []
                                        ]
                                    , Html.div
                                        [ Attr.class "ml-3 text-sm"
                                        ]
                                        [ Html.label
                                            [ Attr.for "remember"
                                            , Attr.class "text-gray-500 dark:text-gray-300"
                                            ]
                                            [ Html.text "Remember me" ]
                                        ]
                                    ]
                                , Html.a
                                    [ Attr.href "#"
                                    , Attr.class """
                                        text-sm
                                        font-medium
                                        text-primary-600
                                        hover:underline
                                        dark:text-gray-500
                                        """
                                    ]
                                    [ Html.text "Forgot password?" ]
                                ]
                            , Html.button
                                [ Attr.type_ "submit"
                                , Attr.class """
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
                                , Attr.disabled model.isSubmittingForm
                                , Attr.attribute "data-test" "login-button-submit"
                                ]
                                [ if model.isSubmittingForm then
                                    Spinner.view
                                        [ Attr.class "flex justify-center text-xs"
                                        ]

                                  else
                                    Html.text "Sign in"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }
