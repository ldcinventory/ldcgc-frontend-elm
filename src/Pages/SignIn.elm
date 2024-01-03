module Pages.SignIn exposing (Model, Msg, page)

import Api.SignIn
import Components.Spinner as Spinner
import Effect exposing (Effect)
import Html exposing (a, button, div, form, h1, img, input, label, section, text)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Extra as Html
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
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
    , errors : List Api.SignIn.Error
    }


init : () -> ( Model, Effect Msg )
init () =
    ( Model "" "" False []
    , Effect.none
    )



-- UPDATE


type Msg
    = UserUpdatedInput Field String
    | UserSubmittedForm
    | SignInApiResponded (Result (List Api.SignIn.Error) Shared.Model.User)


type Field
    = Email
    | Password


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
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

        SignInApiResponded (Ok user) ->
            ( { model | isSubmittingForm = False }
            , Effect.signIn user
            )

        SignInApiResponded (Err errors) ->
            ( { model | isSubmittingForm = False, errors = errors }
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
        [ section
            [ Attr.class "bg-folor-gray-50"
            ]
            [ div
                [ Attr.class "flex flex-col items-center justify-center px-6 py-8 mx-auto lg:py-0 md:h-screen"
                ]
                [ a
                    [ Attr.href "#"
                    , Attr.class "flex items-center mb-6 text-2xl font-semibold text-gray-900"
                    ]
                    [ img
                        [ Attr.class "w-8 h-8 mr-2"
                        , Attr.src "/logo.png"
                        , Attr.alt "logo"
                        ]
                        []
                    , text "LDC GC"
                    ]
                , div
                    [ Attr.class "w-full bg-color-white rounded-lg shadow md:mt-0 sm:max-w-md xl:p-0"
                    ]
                    [ div
                        [ Attr.class "p-6 space-y-4 md:space-y-6 sm:p-9"
                        ]
                        [ h1
                            [ Attr.class """
                                text-xl
                                font-bold
                                leading-tight
                                tracking-tight
                                text-gray-900
                                md:text-2xl
                                """
                            ]
                            [ text "Sign in to your account" ]
                        , form
                            [ Attr.class "space-y-4 md:space-y-6"
                            , Events.onSubmit UserSubmittedForm
                            , Attr.action "#"
                            ]
                            [ div []
                                [ label
                                    [ Attr.for "email"
                                    , Attr.class """
                                        block
                                        mb-2
                                        text-sm
                                        font-medium
                                        text-gray-900
                                        """
                                    ]
                                    [ text "Your email" ]
                                , input
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
                                        """
                                    , Attr.placeholder "name@domain.com"
                                    , Attr.required True
                                    , Attr.value model.email
                                    , Events.onInput (UserUpdatedInput Email)
                                    ]
                                    []
                                ]
                            , div []
                                [ label
                                    [ Attr.for "password"
                                    , Attr.class """
                                        block
                                        mb-2
                                        text-sm
                                        font-medium
                                        text-gray-900
                                        """
                                    ]
                                    [ text "Password" ]
                                , input
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
                                        """
                                    , Attr.required True
                                    , Attr.value model.password
                                    , Events.onInput (UserUpdatedInput Password)
                                    ]
                                    []
                                ]
                            , List.head model.errors
                                |> Html.viewMaybe
                                    (\error ->
                                        div
                                            [ Attr.class "text-sm text-red-500"
                                            ]
                                            [ text error.message ]
                                    )
                            , div
                                [ Attr.class "flex items-center justify-between"
                                ]
                                [ div
                                    [ Attr.class "flex items-start"
                                    ]
                                    [ div
                                        [ Attr.class "flex items-center h-5"
                                        ]
                                        [ input
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
                                                """
                                            ]
                                            []
                                        ]
                                    , div
                                        [ Attr.class "ml-3 text-sm"
                                        ]
                                        [ label
                                            [ Attr.for "remember"
                                            , Attr.class "text-gray-500"
                                            ]
                                            [ text "Remember me" ]
                                        ]
                                    ]
                                , a
                                    [ Attr.href "#"
                                    , Attr.class """
                                        text-sm
                                        font-medium
                                        text-primary-600
                                        hover:underline
                                        """
                                    ]
                                    [ text "Forgot password?" ]
                                ]
                            , button
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
                                    """
                                , Attr.disabled model.isSubmittingForm
                                ]
                                [ if model.isSubmittingForm then
                                    Spinner.view
                                        [ Attr.class "flex justify-center text-xs"
                                        ]

                                  else
                                    text "Sign in"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }
