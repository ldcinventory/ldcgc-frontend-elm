module Pages.SignIn exposing (Model, Msg, page)

import Api.SignIn
import Components.Spinner as Spinner
import Css
import Effect exposing (Effect)
import Html.Styled exposing (a, button, div, form, h1, img, input, label, section, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Html.Styled.Extra as Html
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = init
        , update = update
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
    | SignInApiResponded (Result (List Api.SignIn.Error) Api.SignIn.Data)


type Field
    = Email
    | Password


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
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
                }
            )

        SignInApiResponded (Ok { signatureToken, headerPayloadToken }) ->
            ( { model | isSubmittingForm = False }
            , Effect.signIn
                { signatureToken = signatureToken
                , headerPayloadToken = headerPayloadToken
                }
            )

        SignInApiResponded (Err errors) ->
            ( { model | isSubmittingForm = False, errors = errors }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "LDC Login"
    , body =
        [ section
            [ Attr.css
                [ Tw.bg_color Tw.gray_50
                ]
            ]
            [ div
                [ Attr.css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.items_center
                    , Tw.justify_center
                    , Tw.px_6
                    , Tw.py_8
                    , Tw.mx_auto
                    , Bp.lg
                        [ Tw.py_0
                        ]
                    , Bp.md
                        [ Tw.h_screen
                        ]
                    ]
                ]
                [ a
                    [ Attr.href "#"
                    , Attr.css
                        [ Tw.flex
                        , Tw.items_center
                        , Tw.mb_6
                        , Tw.text_2xl
                        , Tw.font_semibold
                        , Tw.text_color Tw.gray_900
                        ]
                    ]
                    [ img
                        [ Attr.css
                            [ Tw.w_8
                            , Tw.h_8
                            , Tw.mr_2
                            ]
                        , Attr.src "/logo.png"
                        , Attr.alt "logo"
                        ]
                        []
                    , text "LDC GC"
                    ]
                , div
                    [ Attr.css
                        [ Tw.w_full
                        , Tw.bg_color Tw.white
                        , Tw.rounded_lg
                        , Tw.shadow
                        , Bp.md
                            [ Tw.mt_0
                            ]
                        , Bp.sm
                            [ Tw.max_w_md
                            ]
                        , Bp.xl
                            [ Tw.p_0
                            ]
                        ]
                    ]
                    [ div
                        [ Attr.css
                            [ Tw.p_6
                            , Tw.space_y_4
                            , Bp.md
                                [ Tw.space_y_6
                                ]
                            , Bp.sm
                                [ Tw.p_8
                                ]
                            ]
                        ]
                        [ h1
                            [ Attr.css
                                [ Tw.text_xl
                                , Tw.font_bold
                                , Tw.leading_tight
                                , Tw.tracking_tight
                                , Tw.text_color Tw.gray_900
                                , Bp.md
                                    [ Tw.text_2xl
                                    ]
                                ]
                            ]
                            [ text "Sign in to your account" ]
                        , form
                            [ Attr.css
                                [ Tw.space_y_4
                                , Bp.md
                                    [ Tw.space_y_6
                                    ]
                                ]
                            , Events.onSubmit UserSubmittedForm
                            , Attr.action "#"
                            ]
                            [ div []
                                [ label
                                    [ Attr.for "email"
                                    , Attr.css
                                        [ Tw.block
                                        , Tw.mb_2
                                        , Tw.text_sm
                                        , Tw.font_medium
                                        , Tw.text_color Tw.gray_900
                                        ]
                                    ]
                                    [ text "Your email" ]
                                , input
                                    [ Attr.type_ "email"
                                    , Attr.name "email"
                                    , Attr.id "email"
                                    , Attr.css
                                        [ Tw.bg_color Tw.gray_50
                                        , Tw.border
                                        , Tw.border_color Tw.gray_300
                                        , Tw.text_color Tw.gray_900
                                        , Tw.rounded_lg
                                        , Tw.block
                                        , Tw.w_full
                                        , Tw.p_2_dot_5
                                        , Css.focus
                                            [ Tw.ring_color Tw.blue_600
                                            , Tw.border_color Tw.blue_600
                                            ]
                                        , Bp.sm
                                            [ Tw.text_sm
                                            ]
                                        ]
                                    , Attr.placeholder "name@company.com"
                                    , Attr.required True
                                    , Attr.value model.email
                                    , Events.onInput (UserUpdatedInput Email)
                                    ]
                                    []
                                ]
                            , div []
                                [ label
                                    [ Attr.for "password"
                                    , Attr.css
                                        [ Tw.block
                                        , Tw.mb_2
                                        , Tw.text_sm
                                        , Tw.font_medium
                                        , Tw.text_color Tw.gray_900
                                        ]
                                    ]
                                    [ text "Password" ]
                                , input
                                    [ Attr.type_ "password"
                                    , Attr.name "password"
                                    , Attr.id "password"
                                    , Attr.placeholder "••••••••"
                                    , Attr.css
                                        [ Tw.bg_color Tw.gray_50
                                        , Tw.border
                                        , Tw.border_color Tw.gray_300
                                        , Tw.text_color Tw.gray_900
                                        , Tw.rounded_lg
                                        , Tw.block
                                        , Tw.w_full
                                        , Tw.p_2_dot_5
                                        , Css.focus
                                            [ Tw.ring_color Tw.blue_600
                                            , Tw.border_color Tw.blue_600
                                            ]
                                        , Bp.sm
                                            [ Tw.text_sm
                                            ]
                                        ]
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
                                            [ Attr.css
                                                [ Tw.text_sm
                                                , Tw.text_color Tw.red_500
                                                ]
                                            ]
                                            [ text error.message ]
                                    )
                            , div
                                [ Attr.css
                                    [ Tw.flex
                                    , Tw.items_center
                                    , Tw.justify_between
                                    ]
                                ]
                                [ div
                                    [ Attr.css
                                        [ Tw.flex
                                        , Tw.items_start
                                        ]
                                    ]
                                    [ div
                                        [ Attr.css
                                            [ Tw.flex
                                            , Tw.items_center
                                            , Tw.h_5
                                            ]
                                        ]
                                        [ input
                                            [ Attr.id "remember"
                                            , Attr.attribute "aria-describedby" "remember"
                                            , Attr.type_ "checkbox"
                                            , Attr.checked True
                                            , Attr.css
                                                [ Tw.w_4
                                                , Tw.h_4
                                                , Tw.border
                                                , Tw.border_color Tw.gray_300
                                                , Tw.rounded
                                                , Tw.bg_color Tw.gray_50
                                                , Css.focus
                                                    [ Tw.ring_2
                                                    , Tw.ring_color Tw.blue_300
                                                    ]
                                                ]
                                            ]
                                            []
                                        ]
                                    , div
                                        [ Attr.css
                                            [ Tw.ml_3
                                            , Tw.text_sm
                                            ]
                                        ]
                                        [ label
                                            [ Attr.for "remember"
                                            , Attr.css
                                                [ Tw.text_color Tw.gray_500
                                                ]
                                            ]
                                            [ text "Remember me" ]
                                        ]
                                    ]
                                , a
                                    [ Attr.href "#"
                                    , Attr.css
                                        [ Tw.text_sm
                                        , Tw.font_medium
                                        , Tw.text_color Tw.blue_600
                                        , Css.hover
                                            [ Tw.underline
                                            ]
                                        ]
                                    ]
                                    [ text "Forgot password?" ]
                                ]
                            , button
                                [ Attr.type_ "submit"
                                , Attr.css
                                    [ Tw.w_full
                                    , Tw.text_color Tw.white
                                    , Tw.bg_color Tw.orange_600 -- FIXME: fix preflight tailwind bugg for type=submit
                                    , Tw.font_medium
                                    , Tw.rounded_lg
                                    , Tw.text_sm
                                    , Tw.px_5
                                    , Tw.py_2_dot_5
                                    , Tw.text_center
                                    , Css.focus
                                        [ Tw.ring_4
                                        , Tw.outline_none
                                        , Tw.ring_color Tw.blue_300
                                        ]
                                    , Css.hover
                                        [ Tw.bg_color Tw.blue_700
                                        ]
                                    , Css.disabled
                                        [ Tw.opacity_50 ]
                                    ]
                                , Attr.disabled model.isSubmittingForm
                                ]
                                [ if model.isSubmittingForm then
                                    Spinner.view
                                        [ Attr.css
                                            [ Tw.flex
                                            , Tw.justify_center
                                            , Tw.text_xs
                                            ]
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
