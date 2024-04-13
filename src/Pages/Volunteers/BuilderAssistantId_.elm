module Pages.Volunteers.BuilderAssistantId_ exposing (Model, Msg, page)

import Api.Volunteers
import Auth
import Components.Button as Button
import Components.Spinner as Spinner
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Attributes.Extra as Attr
import Html.Events as Events
import Html.Extra as Html
import Http
import Layouts
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Set.Any as Set
import Shared
import Time exposing (Weekday(..))
import View exposing (View)


page : Auth.User -> Shared.Model -> Route { builderAssistantId : String } -> Page Model Msg
page user shared params =
    Page.new
        { init = init user shared params.params
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withOnQueryParameterChanged
            { key = "edit"
            , onChange = EditParameterChanged
            }
        |> Page.withLayout (toLayout user)


toLayout : Auth.User -> Model -> Layouts.Layout msg
toLayout user _ =
    Layouts.Sidebar
        { title = "Volunteer Details"
        , user = user
        }



-- INIT


type alias Model =
    { volunteer : WebData Api.Volunteers.VolunteerDetail
    , editMode : Bool
    }


init : Auth.User -> Shared.Model -> { builderAssistantId : String } -> () -> ( Model, Effect Msg )
init user shared { builderAssistantId } _ =
    ( Model Loading False
    , Api.Volunteers.getDetail
        { onResponse = VolunteerDetailsApiResponded
        , tokens = user.tokens
        , apiUrl = shared.apiUrl
        , builderAssistantId = builderAssistantId
        }
    )



-- UPDATE


type Msg
    = VolunteerDetailsApiResponded (Result Http.Error Api.Volunteers.VolunteerDetail)
    | EditParameterChanged { from : Maybe String, to : Maybe String }
    | ToggleAvailableDay Time.Weekday


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        VolunteerDetailsApiResponded response ->
            ( { model | volunteer = RemoteData.fromResult response }
            , Effect.none
            )

        EditParameterChanged query ->
            case query.to of
                Just "true" ->
                    ( { model | editMode = True }
                    , Effect.none
                    )

                _ ->
                    ( { model | editMode = False }
                    , Effect.none
                    )

        ToggleAvailableDay day ->
            ( { model
                | volunteer =
                    RemoteData.map
                        (\detail -> { detail | availability = Set.toggle day detail.availability })
                        model.volunteer
              }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW
-- TODO: fix availability on mobile
-- TODO: add list of absences (CRUD)


viewWeekDay : Set.AnySet String Time.Weekday -> Bool -> Time.Weekday -> Html Msg
viewWeekDay availability editMode day =
    Html.div
        [ Attr.class "flex group hover:bg-blue-500 hover:bg-opacity-75 hover:shadow-lg rounded-lg mx-1 transition-all duration-150 justify-center w-16"
        , Attr.classList
            [ ( "bg-blue-600", Set.member day availability )
            , ( "cursor-pointer", editMode )
            ]
        , Attr.attributeIf editMode <| Events.onClick <| ToggleAvailableDay day
        ]
        [ Html.div
            [ Attr.class "flex items-center px-4 py-4"
            ]
            [ Html.div
                [ Attr.class "text-center"
                ]
                [ Html.p
                    [ Attr.class "text-gray-100 text-sm"
                    ]
                    [ Html.text <| Api.Volunteers.toEnglishWeekday day ]
                ]
            ]
        ]


view : Model -> View Msg
view model =
    { title = "Volunteer Details"
    , body =
        case model.volunteer of
            NotAsked ->
                [ Html.text "Loading..."
                ]

            Loading ->
                [ Spinner.view [ Attr.class "h-full w-full" ]
                ]

            Failure httpError ->
                [ Html.div [ Attr.class "text-red-500" ]
                    [ Html.text <| Api.Volunteers.errorToString httpError
                    ]
                ]

            Success { name, lastName, availability, builderAssistantId } ->
                [ Html.div
                    [ Attr.class "p-3 flex flex-col gap-4 items-start" ]
                    [ Html.viewIf (not model.editMode) <|
                        Html.a
                            [ Attr.href "?edit=true"
                            , Attr.class "text-blue-500 hover:text-blue-700"
                            ]
                            [ Html.text "Edit" ]
                    , Html.h2
                        [ Attr.class "text-4xl font-extrabold dark:text-white"
                        ]
                        [ Html.text <| name ++ " " ++ lastName ]
                    , Html.div [ Attr.class "flex gap-2" ]
                        [ Html.div
                            [ Attr.class "relative inline-flex items-center justify-center w-32 h-32 overflow-hidden bg-gray-100 rounded-full dark:bg-gray-600"
                            ]
                            [ Html.span
                                [ Attr.class "font-medium text-gray-600 dark:text-gray-300"
                                ]
                                [ Html.text <| String.slice 0 1 name ++ String.slice 0 1 lastName ]
                            ]
                        , Html.div [ Attr.class "flex flex-col gap-2 justify-center align-middle" ]
                            [ Html.text "Volunteer barcode"
                            , Html.span [ Attr.class "text-xl font-mono tracking-widest" ]
                                [ Html.text builderAssistantId ]
                            , Html.strong
                                [ Attr.class "text-4xl font-barcode w-fit tracking-widest" ]
                                [ Html.text builderAssistantId ]
                            ]
                        ]
                    , Html.div
                        [ Attr.attribute "classname" "h-screen bg-gray-100 p-6"
                        ]
                        [ Html.div
                            [ Attr.class "w-full max-w-screen-sm mx-auto"
                            ]
                            [ Html.h3
                                [ Attr.class "text-2xl font-extrabold dark:text-white mb-2"
                                ]
                                [ Html.text "Availability" ]
                            , Html.div
                                [ Attr.class "flex bg-gray-800 shadow-md justify-start md:justify-center rounded-lg mx-auto py-4 px-2"
                                ]
                                ([ Mon, Tue, Wed, Thu, Fri, Sat, Sun ]
                                    |> List.map (viewWeekDay availability model.editMode)
                                )
                            ]
                        ]
                    ]
                , Html.viewIf model.editMode <|
                    Button.primary
                        { content = Html.text "Save changes"
                        , onClick = Nothing
                        , disabled = False
                        , attrs = [ Attr.class "w-fit" ]
                        }
                ]
    }
