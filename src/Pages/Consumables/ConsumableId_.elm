module Pages.Consumables.ConsumableId_ exposing (Model, Msg, page)

-- import Shared.Json exposing (encodeVolunteerDetail)

import Api.Consumables
import Auth
import Components.Button as Button
import Components.Spinner as Spinner
import Components.Toast as Toast
import Dict
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
import Route.Path
import Shared
import Shared.Model exposing (Consumable)
import Time exposing (Weekday(..))
import View exposing (View)


page : Auth.User -> Shared.Model -> Route { consumableId : String } -> Page Model Msg
page user shared route =
    Page.new
        { init = init user shared route
        , update = update user shared
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
        { title = "Consumable Details"
        , user = user
        }



-- INIT


type alias Model =
    { consumable : WebData Consumable
    , editMode : Bool
    }


init : Auth.User -> Shared.Model -> Route { consumableId : String } -> () -> ( Model, Effect Msg )
init user shared route _ =
    let
        editMode : Bool
        editMode =
            Dict.get "edit" route.query == Just "true"
    in
    ( Model Loading editMode
    , Api.Consumables.getDetail
        { onResponse = ConsumablesApiResponded
        , tokens = user.tokens
        , apiUrl = shared.apiUrl
        , consumableId = route.params.consumableId
        }
    )



-- UPDATE


type Msg
    = ConsumablesApiResponded (Result Http.Error Consumable)
    | EditParameterChanged { from : Maybe String, to : Maybe String }
    | SaveChanges Consumable
    | EditConsumableApiResponse (Result Http.Error String)
    | CancelEditMode String


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        ConsumablesApiResponded response ->
            ( { model | consumable = RemoteData.fromResult response }
            , Effect.none
            )

        EditConsumableApiResponse (Ok message) ->
            ( { model | editMode = False }
            , Effect.sendToast message Toast.Success
            )

        EditConsumableApiResponse (Err httpError) ->
            ( { model | consumable = Failure httpError }, Effect.none )

        CancelEditMode consumableId ->
            ( { model | editMode = False }
            , Effect.pushPath <|
                Route.Path.Consumables_ConsumableId_
                    { consumableId = consumableId }
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

        SaveChanges details ->
            ( model
            , Effect.batch
                -- FIXME: [ Api.Consumables.put
                --     { onResponse = EditVolunteerApiResponse
                --     , tokens = user.tokens
                --     , apiUrl = shared.apiUrl
                --     , jsonBody = encodeVolunteerDetail details
                --     , builderAssistantId = details.builderAssistantId
                --     }
                [ Effect.pushPath <|
                    Route.Path.Consumables_ConsumableId_
                        { consumableId = String.fromInt details.id }
                ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Consumable Details"
    , body =
        case model.consumable of
            NotAsked ->
                [ Html.text "Loading..."
                ]

            Loading ->
                [ Spinner.view [ Attr.class "h-full w-full" ]
                ]

            Failure httpError ->
                [ Html.div [ Attr.class "text-red-500" ]
                    [ Html.text <| Api.Consumables.errorToString httpError
                    ]
                ]

            Success { name, barcode, stock, minStock, id } ->
                -- FIXME: display properly all things
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
                        [ Html.text name ]
                    , Html.div [ Attr.class "flex gap-2" ]
                        [ Html.div
                            [ Attr.class "relative inline-flex items-center justify-center w-32 h-32 overflow-hidden bg-gray-100 rounded-full dark:bg-gray-600"
                            ]
                            [ Html.span
                                [ Attr.class "font-medium text-gray-600 dark:text-gray-300"
                                ]
                                [ Html.text name ]
                            ]
                        , Html.div [ Attr.class "flex flex-col gap-2 justify-center align-middle" ]
                            [ Html.text "Volunteer barcode"
                            , Html.span [ Attr.class "text-xl font-mono tracking-widest" ]
                                [ Html.text <| String.fromInt id ]
                            , Html.strong
                                [ Attr.class "text-4xl font-barcode w-fit tracking-widest" ]
                                [ Html.text barcode ]
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
                            ]
                        ]
                    ]
                , Html.viewIf model.editMode <|
                    Html.div [ Attr.class "flex align-center justify-center gap-2" ]
                        [ Button.primary
                            { content = Html.text "Save changes"
                            , onClick =
                                RemoteData.toMaybe model.consumable
                                    |> Maybe.map SaveChanges
                            , disabled = False
                            , attrs = [ Attr.class "w-fit" ]
                            }
                        , Button.secondary
                            { content = "Cancel"
                            , onClick = CancelEditMode <| String.fromInt id
                            , attrs = [ Attr.class "w-48" ]
                            }
                        ]
                ]
    }
