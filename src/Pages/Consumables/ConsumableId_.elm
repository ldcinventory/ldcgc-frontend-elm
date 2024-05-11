module Pages.Consumables.ConsumableId_ exposing (Model, Msg, page)

import Api.Consumables
import Auth
import Components.Button as Button
import Components.Spinner as Spinner
import Components.Toast as Toast
import Dict
import Effect exposing (Effect)
import Html
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
import Shared.Json exposing (encodeConsumable)
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
    = CancelEditMode String
    | SaveChanges Consumable
    | InputChanged (Consumable -> Consumable)
    | EditConsumableApiResponse (Result Http.Error String)
    | ConsumablesApiResponded (Result Http.Error Consumable)
    | EditParameterChanged { from : Maybe String, to : Maybe String }


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

        InputChanged fn ->
            ( { model | consumable = RemoteData.map fn model.consumable }
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

        SaveChanges details ->
            ( model
            , Effect.batch
                [ Api.Consumables.put
                    { onResponse = EditConsumableApiResponse
                    , tokens = user.tokens
                    , apiUrl = shared.apiUrl
                    , jsonBody = encodeConsumable details
                    , consumableId = String.fromInt details.id
                    }
                , Effect.pushPath <|
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

            Success consumable ->
                [ Html.div
                    [ Attr.class "flex p-3 gap-6 justify-start flex-col md:flex-row" ]
                    [ Html.div [ Attr.class "flex flex-col" ]
                        [ Html.h2
                            [ Attr.class "text-4xl font-extrabold dark:text-white pb-3"
                            ]
                            [ Html.text <| consumable.name ++ " "
                            , Html.viewIf (not model.editMode) <|
                                Html.a
                                    [ Attr.href "?edit=true"
                                    , Attr.class "text-blue-500 hover:text-blue-700"
                                    ]
                                    [ Html.text "Edit" ]
                            ]
                        , Html.div [ Attr.class "flex gap-2" ]
                            [ List.head consumable.urlImages
                                |> Html.viewMaybe
                                    (\url ->
                                        Html.div
                                            [ Attr.class "relative inline-flex items-center justify-center w-32 h-32 overflow-hidden bg-gray-100 rounded-full dark:bg-gray-600"
                                            ]
                                            [ Html.img
                                                [ Attr.src url
                                                , Attr.class "h-14 w-14 rounded"
                                                , Attr.alt consumable.name
                                                ]
                                                []
                                            ]
                                    )
                            , Html.div [ Attr.class "flex flex-col gap-2 justify-center align-middle" ]
                                [ Html.text "Consumable barcode"
                                , Html.span [ Attr.class "text-xl font-mono tracking-widest" ]
                                    [ Html.text consumable.barcode ]
                                , Html.strong
                                    [ Attr.class "text-4xl font-barcode w-fit tracking-widest" ]
                                    [ Html.text consumable.barcode ]
                                ]
                            ]
                        ]
                    , Html.form []
                        [ Html.div
                            [ Attr.class "grid gap-6 mb-6 md:grid-cols-2"
                            ]
                            [ Html.div []
                                [ Html.label
                                    [ Attr.for "model"
                                    , Attr.class "block mb-2 text-sm font-bold text-gray-900 dark:text-white"
                                    ]
                                    [ Html.text "Model" ]
                                , if model.editMode then
                                    Html.input
                                        [ Attr.type_ "text"
                                        , Attr.id "model"
                                        , Events.onInput <| \str -> InputChanged (\c -> { c | model = str })
                                        , Attr.class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                                        , Attr.value consumable.model
                                        ]
                                        []

                                  else
                                    Html.span [] [ Html.text consumable.model ]
                                ]
                            , Html.div []
                                [ Html.label
                                    [ Attr.for "description"
                                    , Attr.class "block mb-2 text-sm font-medium text-gray-900 dark:text-white"
                                    ]
                                    [ Html.text "Description" ]
                                , if model.editMode then
                                    Html.input
                                        [ Attr.type_ "text"
                                        , Attr.id "description"
                                        , Events.onInput <| \str -> InputChanged (\c -> { c | description = str })
                                        , Attr.class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                                        , Attr.value consumable.description
                                        ]
                                        []

                                  else
                                    Html.span [] [ Html.text consumable.description ]
                                ]
                            , Html.div []
                                [ Html.label
                                    [ Attr.for "price"
                                    , Attr.class "block mb-2 text-sm font-medium text-gray-900 dark:text-white"
                                    ]
                                    [ Html.text "Price" ]
                                , if model.editMode then
                                    Html.input
                                        [ Attr.type_ "number"
                                        , Attr.id "price"
                                        , Attr.min "0"
                                        , Events.onInput <| \str -> InputChanged (\c -> { c | price = str |> String.toFloat |> Maybe.withDefault c.price })
                                        , Attr.class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                                        , Attr.value <| String.fromFloat consumable.price
                                        ]
                                        []

                                  else
                                    Html.span [] [ Html.text <| String.fromFloat consumable.price ++ " €" ]
                                ]
                            , Html.div []
                                [ Html.label
                                    [ Attr.for "stock"
                                    , Attr.class "block mb-2 text-sm font-medium text-gray-900 dark:text-white"
                                    ]
                                    [ Html.text "Stock" ]
                                , if model.editMode then
                                    Html.input
                                        [ Attr.type_ "number"
                                        , Attr.id "stock"
                                        , Attr.min "0"
                                        , Events.onInput <| \str -> InputChanged (\c -> { c | stock = str |> String.toFloat |> Maybe.withDefault c.stock })
                                        , Attr.class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                                        , Attr.value <| String.fromFloat consumable.stock
                                        ]
                                        []

                                  else
                                    Html.span [] [ Html.text <| String.fromFloat consumable.stock ]
                                ]
                            , Html.div []
                                [ Html.label
                                    [ Attr.for "min-stock"
                                    , Attr.class "block mb-2 text-sm font-medium text-gray-900 dark:text-white"
                                    ]
                                    [ Html.text "Min. Stock" ]
                                , if model.editMode then
                                    Html.input
                                        [ Attr.type_ "number"
                                        , Attr.id "min-stock"
                                        , Attr.min "0"
                                        , Events.onInput <| \str -> InputChanged (\c -> { c | minStock = str |> String.toFloat |> Maybe.withDefault c.minStock })
                                        , Attr.class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                                        , Attr.value <| String.fromFloat consumable.minStock
                                        ]
                                        []

                                  else
                                    Html.span [] [ Html.text <| String.fromFloat consumable.minStock ]
                                ]
                            , Html.div []
                                [ Html.label
                                    [ Attr.for "quantity-each"
                                    , Attr.class "block mb-2 text-sm font-medium text-gray-900 dark:text-white"
                                    ]
                                    [ Html.text "Quantity each" ]
                                , if model.editMode then
                                    Html.input
                                        [ Attr.type_ "number"
                                        , Attr.id "quantity-each"
                                        , Attr.min "0"
                                        , Events.onInput <| \str -> InputChanged (\c -> { c | quantityEachItem = str |> String.toFloat |> Maybe.withDefault c.quantityEachItem })
                                        , Attr.class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                                        , Attr.value <| String.fromFloat consumable.quantityEachItem
                                        ]
                                        []

                                  else
                                    Html.span [] [ Html.text <| String.fromFloat consumable.quantityEachItem ]
                                ]
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
                            , onClick = CancelEditMode <| String.fromInt consumable.id
                            , attrs = [ Attr.class "w-48" ]
                            }
                        ]
                ]
    }
