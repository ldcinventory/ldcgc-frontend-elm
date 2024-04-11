module Pages.Volunteers.BuilderAssistantId_ exposing (Model, Msg, page)

import Api.Volunteers
import Auth
import Components.Spinner as Spinner
import Effect exposing (Effect)
import Html
import Html.Attributes as Attr
import Http
import Layouts
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Auth.User -> Shared.Model -> Route { builderAssistantId : String } -> Page Model Msg
page user shared params =
    Page.new
        { init = init user shared params.params
        , update = update
        , subscriptions = subscriptions
        , view = view
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
    { volunteer : WebData Api.Volunteers.VolunteerDetail }


init : Auth.User -> Shared.Model -> { builderAssistantId : String } -> () -> ( Model, Effect Msg )
init user shared { builderAssistantId } _ =
    ( Model Loading
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


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        VolunteerDetailsApiResponded (Err httpError) ->
            -- TODO: improve the way we deal with errors (maybe move notification stuck to Sidebar layout?)
            ( { model | volunteer = Failure httpError }
            , Effect.none
            )

        VolunteerDetailsApiResponded (Ok volunteer) ->
            ( { model | volunteer = Success volunteer }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Volunteers.BuilderAssistantId_"
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

            Success { name } ->
                [ Html.div []
                    [ Html.text name ]
                ]
    }
