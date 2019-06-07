module Main exposing (main)

-- IMPORTS

import Browser
import Data
import Html exposing (..)
import Html.Attributes
import Markdown
import Slide exposing (Slide)



-- MODEL


type alias Model =
    { slides : List Slide
    }


initialModel : Model
initialModel =
    { slides = Data.slides
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , initialCommand
    )



-- UPDATE


type Msg
    = ClickedButton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedButton ->
            ( model
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html msg
view model =
    div []
        [ header
        , viewSlides model.slides
        ]


header : Html msg
header =
    h1 [ Html.Attributes.class "text-purple-600 text-5xl" ]
        [ text "Presentation" ]


viewSlides slides =
    div []
        (List.map viewSlide slides)


viewSlide : Slide -> Html msg
viewSlide slide =
    div []
        []



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
