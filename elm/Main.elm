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


viewSlides : List Slide -> Html msg
viewSlides slides =
    div []
        (List.map viewSlide slides)


viewSlide : Slide -> Html msg
viewSlide slide =
    div [ Html.Attributes.class "bg-blue-100 border-solid border-4 border-gray-600 h-48 w-48" ]
        [ h2 [ Html.Attributes.class "text-4xl" ]
            [ text slide.title ]
        , span [ Html.Attributes.class "text-xs" ]
            [ text <| String.fromInt slide.id ]
        ]



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
