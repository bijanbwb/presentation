module Main exposing (main)

-- IMPORTS

import Browser
import Data
import Html exposing (..)
import Html.Attributes
import Html.Events
import Markdown
import Slide exposing (Slide)



-- MODEL


type alias Model =
    { current : Maybe Slide
    , slides : List Slide
    }


initialModel : Model
initialModel =
    { current = List.head Data.slides
    , slides = Data.slides
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
    | ClickedSlide Slide


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedButton ->
            ( model
            , Cmd.none
            )
        
        ClickedSlide slide ->
            ( { model | current = Just slide }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header
        , viewSlides model model.slides
        ]


header : Html msg
header =
    h1 [ Html.Attributes.class "text-purple-600 text-5xl" ]
        [ text "Presentation" ]


viewSlides : Model -> List Slide -> Html Msg
viewSlides model slides =
    div []
        (List.map (viewSlide model) slides)


viewSlide : Model -> Slide -> Html Msg
viewSlide model slide =
    let
        borderColor =
            case model.current of
                Just c ->
                    if c == slide then
                        "border-red-600"
                    else
                        "border-gray-600"

                Nothing ->
                    "border-gray-600"
    in
    div [ Html.Attributes.class <| "bg-blue-100 border-solid border-4 h-48 w-48 " ++ borderColor
        , Html.Events.onClick <| ClickedSlide slide
        ]
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
