module Main exposing (main)

-- IMPORTS

import Browser
import Browser.Events
import Data
import Html exposing (..)
import Html.Attributes
import Html.Events
import Json.Decode
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
    = BrowserRenderedFrame Float
    | UserClickedSlide Slide
    | UserPressedKeyDown String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserRenderedFrame frame ->
            ( model
            , Cmd.none
            )

        UserClickedSlide slide ->
            ( { model | current = Just slide }
            , Cmd.none
            )

        UserPressedKeyDown key ->
            let
                next =
                    case model.current of
                        Just c ->
                            c.id + 1

                        Nothing ->
                            0

                prev =
                    case model.current of
                        Just c ->
                            c.id - 1

                        Nothing ->
                            0
            in
            case key of
                "ArrowLeft" ->
                    ( { model
                        | current =
                            model.slides
                                |> List.filter (\s -> s.id == prev)
                                |> List.head
                      }
                    , Cmd.none
                    )

                "ArrowRight" ->
                    ( { model
                        | current =
                            model.slides
                                |> List.filter (\s -> s.id == next)
                                |> List.head
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (Json.Decode.map UserPressedKeyDown keyDecoder)
        , Browser.Events.onAnimationFrameDelta BrowserRenderedFrame
        ]


keyDecoder : Json.Decode.Decoder String
keyDecoder =
    Json.Decode.field "key" Json.Decode.string



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
    div
        [ Html.Attributes.class <| "bg-blue-100 border-solid border-4 h-48 w-48 " ++ borderColor
        , Html.Events.onClick <| UserClickedSlide slide
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
