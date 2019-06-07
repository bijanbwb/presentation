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
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type Change
    = Next
    | Prev


type alias Frame =
    { content : String
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }


type alias Model =
    { current : Maybe Slide
    , currentFrame : Frame
    , frames : List Frame
    , slides : List Slide
    , transition : Maybe Transition
    }


type alias Transition =
    ( Float, Frame )


initialModel : Model
initialModel =
    { current = List.head Data.slides
    , currentFrame =
        { content = "1"
        , x = 0
        , y = 0
        , width = 800
        , height = 400
        }
    , frames =
        [ { content = "2"
          , x = 0
          , y = 0
          , width = 600
          , height = 400
          }
        , { content = "3"
          , x = 0
          , y = 0
          , width = 400
          , height = 400
          }
        , { content = "4"
          , x = 0
          , y = 0
          , width = 200
          , height = 400
          }
        , { content = "5"
          , x = 0
          , y = 0
          , width = 0
          , height = 400
          }
        , { content = "6"
          , x = 0
          , y = 0
          , width = 200
          , height = 400
          }
        ]
    , slides = Data.slides
    , transition = Nothing
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
            case model.transition of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just ( progress, target ) ->
                    if progress == 1 then
                        ( { model | transition = Nothing }
                        , Cmd.none
                        )

                    else
                        let
                            newProgress =
                                progress
                                    + (frame / 2000)
                                    |> min 1

                            newCurrentFrame =
                                { content = target.content
                                , x =
                                    round <|(toFloat model.currentFrame.x) + (toFloat target.x - toFloat model.currentFrame.x) * newProgress
                                , y =
                                    round <| (toFloat model.currentFrame.y) + (toFloat target.y - toFloat model.currentFrame.y) * newProgress
                                , width =
                                    round <| (toFloat model.currentFrame.width) + (toFloat target.width - toFloat model.currentFrame.width) * newProgress
                                , height =
                                    round <| (toFloat model.currentFrame.height) + (toFloat target.height - toFloat model.currentFrame.height) * newProgress
                                }
                        in
                        ( { model
                            | currentFrame = newCurrentFrame
                            , transition = Just ( newProgress, target )
                          }
                        , Cmd.none
                        )

        UserClickedSlide slide ->
            ( { model
                | current = Just slide
                , transition =
                    model.frames
                        |> List.head
                        |> Maybe.map (\first -> ( 0, first ))
                , frames =
                    List.drop 1 model.frames
              }
            , Cmd.none
            )

        UserPressedKeyDown key ->
            case key of
                "ArrowLeft" ->
                    ( changeCurrentSlide Prev model
                    , Cmd.none
                    )

                "ArrowRight" ->
                    ( changeCurrentSlide Next model
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )


changeCurrentSlide : Change -> Model -> Model
changeCurrentSlide change model =
    let
        changeSlideId =
            case model.current of
                Just slide ->
                    case change of
                        Next ->
                            slide.id + 1

                        Prev ->
                            slide.id - 1

                Nothing ->
                    1
    in
    { model
        | current =
            model.slides
                |> List.filter (\s -> s.id == changeSlideId)
                |> List.head
    }



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
        , viewSvg model
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


viewSvg : Model -> Svg.Svg msg
viewSvg model =
    Svg.svg
        [ Svg.Attributes.width "100vw"
        , Svg.Attributes.height "100vh"
        , Svg.Attributes.viewBox <|
            String.fromInt model.currentFrame.x
                ++ " "
                ++ String.fromInt model.currentFrame.y
                ++ " "
                ++ String.fromInt model.currentFrame.width
                ++ " "
                ++ String.fromInt model.currentFrame.height
        ]
        [ Svg.g []
            [ Svg.circle
                [ Svg.Attributes.cx "100"
                , Svg.Attributes.cy "50"
                , Svg.Attributes.r "30"
                , Svg.Attributes.fill "blue"
                ]
                []
            , Svg.text_
                [ Svg.Attributes.x <| String.fromInt model.currentFrame.x
                , Svg.Attributes.y <| String.fromInt (model.currentFrame.y + 100)
                ]
                [ Svg.text model.currentFrame.content ]
            ]
        ]



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
