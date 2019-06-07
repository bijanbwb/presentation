module Main exposing (main)

-- IMPORTS

import Html exposing (..)
import Html.Attributes


-- VIEW


main =
    div []
        [ header ]


header : Html msg
header =
    h1 [ Html.Attributes.class "text-purple-600 text-5xl" ]
        [ text "Presentation" ]
