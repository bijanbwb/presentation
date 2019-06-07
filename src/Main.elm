module Main exposing (main)

-- IMPORTS

import Html exposing (..)
import Html.Attributes
import Markdown


-- VIEW


main =
    div []
        [ header
        , content
        ]


header : Html msg
header =
    h1 [ Html.Attributes.class "text-purple-600 text-5xl" ]
        [ text "Presentation" ]

content : Html msg
content =
    Markdown.toHtml [ Html.Attributes.class "" ]
        """
## Course Creation Process

- Curriculum: Choose courses.
- Authors: Write courses.
- Engineering: Munge courses.
- DevOps: Ship courses.
        """
