module View where

import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Text (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List
import List ((::))
import Mouse
import Signal
import Window

import Model (..)

renderBlock : Block -> Html
renderBlock block =
  div [] [ text block.content ]

renderDocument : Document -> Html
renderDocument doc =
  div
    [ class "lambda-editor" ]
    (List.map renderBlock doc.blocks)

view : EditorState -> Html
view state =
  renderDocument state.document
