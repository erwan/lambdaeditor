module Views where

import Model (..)
import Action (..)

import List as L
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Signal


updates : Signal.Channel Action
updates = Signal.channel NoOp

renderLine : Line -> Html
renderLine line =
  div [] [ text line ]

renderBlock : Block -> Html
renderBlock {lines} =
  div [] (L.map renderLine lines)

renderDocument : Document -> List Html
renderDocument {blocks} =
  L.map renderBlock blocks

hiddenInput : String -> Html
hiddenInput content =
  input
    [ id "hidden-input"
    , value content
    , style [("display", "none")]
    , on "input" targetValue (Signal.send updates << UpdateFromBuffer)
    ] []

view : EditorState -> Html
view state =
  div
    [ class "lambda-editor" ]
    ((hiddenInput state.buffer) :: (renderDocument state.document))