module Views where

import Model (..)
import Action (..)
import Utils (..)

import List as L
import Dict as D
import String as S
import Maybe as M

import Graphics.Element (..)
import Graphics.Collage (..)
import Color (black)
import Text as T

import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Signal

import Native.DrawUtils


interblock = 15

cursorWidth = 2

type alias DocumentView = { blockViews: List BlockView }

type alias BlockView = { lineViews: List LineView }

type alias LineView = {
  line: Line,
  element: Element
}


updates : Signal.Channel Action
updates = Signal.channel NoOp


buildLine : Line -> LineView
buildLine line =
  { line = line, element = T.leftAligned (T.fromString line) }

buildBlock : Block -> BlockView
buildBlock {lines} =
  { lineViews = L.map buildLine lines }

buildDocument : Document -> DocumentView
buildDocument {blocks} =
  { blockViews = L.map buildBlock blocks }


renderBlock : BlockView -> Element
renderBlock {lineViews} =
  flow down (L.map .element lineViews)

renderDocument : DocumentView -> Element
renderDocument {blockViews} =
  flow down (L.intersperse (spacer interblock interblock) (L.map renderBlock blockViews))


lineViewHeight : LineView -> Int
lineViewHeight lineView =
  heightOf lineView.element

linePositions : Line -> List Int
linePositions line =
  let
    length = S.length line
    substrings = L.map (\i -> S.left i line) [0..length]
  in
    L.map (\s -> Native.DrawUtils.sizeOf s "") substrings

blockViewHeight : BlockView -> Int
blockViewHeight blockView =
  L.map lineViewHeight blockView.lineViews |> L.sum

cursorElement : Int -> Element
cursorElement height =
  rect (toFloat cursorWidth) (toFloat height)
  |> filled black
  |> (\f -> [f])
  |> collage cursorWidth height

buildCursor : Document -> DocumentView -> Cursor -> Element
buildCursor { blocks } {blockViews} cursor =
  let
    (blockNo, lineNo, posInLine) = cursorBlockLine blocks cursor

    blocksBefore = L.take blockNo blockViews
    blockView = lift blockNo blockViews |> M.withDefault (L.head blockViews)

    linesBefore = L.take lineNo blockView.lineViews
    lineView = lift lineNo blockView.lineViews |> M.withDefault (L.head blockView.lineViews)

    pixelX = lift posInLine (linePositions lineView.line) |> M.withDefault 0
    pixelY = (L.map blockViewHeight blocksBefore |> L.sum) + (blockNo * interblock) + (L.map lineViewHeight linesBefore |> L.sum)

    cursorElt = cursorElement (lineViewHeight lineView)
  in
    container (pixelX + 20) (pixelY + 20) (topLeftAt (Absolute pixelX) (Absolute pixelY)) cursorElt


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
  let
    documentView = buildDocument state.document
    cursorView = buildCursor state.document documentView state.cursor
    complete = layers [(renderDocument documentView), cursorView]
  in
    div
      [ class "lambda-editor" ]
      [ (hiddenInput state.buffer), fromElement complete ]
