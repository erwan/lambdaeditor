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


updatesChannel : Signal.Channel Action
updatesChannel = Signal.channel NoOp

rawText = T.defaultStyle

spanStyle : SpanType -> T.Style
spanStyle style =
  case style of
    Bold   -> { rawText | bold <- True }
    Italic -> { rawText | italic <- True }

blockStyle : BlockType -> T.Style
blockStyle type_ =
  case type_ of
    Paragraph -> rawText
    Section   -> { rawText | height <- Just 24 }

styleString : T.Style -> String -> T.Text
styleString style text = T.style style (T.fromString text)

buildText : List Span -> BlockType -> Int -> String -> Element
buildText spans blockType offset text =
  let
    -- keep only spans that apply to our text fragment (TODO support spans that are partially on our text fragment)
    filteredSpans =
      spans
        |> L.map (\span -> { span | start <- span.start - offset, end <- span.end - offset })
        |> L.filter (\span -> span.start > 0 && span.end < (S.length text))

    -- find the next span to apply, if any (assumes that spans do not overlap each other)
    min : List Span -> Maybe Span
    min = L.foldl (\span maybeMin ->
        case maybeMin of
          Just minSpan -> if span.start < minSpan.start then Just span else Just minSpan
          Nothing  -> Just span
      ) Nothing

    loop : List Span -> Int -> List T.Text -> List T.Text
    loop spans_ offset_ acc =
      case min spans_ of
        Just span ->
          let prefix = styleString rawText (S.slice offset_ span.start text)
              text_ = styleString (spanStyle span.type_) (S.slice span.start span.end text)
              remainingSpans = L.filter (\span_ -> span_ /= span) spans_
              nextOffset = offset_ + span.end
          in
            [prefix, text_] ++ (loop remainingSpans nextOffset acc)
        Nothing   -> (styleString rawText (S.dropLeft offset_ text)) :: acc

    texts = loop filteredSpans 0 []
  in
    texts |> T.concat >> T.style (blockStyle blockType) >> T.leftAligned

buildLine : List Span -> BlockType -> (Line, Int) -> LineView
buildLine spans blockType (line, offset) =
  { line = line, element = buildText spans blockType offset line }

offsetLines : Int -> List Line -> List (Line, Int)
offsetLines offset lines =
  case lines of
    line :: lines_ -> (line, offset) :: (offsetLines (offset + (S.length line)) lines_)
    []             -> []

buildBlock : Block -> BlockView
buildBlock {lines, spans, type_} =
  { lineViews = L.map (buildLine spans type_) (offsetLines 0 lines) }

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

hiddenInput : Html
hiddenInput =
  input
    [ id "hidden-input"
    , value ""
    , style [("position", "absolute"), ("opacity", "0"), ("width", "0")]
    , on "input" targetValue (Signal.send updatesChannel << UpdateFromBuffer)
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
      [ hiddenInput, fromElement complete ]
