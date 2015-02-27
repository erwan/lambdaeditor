module Model where

import Utils (..)
import Core (..)

import Maybe as M
import String as S
import List as L
import Json.Decode (..)
import Json.Encode as E

type alias Line = String

type alias Span =
  { start: Int
  , end: Int
  , type_: SpanType
  }

type SpanType
  = Bold -- just bold for now

type alias Block =
  { lines : List Line
  , spans : List Span -- list of spans that qualify the text
  }

type alias Document = { blocks : List Block }

type alias Pixel = Int

type alias Cursor =
  { block : Int
  , x : Int
  }

type alias EditorState =
  { document : Document
  , cursor: Cursor
  , buffer: String
  }

initialState : EditorState
initialState =
  { document = { blocks = [ ] }
  , cursor = { block = 0, x = 0 }
  , buffer = ""
  }

blocksDecoder : Decoder (List Block)
blocksDecoder =
  list blockDecoder

blockDecoder : Decoder Block
blockDecoder =
  object2 Block linesDecoder ("spans" := list spanDecoder)

linesDecoder : Decoder (List Line)
linesDecoder = map (textToLines "" 800) ("text" := string)

spanDecoder : Decoder Span
spanDecoder =
  object3 Span
    ("start" := int)
    ("end" := int)
    ("type" := spanTypeDecoder)

spanTypeDecoder : Decoder SpanType
spanTypeDecoder =
  string `andThen` (\s -> case s of
    "bold" -> succeed Bold
    _      -> fail "invalid span type"
  )

documentEncoder : Document -> Value
documentEncoder {blocks} =
  E.object [ ("blocks", E.list (L.map blockEncoder blocks)) ]

blockEncoder : Block -> Value
blockEncoder {lines, spans} =
  E.object
    [ ("type", E.string "paragraph")
    , ("text", E.string (S.concat lines))
    , ("spans", E.list (L.map spanEncoder spans))
    ]

spanEncoder : Span -> Value
spanEncoder {start, end, type_} =
  E.object
    [ ("start", E.int start)
    , ("end", E.int end)
    , ("type", E.string (case type_ of
      Bold -> "bold"
    ))
    ]
cursorBlockLine: List Block -> Cursor -> (Int, Int, Int)
cursorBlockLine blocks cursor =
  let
    blockNo = cursor.block
    block = lift blockNo blocks |> M.withDefault (L.head blocks)
    linesWithIndex : List (Int, Line)
    linesWithIndex = (L.indexedMap (,) block.lines)
    (line, position) = cursorLineRec linesWithIndex cursor cursor.x
  in
    (blockNo, line, position)


cursorLineRec: List (Int, Line) -> Cursor -> Int -> (Int, Int)
cursorLineRec lines cursor posInBlock =
  case uncons lines of
    Nothing -> (0, 0)
    Just ((index, first), rest) ->
      if S.length first > posInBlock then
        (index, S.length first - posInBlock)
      else
        if L.isEmpty rest then
          (index, S.length first)
        else
          cursorLineRec rest cursor (posInBlock - S.length first)

