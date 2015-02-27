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
  | Italic

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
  , cursor = { block = 0, x = 200 }
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
    "bold"   -> succeed Bold
    "italic" -> succeed Italic
    _        -> fail "invalid span type"
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
        Bold   -> "bold"
        Italic -> "italic"
    ))
    ]

{-|  get (block number, line number, position in line) of cursor in blocks -}
cursorBlockLine: List Block -> Cursor -> (Int, Int, Int)
cursorBlockLine blocks cursor =
  let
    blockNo = cursor.block
    block = lift blockNo blocks |> M.withDefault (L.head blocks)
    linesWithIndex = (L.indexedMap (,) block.lines)
    (line, position) = cursorLineRec linesWithIndex cursor.x
  in
    (blockNo, line, position)


{-|  get (line, position in line) of cursor position inside a block -}
cursorLineRec: List (Int, Line) -> Int -> (Int, Int)
cursorLineRec lines posInBlock =
  case uncons lines of
    Nothing -> (0, 0)
    Just ((lineIndex, line), rest) ->
      if S.length line > posInBlock then
        (lineIndex, posInBlock)
      else
        if L.isEmpty rest then
          (lineIndex, S.length line)
        else
          cursorLineRec rest (posInBlock - S.length line)

blockLength : Block -> Int
blockLength { lines } =
  L.map S.length lines |> L.sum

moveLeft : Document -> Cursor -> Cursor
moveLeft { blocks } cursor =
  if cursor.x > 0 then
    { block = cursor.block, x = cursor.x - 1 }
  else
    if cursor.block > 0 then
      let
        newBlockIndex = cursor.block - 1
        newBlock: Block
        newBlock = lift newBlockIndex blocks |> M.withDefault (L.head blocks)
      in
        { block = newBlockIndex, x = (blockLength newBlock) }
    else
      { block = 0, x = 0 }

moveRight : Document -> Cursor -> Cursor
moveRight { blocks } cursor =
  let
    currentBlock = lift cursor.block blocks |> M.withDefault (L.head blocks)
    currentBlockLength = blockLength currentBlock
  in
    if cursor.x < currentBlockLength then
      { block = cursor.block, x = cursor.x + 1 }
    else
      if cursor.block < (L.length blocks) then
        { block = cursor.block + 1, x = 0 }
      else
        { block = cursor.block, x = currentBlockLength }

moveUp : Document -> Cursor -> Cursor
moveUp { blocks } cursor =
  let
    (blockNo, lineNo, posInLine) = cursorBlockLine blocks cursor
    { lines } = lift blockNo blocks |> M.withDefault (L.head blocks)
  in
    case (blockNo, lineNo) of
      (0, 0) -> { block = 0, x = 0 }
      (_, 0) ->
        let
          newBlockNo = min 0 (blockNo - 1)
          newBlock = lift newBlockNo blocks |> M.withDefault (L.head blocks)
          startLastLine = L.map S.length (L.take ((L.length newBlock.lines) - 1) newBlock.lines) |> L.sum
          newX = startLastLine + posInLine
        in
          { block = newBlockNo, x = newX }
      _ ->
        let
          prevLine = lift (lineNo - 1) lines |> M.withDefault (L.head lines)
          newX = cursor.x - (S.length prevLine)
        in
          { block = blockNo, x = newX }

moveDown : Document -> Cursor -> Cursor
moveDown { blocks } cursor =
  let
    (blockNo, lineNo, posInLine) = cursorBlockLine blocks cursor
    block = lift blockNo blocks |> M.withDefault (L.head blocks)
    line = lift lineNo block.lines |> M.withDefault (L.head block.lines)
    lastBlockNo = (L.length blocks) - 1
  in
    if lineNo >= (L.length block.lines) - 1 then
      if blockNo >= lastBlockNo then
        { block = blockNo, x = blockLength block }
      else
        { block = blockNo + 1, x = posInLine }
    else
      { block = blockNo, x = cursor.x + (S.length line) }

