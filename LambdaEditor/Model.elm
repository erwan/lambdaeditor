module Model where

import Utils (..)
import Core (..)

import Maybe as M
import String as S
import List as L
import Json.Decode (..)
import Json.Encode as E


-- temp hardcoded values
lineSize = 800
-- lineStyle = ""

type alias Line = String

type alias Span =
  { start: Int
  , end: Int
  , type_: SpanType
  }

type SpanType
  = Bold
  | Italic

type alias Block =
  { lines : List Line
  , spans : List Span -- list of spans that qualify the text
  , type_: BlockType
  }

type BlockType
  = Paragraph
  | Section

type alias Document = { blocks : List Block }

type alias Pixel = Int

type alias Cursor =
  { block : Int
  , x : Int
  }

type alias EditorState =
  { document : Document
  , cursor: Cursor
  }

emptyBlock =
  { lines = []
  , spans = []
  , type_ = Paragraph}

initialState : EditorState
initialState =
  { document = { blocks = [ ] }
  , cursor = { block = 0, x = 200 }
  }

type alias CursorDocument =
  { blocksBefore: List Block
  , blocksAfter: List Block
  , cursorBlock: Block
  , textBefore: String
  , textAfter: String
  }

blocksDecoder : Decoder (List Block)
blocksDecoder =
  list blockDecoder

blockDecoder : Decoder Block
blockDecoder =
  ("type" := blockTypeDecoder) `andThen` (\type_ ->
    object3 Block
      (linesDecoder type_)
      ("spans" := list spanDecoder)
      (succeed type_)
  )

linesDecoder : BlockType -> Decoder (List Line)
linesDecoder blockType =
  map (textToLines (blockStyle blockType) lineSize) ("text" := string)

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

blockTypeDecoder : Decoder BlockType
blockTypeDecoder =
  string `andThen` (\s -> case s of
    "paragraph" -> succeed Paragraph
    "section"   -> succeed Section
    _           -> fail "invalid block type"
  )

-- HACK compute a style attribute based on the style we assume Elm generates
-- It duplicates the code that is currently in Views.
blockStyle : BlockType -> String
blockStyle blockType =
  case blockType of
    Paragraph -> ""
    Section -> "font-size: 24px;"

documentEncoder : Document -> Value
documentEncoder {blocks} =
  E.object [ ("blocks", E.list (L.map blockEncoder blocks)) ]

blockEncoder : Block -> Value
blockEncoder {lines, spans, type_} =
  E.object
    [ ("type", E.string (case type_ of
        Paragraph -> "paragraph"
        Section  -> "section"
      ))
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


cursorize : Cursor -> Document -> CursorDocument
cursorize cursor {blocks} =
  let
    blocksBefore = L.take cursor.block blocks
    blocksAfter = L.drop (cursor.block + 1) blocks
    cursorBlockMaybe = lift cursor.block blocks
  in
    case cursorBlockMaybe of
      Nothing ->
        { blocksBefore = blocksBefore
        , blocksAfter = blocksAfter
        , cursorBlock = emptyBlock
        , textBefore = ""
        , textAfter = ""
        }
      Just block ->
        let
          allText = S.concat block.lines

          textBefore = S.left cursor.x allText
          textAfter = S.dropLeft cursor.x allText
        in
          { blocksBefore = blocksBefore
          , blocksAfter = blocksAfter
          , cursorBlock = block
          , textBefore = textBefore
          , textAfter = textAfter
          }

uncursorize : CursorDocument -> (Cursor,Document)
uncursorize {blocksBefore,blocksAfter,cursorBlock,textBefore,textAfter} =
  let
    cursorX = S.length textBefore
    blockNo = L.length blocksBefore

    newText = S.concat [textBefore, textAfter]
    newLines = textToLines (blockStyle cursorBlock.type_) lineSize newText

    newBlock = { cursorBlock | lines <- newLines }
    allBlocks = L.concat [blocksBefore, [newBlock], blocksAfter]

    doc = { blocks = allBlocks }
    cursor = { block = blockNo, x = cursorX }
  in
    (cursor,doc)


insertAtCursor : String -> EditorState -> EditorState
insertAtCursor s ({cursor,document} as state) =
  let
    cursorDoc = cursorize cursor document
    {cursorBlock,textBefore} = cursorDoc

    newSpans = L.map (insertInSpan cursor.x) cursorBlock.spans
    newCursorBlock = { cursorBlock | spans <- newSpans }

    newCursorDoc = { cursorDoc
      | textBefore <- (textBefore ++ s)
      , cursorBlock <- newCursorBlock
      }

    (newCursor,newDoc) = uncursorize newCursorDoc
  in
    { state | document <- newDoc, cursor <- newCursor }


insertInSpan : Int -> Span -> Span
insertInSpan x ({start,end} as span) =
  let
    newStart = if x > start then start else start + 1
    newEnd = if x > end then end else end + 1
  in
    { span | start <- newStart, end <- newEnd }


-- TODO deal with blocks merge
deletePreviousCharacter : EditorState -> EditorState
deletePreviousCharacter ({cursor,document} as state) =
  let
    cursorDoc = cursorize cursor document
    {cursorBlock,textBefore} = cursorDoc

    newTextBefore = S.left (S.length textBefore - 1) textBefore
    newSpans = L.map (removeFromSpan cursor.x) cursorBlock.spans
    newCursorBlock = { cursorBlock | spans <- newSpans }

    newCursorDoc = { cursorDoc
      | textBefore <- newTextBefore
      , cursorBlock <- newCursorBlock
      }

    (newCursor,newDoc) = uncursorize newCursorDoc
  in
    { state | document <- newDoc, cursor <- newCursor }


-- TODO deal with blocks merge
deleteNextCharacter : EditorState -> EditorState
deleteNextCharacter ({cursor,document} as state) =
  let
    cursorDoc = cursorize cursor document
    {cursorBlock,textAfter} = cursorDoc

    newTextAfter = S.right (S.length textAfter - 1) textAfter
    newSpans = L.map (removeFromSpan (cursor.x + 1)) cursorBlock.spans
    newCursorBlock = { cursorBlock | spans <- newSpans }

    newCursorDoc = { cursorDoc
      | textAfter <- newTextAfter
      , cursorBlock <- newCursorBlock
      }

    (newCursor,newDoc) = uncursorize newCursorDoc
  in
    { state | document <- newDoc, cursor <- newCursor }

removeFromSpan : Int -> Span -> Span
removeFromSpan x ({start,end} as span) =
  let
    newStart = if x > start then start else start - 1
    newEnd = if x > end then end else end - 1
  in
    { span | start <- newStart, end <- newEnd }

