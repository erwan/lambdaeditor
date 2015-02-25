module Model where

import Utils (..)
import Core (..)

import String as S
import List as L
import Json.Decode (..)
import Json.Encode as E


type alias StructuredText = List StructuredBlock

type alias StructuredBlock =
  { type_: String
  , text: String
  , spans: List StructuredSpan
  }

type alias StructuredSpan =
  { start: Int
  , end: Int
  , type_: String
  }

type alias Line = String

type alias Block = { lines : List Line }

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

toStructuredText : Document -> StructuredText
toStructuredText {blocks} =
  let
    blockConverter b =
      { type_ = "paragraph"
      , text = S.concat b.lines
      , spans = []
      }
  in
    L.map blockConverter blocks

fromStructuredText : StructuredText -> Document
fromStructuredText st =
  let
    extractLines = .text >> (textToLines "" 800) >> Block
    blocks = L.map extractLines st
  in
    { blocks = blocks }


blocksDecoder : Decoder (List Block)
blocksDecoder =
  list blockDecoder

blockDecoder : Decoder Block
blockDecoder =
  object1 Block
    (map (textToLines "" 800) ("text" := string))


documentEncoder : Document -> Value
documentEncoder {blocks} =
  E.object [ ("blocks", E.list (L.map blockEncoder blocks)) ]

blockEncoder : Block -> Value
blockEncoder {lines} =
  E.object
    [ ("type", E.string "paragraph")
    , ("text", E.string (S.concat lines))
    , ("spans", E.list [])
    ]