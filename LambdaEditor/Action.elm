module Action where

import Model (..)
import Utils (..)
import Native.DrawUtils

import String as S
import List as L
import Json.Decode as Json

-- See in the state where the cursor is located to know where the operation should be applied
type Action
    = NoOp
    | Reset Json.Value
    | UpdateFromBuffer String
    | SplitBlock
    | MergeBlocks
    | DeletePreviousCharacter
    | DeleteNextCharacter

iterate : Action -> EditorState -> EditorState
iterate action state =
    state
