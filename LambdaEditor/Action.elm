module Action where

import Model (..)
import Utils (..)
import Native.DrawUtils
import Debug

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
    | MoveLeft
    | MoveRight
    | MoveUp
    | MoveDown

iterate : Action -> EditorState -> EditorState
iterate action state =
    case action of
        MoveLeft ->
            { state | cursor <- moveLeft state.document state.cursor }
        MoveRight ->
            { state | cursor <- moveRight state.document state.cursor }
        MoveUp ->
            { state | cursor <- moveUp state.document state.cursor }
        MoveDown ->
            { state | cursor <- moveDown state.document state.cursor }
        _ -> state

