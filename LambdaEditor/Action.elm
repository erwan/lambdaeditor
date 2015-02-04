module Action where

import Model (..)

-- See in the state where the cursor is located to know where the operation should be applied
type Action
    = NoOp
    | Insert String
    | SplitBlock
    | MergeBlocks
    | DeletePreviousCharacter
    | DeleteNextCharacter

step : Action -> EditorState -> EditorState
step action state =
    state

