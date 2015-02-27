module LambdaEditor where

import Views (view,updates)
import Model (..)
import Action (..)

import Signal
import Html (..)
import Json.Decode as Json
import Debug

-- PORTS

port setup : Json.Value

stateWithSetup : EditorState
stateWithSetup =
  case Json.decodeValue blocksDecoder (Debug.log "setup" setup) of
    Err e ->
       initialState
    Ok blocks ->
      let
        document = { blocks = blocks }
      in
        { initialState | document <- document }


-- MAIN BLOCK

main : Signal Html
main = Signal.map view model

model : Signal EditorState
model = Signal.foldp iterate stateWithSetup (Signal.subscribe updates)


port documentUpdates : Signal Json.Value
port documentUpdates =
  Signal.map (.document >> documentEncoder) model