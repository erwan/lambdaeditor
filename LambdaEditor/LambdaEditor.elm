module LambdaEditor where

import Views (view,updatesChannel)
import Model (..)
import Action (..)
import Keyboard

import Signal
import Html (..)
import Json.Decode as Json


-- PORTS

port setup : Json.Value

stateWithSetup : EditorState
stateWithSetup =
  case Json.decodeValue blocksDecoder setup of
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

updates : Signal Action
updates = Signal.mergeMany [
    Signal.subscribe updatesChannel,
    Signal.map keyboardAction Keyboard.lastPressed
    ]

model : Signal EditorState
model = Signal.foldp iterate stateWithSetup updates

port documentUpdates : Signal Json.Value
port documentUpdates =
  Signal.map (.document >> documentEncoder) model
