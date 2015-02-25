module LambdaEditor where

import Views (view,updates)
import Model (..)
import Action (..)

import Signal
import Html (..)
import Json.Decode as Json
import Debug


port setup : StructuredText

stateWithSetup = { initialState | document <- fromStructuredText setup }

main : Signal Html
main = Signal.map view model

model : Signal EditorState
model = Signal.foldp iterate stateWithSetup (Signal.subscribe updates)

port documentUpdates : Signal StructuredText
port documentUpdates =
  Signal.map (.document >> toStructuredText) model
