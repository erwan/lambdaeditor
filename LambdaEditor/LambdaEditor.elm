module LambdaEditor where

import Views (view,updates)
import Model (..)
import Action (..)

import Signal
import Html (..)
import Json.Decode as Json
import Debug


-- INITIAL DATA

--initialModel : EditorState
--initialModel =
--    { document = { blocks = [
--            { content = "Lorem ipsum dolor sit amet, vel quod vocent ei. Omittam nominavi imperdiet nec ei, cum soluta quaerendum te, ex qui nibh malorum. Natum option per ad. Vix inermis perpetua definitiones at, quis adipisci deseruisse vis ex. Deserunt inciderint at per." },
--            { content = "Iuvaret tibique id sit, vel cu iriure sanctus. In purto fuisset indoctum vix. In sumo reprimique vis. Reque tritani oblique eu duo. Est eu paulo labore invidunt, at iusto elitr aperiam nec. At quod aliquid minimum mel, doming omnesque per te. Purto nostrum pri id, ancillae mentitum qui cu, stet impetus prompta his id." }
--        ]
--    }
--    , cursor = { block = 0, x = 0 }
--    , buffer = ""
--    }

-- VIEWS

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