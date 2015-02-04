module LambdaEditor where

import Html (..)
import Model (..)
import View (..)
import Signal

type Action
    = NoOp
    | Insert String

update : Action -> EditorState -> EditorState
update action model =
    model

initialModel : EditorState
initialModel = {
        document = { blocks = [
            { content = "Lorem ipsum dolor sit amet, vel quod vocent ei. Omittam nominavi imperdiet nec ei, cum soluta quaerendum te, ex qui nibh malorum. Natum option per ad. Vix inermis perpetua definitiones at, quis adipisci deseruisse vis ex. Deserunt inciderint at per." },
            { content = "Iuvaret tibique id sit, vel cu iriure sanctus. In purto fuisset indoctum vix. In sumo reprimique vis. Reque tritani oblique eu duo. Est eu paulo labore invidunt, at iusto elitr aperiam nec. At quod aliquid minimum mel, doming omnesque per te. Purto nostrum pri id, ancillae mentitum qui cu, stet impetus prompta his id." }
        ]},
        cursor = { block = 0, x = 0 }
    }

main : Signal Html
main = Signal.map view model

model : Signal EditorState
model = Signal.foldp update initialModel (Signal.subscribe updates)

updates : Signal.Channel Action
updates = Signal.channel NoOp
