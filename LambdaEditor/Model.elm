module Model where

type alias Block = { content : String }

type alias Document = { blocks : List Block }

type alias Cursor = {
    block : Int,
    x : Int
}

type alias EditorState = {
    document : Document,
    cursor : Cursor,
    buffer: String
}

