module Core where

import Debug
import Utils (..)
import Native.DrawUtils

import String as S
import List as L


textToLines : String -> Int -> String -> List String
textToLines style lineSize text =
  lineExtractor text style lineSize [""] |> L.reverse

lineExtractor : String -> String -> Int -> List String -> List String
lineExtractor text style lineSize linesAcc =
  case S.uncons text of
    Nothing ->
      linesAcc
    Just (c, remainingText) ->
      case uncons linesAcc of
        Nothing ->
          linesAcc
        Just (currentLine, otherLines) ->
          let
            candidate = currentLine ++ (S.fromChar c)
            candidateSize = Native.DrawUtils.sizeOf candidate style
          in
            if candidateSize <= lineSize then
              lineExtractor remainingText style lineSize (candidate :: otherLines)
            else
              lineExtractor remainingText style lineSize (S.fromChar c :: linesAcc)
