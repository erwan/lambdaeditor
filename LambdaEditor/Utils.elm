module Utils where

import Maybe as M
import String as S
import List (..)
import Debug

-- Maybe

isNothing : Maybe a -> Bool
isNothing m =
  case m of
    Nothing -> True
    _ -> False

isJust : Maybe a -> Bool
isJust m =
  not (isNothing m)


-- List

lift : Int -> List a -> Maybe a
lift n items =
  drop n items |> headMaybe

find : (a -> Bool) -> List a -> Maybe a
find f list =
  let
    filtered = filter f list
  in
    if isEmpty filtered then Nothing else Just (head filtered)

exists : (a -> Bool) -> List a -> Bool
exists f list =
  isJust (find f list)

headMaybe : List a -> Maybe a
headMaybe list =
  if isEmpty list then Nothing else Just (head list)

tail : List a -> a
tail = reverse >> head

uncons : List a -> Maybe (a, List a)
uncons list =
  case list of
    x :: xs -> Just (x, xs)
    [] -> Nothing

unconsWord : String -> Maybe (String, String)
unconsWord input =
  unconsWordRec "" input

unconsWordRec : String -> String -> Maybe (String, String)
unconsWordRec acc input =
  if S.isEmpty acc && S.isEmpty input then
    Nothing
  else
    case S.uncons input of
      Nothing -> Just(acc, "")
      Just(' ', rest) -> Just(acc ++ (S.fromChar ' '), rest)
      Just(c, rest) -> unconsWordRec (acc ++ (S.fromChar c)) rest
