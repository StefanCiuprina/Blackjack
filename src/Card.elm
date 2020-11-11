-----------------------
-- Stefan Ciuprina
-- 10.10.2020
-----------------------

module Card exposing (Card(..), Face(..), Suit(..), cardValue, viewCard, cardToString, deck)

import Html exposing (..)
import Html.Attributes exposing (style)

type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Suit = Clubs | Diamonds | Hearts | Spades
type Card = Card Face Suit

faceToString : Face -> String
faceToString face =
    case face of
        Ace -> "Ace"
        Two -> "2"
        Three -> "3"
        Four -> "4"
        Five -> "5"
        Six -> "6"
        Seven -> "7"
        Eight -> "8"
        Nine -> "9"
        Ten -> "10"
        Jack -> "Jack"
        Queen -> "Queen"
        King -> "King"

suitToString : Suit -> String
suitToString suit =
    case suit of
        Clubs -> "Clubs"
        Diamonds -> "Diamonds"
        Hearts -> "Hearts"
        Spades -> "Spades"

cardToString : Card -> String
cardToString (Card face suit) = faceToString face ++ " of " ++ suitToString suit

cardValue : Card -> List Int
cardValue (Card face _) =
    case face of
        Ace -> [1, 11]
        Two -> [2]
        Three -> [3]
        Four -> [4]
        Five -> [5]
        Six -> [6]
        Seven -> [7]
        Eight -> [8]
        Nine -> [9]
        _ -> [10]

deck : List Card
deck =
    let
        numberToFace number =
            case number of
                1 -> Ace
                2 -> Two
                3 -> Three
                4 -> Four
                5 -> Five
                6 -> Six
                7 -> Seven
                8 -> Eight
                9 -> Nine
                10 -> Ten
                11 -> Jack
                12 -> Queen
                _ -> King
        numberToSuit number =
            case number of
                1 -> Clubs
                2 -> Diamonds
                3 -> Hearts
                _ -> Spades
        deckHelper numberFace numberSuit =
            if numberSuit == 5 then
                []
            else if numberFace <= 13 then
                Card (numberToFace numberFace) (numberToSuit numberSuit) :: (deckHelper (numberFace + 1) numberSuit)
            else
                if numberSuit == 4 then
                    []
                else
                    Card (numberToFace 1) (numberToSuit (numberSuit + 1)) :: (deckHelper 2 (numberSuit + 1))
    in
        deckHelper 1 1

cardToUnicode : Card -> String
cardToUnicode (Card face suit) =
   case face of
     Ace -> case suit of
       Spades ->"🂡"
       Hearts -> "🂱"
       Clubs ->  "🃑"
       Diamonds -> "🃁"
     Two -> case suit of
       Spades ->"🂢"
       Hearts -> "🂲"
       Clubs ->  "🃒"
       Diamonds -> "🃂"
     Three -> case suit of
       Spades ->"🂣"
       Hearts -> "🂳"
       Clubs ->  "🃓"
       Diamonds ->"🃃"
     Four -> case suit of
       Spades ->"🂤"
       Hearts -> "🂴"
       Clubs ->  "🃔"
       Diamonds -> "🃄"
     Five -> case suit of
       Spades ->"🂥"
       Hearts -> "🂵"
       Clubs ->  "🃕"
       Diamonds -> "🃅"
     Six -> case suit of
       Spades ->"🂦"
       Hearts -> "🂶"
       Clubs ->  "🃖"
       Diamonds -> "🃆"
     Seven -> case suit of
       Spades ->"🂧"
       Hearts -> "🂷"
       Clubs ->  "🃗"
       Diamonds -> "🃇"
     Eight -> case suit of
       Spades -> "🂨"
       Hearts ->  "🂸"
       Clubs ->   "🃘"
       Diamonds ->  "🃈"
     Nine -> case suit of
       Spades -> "🂩"
       Hearts ->  "🂹"
       Clubs ->   "🃙"
       Diamonds ->  "🃉"
     Ten -> case suit of
       Spades ->"🂪"
       Hearts -> "🂺"
       Clubs ->  "🃚"
       Diamonds -> "🃊"
     Jack -> case suit of
       Spades ->"🂫"
       Hearts -> "🂻"
       Clubs ->  "🃛"
       Diamonds -> "🃋"
     Queen -> case suit of
       Spades ->"🂭"
       Hearts -> "🂽"
       Clubs ->  "🃝"
       Diamonds -> "🃍"
     King -> case suit of
       Spades -> "🂮"
       Hearts -> "🂾"
       Clubs ->  "🃞"
       Diamonds -> "🃎"

{-
  Modify this function (if needed) to work with your `Card` definition
-}
viewCard : Card -> Html msg
viewCard (Card face suit) =
   let
     suitColor s =
       case s of
         Diamonds -> "red"
         Spades -> "black"
         Hearts -> "red"
         Clubs -> "black"
     unicode = cardToUnicode (Card face suit)
   in
     div [style "display" "inline-block"] [
       div [style "font-size" "12em", style "color" (suitColor suit)] [text unicode],
       div [style "font-size" "0.8em"]  [text (cardToString (Card face suit))]
     ]