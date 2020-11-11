-----------------------
-- Stefan Ciuprina
-- 10.10.2020
-----------------------

module Main exposing (main, calculateScore)

import Browser
import Html exposing (..)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (..)
import Random


import Card exposing (..)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool
  }

startingModel : Model
startingModel =
    Model [] Card.deck True

init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , Cmd.none
  )


type Msg
  = Draw
  | NewCard Card
  | ToggleDeck
  | Reset


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->
      ( model
      , drawCard model
      )
    
    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewCard newCard ->
      ({hand = List.append model.hand [newCard], deck = List.filter (\x -> x /= newCard) model.deck, showDeck = model.showDeck}
      , Cmd.none
      )

    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToggleDeck ->
      (
        if model.showDeck == True then
            {hand = model.hand, deck = model.deck, showDeck = False}
        else
            {hand = model.hand, deck = model.deck, showDeck = True}
      , Cmd.none
      )
    Reset ->
        (startingModel
        , Cmd.none
        )

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none


--Instead of calculating all the possible scores and then picking up the closest to 21,
--I decided to leave all the aces at the end and add the appropriate value (1 or 11),
--depending on which gets us closer to 21, but doesn't exceed this value, since
--I found this implementation more optimal, the computer computing only one and correct score.
calculateScore : List Card -> Int
calculateScore cards =
    let
        calculateScoreHelper cardsList score numberOfAces =
            case cardsList of
                x::xs ->
                    case cardValue x of
                        [] -> -1 --impossible to get here, needed to complete the case structure
                        y::ys ->
                            if ys == [] then
                                calculateScoreHelper xs (score + y) numberOfAces
                            else --if the list of scores contains more than one element => is an ace
                                calculateScoreHelper xs score (numberOfAces + 1)
                [] -> --we leave the aces at the end, then we choose the appropriate value for each
                    if numberOfAces == 0 then
                        score
                    else
                        if ((21 - score - 1) < (21 - score - 11)) || ((score + 11) > 21) then
                            calculateScoreHelper [] (score + 1) (numberOfAces - 1)
                        else
                            calculateScoreHelper [] (score + 11) (numberOfAces - 1)
    in
        calculateScoreHelper cards 0 0

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"
    cards =
        if model.showDeck == True then
            model.hand
                |> List.map viewCard
        else
            []
    winMessage =
        if calculateScore model.hand == 21 then
            "You've won!"
        else if calculateScore model.hand > 21 then
            "You've lost!"
        else
            ""
  in
    div []
    [
      div [] [ h1 [] [text appName]],
      button [onClick Draw, disabled((calculateScore model.hand)>=21)] [text "Draw card"],
      button [onClick ToggleDeck] [text "Toggle Deck"],
      button [onClick Reset] [text "Reset"],
      div [] cards,
      div [] [text ("score = " ++ String.fromInt (calculateScore model.hand))],
      div [] [text winMessage]
    ]