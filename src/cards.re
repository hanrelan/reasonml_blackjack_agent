type pip =
  | Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King;

type suit =
  | Clubs
  | Hearts
  | Spades
  | Diamonds;

let pips = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King];

let suits = [Clubs, Hearts, Spades, Diamonds];

type card = (pip, suit);

let all_cards =
  Array.of_list(List.concat(List.map((pip) => List.map((suit) => (pip, suit), suits), pips)));

type deck = (array(card), list(card));

let full_deck = (all_cards, []);

let rec draw_card = (deck: deck) => {
  let (cards_available, cards_used) = deck;
  let card = cards_available |> Array.length |> Random.int |> Array.get(cards_available);
  if (List.mem(card, cards_used)) {
    draw_card(deck)
  } else {
    (card, (cards_available, [card, ...cards_used]))
  }
};