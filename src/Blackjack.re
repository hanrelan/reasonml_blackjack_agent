open Cards;

type hand = list(card);

type state =
  | PlayerTurn
  | PlayerLost
  | PlayerWon
  | Draw;

type round = {
  dealer_hand: hand,
  player_hand: hand,
  state,
  deck
};

type action =
  | Hit
  | Stand;

let new_round = () => {
  let deck = full_deck;
  let (dealer_card, deck) = draw_card(deck);
  let (player_card_1, deck) = draw_card(deck);
  let (player_card_2, deck) = draw_card(deck);
  {
    dealer_hand: [dealer_card],
    player_hand: [player_card_1, player_card_2],
    state: PlayerTurn,
    deck
  }
};

let card_value = ((pip, _suit): card) =>
  switch pip {
  | Ace => 11
  | Two => 2
  | Three => 3
  | Four => 4
  | Five => 5
  | Six => 6
  | Seven => 7
  | Eight => 8
  | Nine => 9
  | Ten => 10
  | Jack => 10
  | Queen => 10
  | King => 10
  };

let hand_value = (hand: hand) => List.fold_left((acc, card) => acc + card_value(card), 0, hand);

let rec play_dealer_turn = (round: round) => {
  let (dealer_card, deck) = draw_card(round.deck);
  let dealer_hand = [dealer_card, ...round.dealer_hand];
  let dealer_value = hand_value(dealer_hand);
  let player_value = hand_value(round.player_hand);
  if (dealer_value < 17) {
    play_dealer_turn({...round, dealer_hand, deck})
  } else if (dealer_value > 21) {
    {...round, dealer_hand, deck, state: PlayerWon}
  } else if (dealer_value > player_value) {
    {...round, dealer_hand, deck, state: PlayerLost}
  } else if (dealer_value == player_value) {
    {...round, dealer_hand, deck, state: Draw}
  } else {
    {...round, dealer_hand, deck, state: PlayerWon}
  }
};

let play_hit = (round: round) => {
  let (player_card, deck) = draw_card(round.deck);
  let player_hand = [player_card, ...round.player_hand];
  let player_value = hand_value(player_hand);
  if (player_value > 21) {
    {...round, player_hand, deck, state: PlayerLost}
  } else {
    {...round, player_hand, deck, state: PlayerTurn}
  }
};

let play = (round, action) =>
  switch (round.state, action) {
  | (PlayerLost, _) => round
  | (PlayerWon, _) => round
  | (Draw, _) => round
  | (PlayerTurn, Stand) => play_dealer_turn(round)
  | (PlayerTurn, Hit) => play_hit(round)
  };

let first_round = new_round();