// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var List                 = require("bs-platform/lib/js/list.js");
var Cards$Blackjackagent = require("./cards.bs.js");

function new_round() {
  var match = Cards$Blackjackagent.draw_card(Cards$Blackjackagent.full_deck);
  var match$1 = Cards$Blackjackagent.draw_card(match[1]);
  var match$2 = Cards$Blackjackagent.draw_card(match$1[1]);
  return /* record */[
          /* dealer_hand : :: */[
            match[0],
            /* [] */0
          ],
          /* player_hand : :: */[
            match$1[0],
            /* :: */[
              match$2[0],
              /* [] */0
            ]
          ],
          /* state : PlayerTurn */0,
          /* deck */match$2[1]
        ];
}

function card_value(param) {
  switch (param[0]) {
    case 0 : 
        return 11;
    case 1 : 
        return 2;
    case 2 : 
        return 3;
    case 3 : 
        return 4;
    case 4 : 
        return 5;
    case 5 : 
        return 6;
    case 6 : 
        return 7;
    case 7 : 
        return 8;
    case 8 : 
        return 9;
    case 9 : 
    case 10 : 
    case 11 : 
    case 12 : 
        return 10;
    
  }
}

function hand_value(hand) {
  return List.fold_left((function (acc, card) {
                return acc + card_value(card) | 0;
              }), 0, hand);
}

function play_dealer_turn(_round) {
  while(true) {
    var round = _round;
    var match = Cards$Blackjackagent.draw_card(round[/* deck */3]);
    var deck = match[1];
    var dealer_hand_000 = match[0];
    var dealer_hand_001 = round[/* dealer_hand */0];
    var dealer_hand = /* :: */[
      dealer_hand_000,
      dealer_hand_001
    ];
    var dealer_value = hand_value(dealer_hand);
    var player_value = hand_value(round[/* player_hand */1]);
    if (dealer_value < 17) {
      _round = /* record */[
        /* dealer_hand */dealer_hand,
        /* player_hand */round[/* player_hand */1],
        /* state */round[/* state */2],
        /* deck */deck
      ];
      continue ;
      
    } else if (dealer_value > 21) {
      return /* record */[
              /* dealer_hand */dealer_hand,
              /* player_hand */round[/* player_hand */1],
              /* state : PlayerWon */2,
              /* deck */deck
            ];
    } else if (dealer_value > player_value) {
      return /* record */[
              /* dealer_hand */dealer_hand,
              /* player_hand */round[/* player_hand */1],
              /* state : PlayerLost */1,
              /* deck */deck
            ];
    } else if (dealer_value === player_value) {
      return /* record */[
              /* dealer_hand */dealer_hand,
              /* player_hand */round[/* player_hand */1],
              /* state : Draw */3,
              /* deck */deck
            ];
    } else {
      return /* record */[
              /* dealer_hand */dealer_hand,
              /* player_hand */round[/* player_hand */1],
              /* state : PlayerWon */2,
              /* deck */deck
            ];
    }
  };
}

function play_hit(round) {
  var match = Cards$Blackjackagent.draw_card(round[/* deck */3]);
  var deck = match[1];
  var player_hand_000 = match[0];
  var player_hand_001 = round[/* player_hand */1];
  var player_hand = /* :: */[
    player_hand_000,
    player_hand_001
  ];
  var player_value = hand_value(player_hand);
  if (player_value > 21) {
    return /* record */[
            /* dealer_hand */round[/* dealer_hand */0],
            /* player_hand */player_hand,
            /* state : PlayerLost */1,
            /* deck */deck
          ];
  } else {
    return /* record */[
            /* dealer_hand */round[/* dealer_hand */0],
            /* player_hand */player_hand,
            /* state : PlayerTurn */0,
            /* deck */deck
          ];
  }
}

function play(round, action) {
  var match = round[/* state */2];
  if (match !== 0) {
    return round;
  } else if (action !== 0) {
    return play_dealer_turn(round);
  } else {
    return play_hit(round);
  }
}

var first_round = new_round(/* () */0);

exports.new_round        = new_round;
exports.card_value       = card_value;
exports.hand_value       = hand_value;
exports.play_dealer_turn = play_dealer_turn;
exports.play_hit         = play_hit;
exports.play             = play;
exports.first_round      = first_round;
/* first_round Not a pure module */