// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var List       = require("bs-platform/lib/js/list.js");
var $$Array    = require("bs-platform/lib/js/array.js");
var Random     = require("bs-platform/lib/js/random.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");

var pips = /* :: */[
  /* Ace */0,
  /* :: */[
    /* Two */1,
    /* :: */[
      /* Three */2,
      /* :: */[
        /* Four */3,
        /* :: */[
          /* Five */4,
          /* :: */[
            /* Six */5,
            /* :: */[
              /* Seven */6,
              /* :: */[
                /* Eight */7,
                /* :: */[
                  /* Nine */8,
                  /* :: */[
                    /* Ten */9,
                    /* :: */[
                      /* Jack */10,
                      /* :: */[
                        /* Queen */11,
                        /* :: */[
                          /* King */12,
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
];

var suits = /* :: */[
  /* Clubs */0,
  /* :: */[
    /* Hearts */1,
    /* :: */[
      /* Spades */2,
      /* :: */[
        /* Diamonds */3,
        /* [] */0
      ]
    ]
  ]
];

var all_cards = $$Array.of_list(List.concat(List.map((function (pip) {
                return List.map((function (suit) {
                              return /* tuple */[
                                      pip,
                                      suit
                                    ];
                            }), suits);
              }), pips)));

var full_deck = /* tuple */[
  all_cards,
  /* [] */0
];

function draw_card(deck) {
  while(true) {
    var cards_used = deck[1];
    var cards_available = deck[0];
    var card = Caml_array.caml_array_get(cards_available, Random.$$int(cards_available.length));
    if (List.mem(card, cards_used)) {
      continue ;
      
    } else {
      return /* tuple */[
              card,
              /* tuple */[
                cards_available,
                /* :: */[
                  card,
                  cards_used
                ]
              ]
            ];
    }
  };
}

exports.pips      = pips;
exports.suits     = suits;
exports.all_cards = all_cards;
exports.full_deck = full_deck;
exports.draw_card = draw_card;
/* all_cards Not a pure module */
