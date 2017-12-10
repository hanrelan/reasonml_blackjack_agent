open Blackjack;

type reward_type =
  | Nonterminal
  | Terminal;

type reward = (float, reward_type);

let q = Hashtbl.create(420); /* 10 dealer cards * 21 player possibilities * 2 actions */

let epsilon = 5; /* 5% */

let alpha = 0.1;

let gamma = 0.9;

let actions = Array.of_list([Hit, Stand]);

type q_state = (int, int, action);

let round_to_q_key = (round, action) => (
  hand_value(round.dealer_hand),
  hand_value(round.player_hand),
  action
);

let get_q_value = (round, action) : float => {
  let key: q_state = round_to_q_key(round, action);
  try (Hashtbl.find(q, key)) {
  | Not_found => 0.0
  }
};

let argmax = (arr) => {
  let rec argmax_helper = (current_index, max_index) =>
    if (current_index >= Array.length(arr)) {
      max_index
    } else if (arr[current_index] > arr[max_index]) {
      argmax_helper(current_index + 1, current_index)
    } else {
      argmax_helper(current_index + 1, max_index)
    };
  argmax_helper(0, 0)
};

let select_action_epsilon = (eps, round) =>
  if (Random.int(100) < eps) {
    actions |> Array.length |> Random.int |> Array.get(actions)
  } else {
    let action_values = Array.map(get_q_value(round), actions);
    let max_action_index = argmax(action_values);
    actions[max_action_index]
  };

let select_action = select_action_epsilon(epsilon);

let get_reward = (round) =>
  switch round.state {
  | PlayerLost => ((-10.0), Terminal)
  | PlayerWon => (10.0, Terminal)
  | Draw => (0.0, Terminal)
  | PlayerTurn => (0.0, Nonterminal)
  };

let update_q = (old_state, action, next_state) => {
  let q_value = get_q_value(old_state, action);
  let (reward_value, _reward_type) = get_reward(next_state);
  let next_action = select_action(next_state);
  let next_q_value = get_q_value(next_state, next_action);
  let new_q_value = q_value +. alpha *. (reward_value +. gamma *. next_q_value -. q_value);
  Hashtbl.replace(q, round_to_q_key(old_state, action), new_q_value)
};

let rec run_episode = (round) => {
  let action = select_action(round);
  let result_round = play(round, action);
  update_q(round, action, result_round);
  switch (get_reward(result_round)) {
  | (_, Terminal) => ()
  | (_, Nonterminal) => run_episode(result_round)
  }
};

let train = (iterations) =>
  for (_ in 0 to iterations) {
    run_episode(new_round())
  };

let action_to_string = (action) =>
  switch action {
  | Hit => "H"
  | Stand => "S"
  };

let best_action = (dealer_value, player_value) => {
  let get_action_value = (action) => {
    let key = (dealer_value, player_value, action);
    try (Hashtbl.find(q, key)) {
    | Not_found => 0.0
    }
  };
  let action_values = Array.map(get_action_value, actions);
  let max_action_index = argmax(action_values);
  (actions[max_action_index], action_values[max_action_index])
};

let print_q = () => {
  Printf.printf("   ");
  for (dealer_value in 1 to 11) {
    Printf.printf("%2i ", dealer_value)
  };
  Printf.printf("\n");
  for (player_value in 1 to 21) {
    Printf.printf("%2i ", player_value);
    for (dealer_value in 1 to 11) {
      let (action, _best_action_val) = best_action(dealer_value, player_value);
      Printf.printf("%2s ", action_to_string(action))
    };
    Printf.printf("\n")
  }
};

train(2000000);

print_q();