open Blackjack;

type reward_type =
  | Nonterminal
  | Terminal;

type reward = (float, reward_type);

let q = Hashtbl.create(420); /* 10 dealer cards * 21 player possibilities * 2 actions */

let alpha = 0.01;

let gamma = 1.0;

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
  if (Random.float(100.0) < eps) {
    actions |> Array.length |> Random.int |> Array.get(actions)
  } else {
    let action_values = Array.map(get_q_value(round), actions);
    let max_action_index = argmax(action_values);
    actions[max_action_index]
  };

let get_reward = (round) =>
  switch round.state {
  | PlayerLost => ((-10.0), Terminal)
  | PlayerWon => (10.0, Terminal)
  | Draw => (0.0, Terminal)
  | PlayerTurn => (0.0, Nonterminal)
  };

let update_q = (old_state, action, next_state, policy) => {
  let q_value = get_q_value(old_state, action);
  let (reward_value, _reward_type) = get_reward(next_state);
  let next_action = policy(next_state);
  let next_q_value = get_q_value(next_state, next_action);
  let new_q_value = q_value +. alpha *. (reward_value +. gamma *. next_q_value -. q_value);
  Hashtbl.replace(q, round_to_q_key(old_state, action), new_q_value)
};

let rec run_episode = (round, epsilon) => {
  let select_action = select_action_epsilon(epsilon);
  let action = select_action(round);
  let result_round = play(round, action);
  update_q(round, action, result_round, select_action);
  switch (get_reward(result_round)) {
  | (_, Terminal) => ()
  | (_, Nonterminal) => run_episode(result_round, epsilon)
  }
};

let train = (iterations) => {
  let starting_epsilon = 50.0;
  let epsilon = ref(starting_epsilon);
  let epsilon_decay = 1.0 -. 1.0 /. float_of_int(iterations / 10);
  for (_iteration in 1 to iterations + 1) {
    epsilon := epsilon^ *. epsilon_decay;
    run_episode(new_round(), epsilon^)
  }
};

let action_to_string = (action) =>
  switch action {
  | Hit => "H"
  | Stand => "S"
  };

let get_action_value = (dealer_value, player_value, action) => {
  let key = (dealer_value, player_value, action);
  try (Hashtbl.find(q, key)) {
  | Not_found => 0.0
  }
};

let best_action = (dealer_value, player_value) => {
  let action_values = Array.map(get_action_value(dealer_value, player_value), actions);
  let max_action_index = argmax(action_values);
  (actions[max_action_index], action_values[max_action_index])
};

let print_q = () => {
  Printf.printf("   ");
  for (dealer_value in 2 to 11) {
    Printf.printf("%3i ", dealer_value)
  };
  Printf.printf("\n");
  for (player_value in 2 to 21) {
    Printf.printf("%3i ", player_value);
    for (dealer_value in 2 to 11) {
      let (action, _best_action_val) = best_action(dealer_value, player_value);
      Printf.printf("%3s ", action_to_string(action))
    };
    Printf.printf("\n")
  };
  Printf.printf("\n\n");
  Printf.printf("  ");
  for (dealer_value in 2 to 11) {
    Printf.printf("%7i ", dealer_value)
  };
  Printf.printf("\n");
  for (player_value in 2 to 21) {
    Printf.printf("%2i ", player_value);
    for (dealer_value in 2 to 11) {
      let (action, best_action_val) = best_action(dealer_value, player_value);
      Printf.printf("%5.1f(%s)", best_action_val, action_to_string(action))
    };
    Printf.printf("\n")
  };
  Printf.printf("\n\n");
  Printf.printf("  ");
  for (dealer_value in 2 to 11) {
    Printf.printf("%4i ", dealer_value)
  };
  Printf.printf("\n");
  for (player_value in 2 to 21) {
    Printf.printf("%2i ", player_value);
    for (dealer_value in 2 to 11) {
      let hit_val = get_action_value(dealer_value, player_value, Hit);
      let stand_val = get_action_value(dealer_value, player_value, Stand);
      Printf.printf("%5.1f", abs_float(hit_val -. stand_val))
    };
    Printf.printf("\n")
  }
};

train(10_000_000);

print_q();