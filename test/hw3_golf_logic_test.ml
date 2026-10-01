open Tictactoe_logic_library.Golf_logic

let drawn = List.hd initial_state.draw_pile
let taken = List.hd initial_state.discard_pile

(* Unit tests return a bool, as in Lesson 3. *)
let%test "HW1 final move produces the exact terminal state" =
  make_move before_terminal_state move_to_terminal_state = Ok terminal_state
;;

let%test "HW1 terminal hands have the expected scores" =
  score_hand terminal_state.p1_hand = 26
  && score_hand terminal_state.p2_hand = 8
;;

let%test "matching ranks in a vertical column score zero" =
  let top = { card = { rank = Two; suit = Hearts }; face_up = true } in
  let bottom = { card = { rank = Two; suit = Clubs }; face_up = true } in
  score_column top bottom = 0
;;

let%test "revealing the last hidden card gives the opponent one final turn" =
  let state = { interesting_state with decision = In_progress { whose_turn = P1 } } in
  let move = Draw_discard_and_flip { drawn = List.hd state.draw_pile; flip_index = 4 } in
  match make_move state move with
  | Ok next -> next.decision = Final_turn P2
  | Error _ -> false
;;

(* Show the two rows of each hand, like the board printer in Lesson 3.
   Hidden cards are printed as ??. *)
let card_text card =
  let rank =
    match card.rank with
    | Ace -> "A" | Two -> "2" | Three -> "3" | Four -> "4"
    | Five -> "5" | Six -> "6" | Seven -> "7" | Eight -> "8"
    | Nine -> "9" | Ten -> "10" | Jack -> "J" | Queen -> "Q" | King -> "K"
  in
  let suit =
    match card.suit with Hearts -> "H" | Diamonds -> "D" | Clubs -> "C" | Spades -> "S"
  in
  rank ^ suit
;;

let player_text = function P1 -> "P1" | P2 -> "P2"

let print_state state =
  let print_hand name hand =
    Printf.printf "%s:\n" name;
    List.iteri
      (fun i slot ->
        Printf.printf "%s%s" (if slot.face_up then card_text slot.card else "??")
          (if i mod 3 = 2 then "\n" else " "))
      hand
  in
  print_hand "P1" state.p1_hand;
  print_hand "P2" state.p2_hand;
  Printf.printf "Draw pile: %d cards\n" (List.length state.draw_pile);
  Printf.printf "Discard pile: %s\n" (String.concat " " (List.map card_text state.discard_pile));
  match state.decision with
  | In_progress { whose_turn } -> Printf.printf "Turn: %s\n" (player_text whose_turn)
  | Final_turn player -> Printf.printf "Final turn: %s\n" (player_text player)
  | Winner { player; p1_score; p2_score } ->
    Printf.printf "%s wins: P1 = %d, P2 = %d\n" (player_text player) p1_score p2_score
  | Tie { p1_score; p2_score } -> Printf.printf "Tie: P1 = %d, P2 = %d\n" p1_score p2_score
;;

let make_move_and_print state move =
  match make_move state move with
  | Ok next -> print_state next
  | Error error ->
    let name =
      match error with
      | Game_is_over -> "Game_is_over"
      | Illegal_card_position -> "Illegal_card_position"
      | Card_already_face_up -> "Card_already_face_up"
      | Empty_draw_pile -> "Empty_draw_pile"
      | Empty_discard_pile -> "Empty_discard_pile"
      | Card_not_on_top -> "Card_not_on_top"
    in
    Printf.printf "(Error %s)\n" name
;;

let%expect_test "HW1 initial hands and all three legal turn choices" =
  print_state initial_state;
  [%expect
    {|
    P1:
    7S QH ??
    ?? ?? ??
    P2:
    3C 9H ??
    ?? ?? ??
    Draw pile: 4 cards
    Discard pile: 5D
    Turn: P1
    |}];
  make_move_and_print initial_state (Draw_and_swap { drawn; replace_index = 2 });
  [%expect
    {|
    P1:
    7S QH 4S
    ?? ?? ??
    P2:
    3C 9H ??
    ?? ?? ??
    Draw pile: 3 cards
    Discard pile: 6C 5D
    Turn: P2
    |}];
  make_move_and_print initial_state (Draw_discard_and_flip { drawn; flip_index = 2 });
  [%expect
    {|
    P1:
    7S QH 6C
    ?? ?? ??
    P2:
    3C 9H ??
    ?? ?? ??
    Draw pile: 3 cards
    Discard pile: 4S 5D
    Turn: P2
    |}];
  make_move_and_print initial_state (Take_discard_and_swap { taken; replace_index = 2 });
  [%expect
    {|
    P1:
    7S QH 5D
    ?? ?? ??
    P2:
    3C 9H ??
    ?? ?? ??
    Draw pile: 4 cards
    Discard pile: 6C
    Turn: P2
    |}]
;;

let%expect_test "illegal moves print each error kind" =
  make_move_and_print initial_state (Draw_and_swap { drawn; replace_index = 6 });
  [%expect {| (Error Illegal_card_position) |}];
  make_move_and_print initial_state (Draw_discard_and_flip { drawn; flip_index = 0 });
  [%expect {| (Error Card_already_face_up) |}];
  make_move_and_print { initial_state with draw_pile = [] }
    (Draw_and_swap { drawn; replace_index = 2 });
  [%expect {| (Error Empty_draw_pile) |}];
  make_move_and_print { initial_state with discard_pile = [] }
    (Take_discard_and_swap { taken; replace_index = 2 });
  [%expect {| (Error Empty_discard_pile) |}];
  make_move_and_print initial_state
    (Draw_and_swap { drawn = List.nth initial_state.draw_pile 1; replace_index = 2 });
  [%expect {| (Error Card_not_on_top) |}];
  make_move_and_print terminal_state move_to_terminal_state;
  [%expect {| (Error Game_is_over) |}]
;;

let%expect_test "HW1 final turn reveals the winning hands" =
  print_state before_terminal_state;
  [%expect
    {|
    P1:
    4S 6D 8C
    2H JD KS
    P2:
    3C 5S 4D
    3D AH ??
    Draw pile: 2 cards
    Discard pile: 2C 9D
    Final turn: P2
    |}];
  make_move_and_print before_terminal_state move_to_terminal_state;
  [%expect
    {|
    P1:
    4S 6D 8C
    2H JD KS
    P2:
    3C 5S 4D
    3D AH 2C
    Draw pile: 2 cards
    Discard pile: QC 9D
    P2 wins: P1 = 26, P2 = 8
    |}]
;;

let candidate_moves state =
  List.concat
    (List.init 6 (fun index ->
       let draws =
         match state.draw_pile with
         | [] -> []
         | card :: _ ->
           [ Draw_and_swap { drawn = card; replace_index = index }
           ; Draw_discard_and_flip { drawn = card; flip_index = index }
           ]
       in
       match state.discard_pile with
       | [] -> draws
       | card :: _ -> Take_discard_and_swap { taken = card; replace_index = index } :: draws))
;;

(* A local seed makes this reproducible without changing other tests' randomness.
   Bound the walk so a regression cannot leave the test running forever. *)
let random_walk_and_print start seed =
  let random = Random.State.make [| seed |] in
  let rec walk remaining state =
    match state.decision with
    | Winner _ | Tie _ -> print_state state
    | In_progress _ | Final_turn _ ->
      if remaining = 0 then failwith "Random walk did not finish";
      let next_states =
        List.filter_map
          (fun move -> match make_move state move with Ok next -> Some next | Error _ -> None)
          (candidate_moves state)
      in
      if next_states = [] then failwith "No legal moves before game over";
      let next = List.nth next_states (Random.State.int random (List.length next_states)) in
      walk (remaining - 1) next
  in
  walk 1000 start
;;

let%expect_test "seeded random walks show the final hands, piles, and scores" =
  random_walk_and_print initial_state 1;
  [%expect
    {|
    P1:
    7S QH 5H
    3C 8C 8D
    P2:
    10D 9H 4D
    AS JC 6D
    Draw pile: 0 cards
    Discard pile: 6C KS 2C 4S 5D
    P2 wins: P1 = 41, P2 = 40
    |}];
  random_walk_and_print initial_state 3;
  [%expect
    {|
    P1:
    3C 8D 10D
    AS 2C KS
    P2:
    JC 6C 4S
    8C 5H QH
    Draw pile: 0 cards
    Discard pile: 7S 4D 6D 9H 5D
    P1 wins: P1 = 20, P2 = 43
    |}];
  random_walk_and_print interesting_state 1234;
  [%expect
    {|
    P1:
    6S 8D 2S
    2H 7C JC
    P2:
    KC 10C 3H
    7D 5S 9C
    Draw pile: 0 cards
    Discard pile: 4S AD QS QH KH
    P1 wins: P1 = 27, P2 = 34
    |}]
;;
