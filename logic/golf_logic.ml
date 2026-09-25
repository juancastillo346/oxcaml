(* ====================================================== *)
(* SIX-CARD GOLF                                          *)
(* ====================================================== *)


(* ------------------------- *)
(* TYPES                     *)
(* ------------------------- *)

type player =
  | P1
  | P2
;;


type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades
;;


type rank =
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
  | King
;;


type card =
  { rank : rank
  ; suit : suit
  }
;;


(* A card in a player's 2x3 grid can be face-up or face-down. *)
type card_slot =
  { card : card
  ; face_up : bool
  }
;;


(* Cards are stored in this order:

   0 1 2
   3 4 5
*)
type hand = card_slot list
;;


type decision =
  | In_progress of { whose_turn : player }
  | Final_turn of player
  | Winner of
      { player : player
      ; p1_score : int
      ; p2_score : int
      }
  | Tie of
      { p1_score : int
      ; p2_score : int
      }
;;


type game_state =
  { p1_hand : hand
  ; p2_hand : hand
  ; draw_pile : card list
  ; discard_pile : card list
  ; decision : decision
  }
;;


(* These represent the three possible turn choices
   in the rules we chose. *)
type move =
  | Draw_and_swap of
      { drawn : card
      ; replace_index : int
      }
  | Draw_discard_and_flip of
      { drawn : card
      ; flip_index : int
      }
  | Take_discard_and_swap of
      { taken : card
      ; replace_index : int
      }
;;



(* ====================================================== *)
(* INITIAL STATE                                          *)
(* ====================================================== *)

let initial_state : game_state =
  { p1_hand =
      [ { card = { rank = Seven; suit = Spades }; face_up = true }
      ; { card = { rank = Queen; suit = Hearts }; face_up = true }
      ; { card = { rank = Six; suit = Clubs }; face_up = false }
      ; { card = { rank = Ten; suit = Diamonds }; face_up = false }
      ; { card = { rank = Two; suit = Clubs }; face_up = false }
      ; { card = { rank = King; suit = Spades }; face_up = false }
      ]

  ; p2_hand =
      [ { card = { rank = Three; suit = Clubs }; face_up = true }
      ; { card = { rank = Nine; suit = Hearts }; face_up = true }
      ; { card = { rank = Four; suit = Diamonds }; face_up = false }
      ; { card = { rank = Ace; suit = Spades }; face_up = false }
      ; { card = { rank = Five; suit = Hearts }; face_up = false }
      ; { card = { rank = Eight; suit = Clubs }; face_up = false }
      ]

  ; draw_pile =
      [ { rank = Four; suit = Spades }
      ; { rank = Six; suit = Diamonds }
      ; { rank = Eight; suit = Diamonds }
      ; { rank = Jack; suit = Clubs }
      ]

  ; discard_pile =
      [ { rank = Five; suit = Diamonds } ]

  ; decision =
      In_progress { whose_turn = P1 }
  }
;;



(* ====================================================== *)
(* INTERESTING STATE                                      *)
(* ====================================================== *)

let interesting_state : game_state =
  { p1_hand =
      [ { card = { rank = Six; suit = Spades }; face_up = true }
      ; { card = { rank = Eight; suit = Diamonds }; face_up = true }
      ; { card = { rank = Four; suit = Spades }; face_up = true }
      ; { card = { rank = Two; suit = Hearts }; face_up = true }
      ; { card = { rank = Seven; suit = Clubs }; face_up = false }
      ; { card = { rank = Jack; suit = Clubs }; face_up = true }
      ]

  ; p2_hand =
      [ { card = { rank = Ace; suit = Diamonds }; face_up = true }
      ; { card = { rank = Ten; suit = Clubs }; face_up = true }
      ; { card = { rank = Three; suit = Hearts }; face_up = true }
      ; { card = { rank = Queen; suit = Spades }; face_up = false }
      ; { card = { rank = Five; suit = Spades }; face_up = true }
      ; { card = { rank = Nine; suit = Clubs }; face_up = false }
      ]

  ; draw_pile =
      [ { rank = Seven; suit = Diamonds }
      ; { rank = Two; suit = Spades }
      ; { rank = King; suit = Clubs }
      ]

  ; discard_pile =
      [ { rank = Queen; suit = Hearts }
      ; { rank = King; suit = Hearts }
      ]

  ; decision =
      In_progress { whose_turn = P2 }
  }
;;



(* ====================================================== *)
(* BEFORE FINAL TURN                                      *)
(* ====================================================== *)

(* P1 already has all six cards face-up.
   P2 gets one final turn. *)

let before_terminal_state : game_state =
  { p1_hand =
      [ { card = { rank = Four; suit = Spades }; face_up = true }
      ; { card = { rank = Six; suit = Diamonds }; face_up = true }
      ; { card = { rank = Eight; suit = Clubs }; face_up = true }
      ; { card = { rank = Two; suit = Hearts }; face_up = true }
      ; { card = { rank = Jack; suit = Diamonds }; face_up = true }
      ; { card = { rank = King; suit = Spades }; face_up = true }
      ]

  ; p2_hand =
      [ { card = { rank = Three; suit = Clubs }; face_up = true }
      ; { card = { rank = Five; suit = Spades }; face_up = true }
      ; { card = { rank = Four; suit = Diamonds }; face_up = true }
      ; { card = { rank = Three; suit = Diamonds }; face_up = true }
      ; { card = { rank = Ace; suit = Hearts }; face_up = true }
      ; { card = { rank = Queen; suit = Clubs }; face_up = false }
      ]

  ; draw_pile =
      [ { rank = Ten; suit = Spades }
      ; { rank = Seven; suit = Hearts }
      ]

  ; discard_pile =
      [ { rank = Two; suit = Clubs }
      ; { rank = Nine; suit = Diamonds }
      ]

  ; decision =
      Final_turn P2
  }
;;



(* ====================================================== *)
(* FINAL MOVE                                             *)
(* ====================================================== *)

(* P2 takes the 2 of Clubs from the discard pile
   and swaps it with the face-down Queen in slot 5. *)

let move_to_terminal_state : move =
  Take_discard_and_swap
    { taken = { rank = Two; suit = Clubs }
    ; replace_index = 5
    }
;;



(* ====================================================== *)
(* TERMINAL / END STATE                                   *)
(* ====================================================== *)

let terminal_state : game_state =
  { p1_hand =
      [ { card = { rank = Four; suit = Spades }; face_up = true }
      ; { card = { rank = Six; suit = Diamonds }; face_up = true }
      ; { card = { rank = Eight; suit = Clubs }; face_up = true }
      ; { card = { rank = Two; suit = Hearts }; face_up = true }
      ; { card = { rank = Jack; suit = Diamonds }; face_up = true }
      ; { card = { rank = King; suit = Spades }; face_up = true }
      ]

  ; p2_hand =
      [ { card = { rank = Three; suit = Clubs }; face_up = true }
      ; { card = { rank = Five; suit = Spades }; face_up = true }
      ; { card = { rank = Four; suit = Diamonds }; face_up = true }
      ; { card = { rank = Three; suit = Diamonds }; face_up = true }
      ; { card = { rank = Ace; suit = Hearts }; face_up = true }
      ; { card = { rank = Two; suit = Clubs }; face_up = true }
      ]

  ; draw_pile =
      [ { rank = Ten; suit = Spades }
      ; { rank = Seven; suit = Hearts }
      ]

  ; discard_pile =
      [ { rank = Queen; suit = Clubs }
      ; { rank = Nine; suit = Diamonds }
      ]

  ; decision =
      Winner
        { player = P2
        ; p1_score = 30
        ; p2_score = 8
        }
  }
;;



(* ====================================================== *)
(* HW2: MAKE_MOVE                                         *)
(* ====================================================== *)


(* ------------------------- *)
(* MOVE ERRORS               *)
(* ------------------------- *)

type move_error =
  | Game_is_over
  | Illegal_card_position
  | Card_already_face_up
  | Empty_draw_pile
  | Empty_discard_pile
  | Card_not_on_top
;;



(* ------------------------- *)
(* HELPER FUNCTIONS          *)
(* ------------------------- *)

let other_player player =
  match player with
  | P1 -> P2
  | P2 -> P1
;;


(* A six-card hand uses positions 0 through 5. *)
let valid_index index =
  0 <= index && index < 6
;;


(* Get an item from a list at a certain index. *)
let rec get_at list index =
  match list, index with
  | [], _ ->
      None

  | first :: _, 0 ->
      Some first

  | _ :: rest, n ->
      get_at rest (n - 1)
;;


(* Create a new list with one item replaced. *)
let rec replace_at list index new_value =
  match list, index with
  | [], _ ->
      []

  | _ :: rest, 0 ->
      new_value :: rest

  | first :: rest, n ->
      first :: replace_at rest (n - 1) new_value
;;


(* Get the hand belonging to a player. *)
let hand_of_player state player =
  match player with
  | P1 ->
      state.p1_hand

  | P2 ->
      state.p2_hand
;;


(* Create a new state with one player's hand changed. *)
let set_hand state player new_hand =
  match player with
  | P1 ->
      { state with
        p1_hand = new_hand
      }

  | P2 ->
      { state with
        p2_hand = new_hand
      }
;;


(* Check whether every card is face-up. *)
let all_face_up hand =
  List.for_all
    (fun slot -> slot.face_up)
    hand
;;


(* Turn every card face-up. *)
let reveal_all hand =
  List.map
    (fun slot ->
      { slot with
        face_up = true
      })
    hand
;;



(* ------------------------- *)
(* SCORING                   *)
(* ------------------------- *)

let card_value card =
  match card.rank with
  | Ace -> 1
  | Two -> -2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten -> 10
  | Jack -> 10
  | Queen -> 10
  | King -> 0
;;


(* If two cards in the same vertical column
   have the same rank, the column scores 0. *)
let score_column top bottom =
  if top.card.rank = bottom.card.rank then
    0
  else
    card_value top.card + card_value bottom.card
;;


(* Hand layout:

      0  1  2
      3  4  5

   Vertical pairs:

      0 and 3
      1 and 4
      2 and 5
*)
let score_hand hand =
  match hand with
  | [ a; b; c; d; e; f ] ->
      score_column a d
      + score_column b e
      + score_column c f

  | _ ->
      failwith "Golf hand must contain exactly 6 cards"
;;



(* ------------------------- *)
(* FINISH A TURN             *)
(* ------------------------- *)

let finish_turn old_decision current_player state =
  match old_decision with

  (* If this was already the opponent's final turn,
     the round is now finished. *)
  | Final_turn _ ->

      let p1_hand =
        reveal_all state.p1_hand
      in

      let p2_hand =
        reveal_all state.p2_hand
      in

      let p1_score =
        score_hand p1_hand
      in

      let p2_score =
        score_hand p2_hand
      in

      let decision =
        if p1_score < p2_score then
          Winner
            { player = P1
            ; p1_score
            ; p2_score
            }

        else if p2_score < p1_score then
          Winner
            { player = P2
            ; p1_score
            ; p2_score
            }

        else
          Tie
            { p1_score
            ; p2_score
            }
      in

      { state with
        p1_hand
      ; p2_hand
      ; decision
      }


  (* This was a normal turn. *)
  | In_progress _ ->

      let current_hand =
        hand_of_player
          state
          current_player
      in

      if all_face_up current_hand then

        (* This player now has all six cards face-up.
           The opponent gets one final turn. *)
        { state with
          decision =
            Final_turn
              (other_player current_player)
        }

      else

        (* Game continues normally.
           Switch to the other player. *)
        { state with
          decision =
            In_progress
              { whose_turn =
                  other_player current_player
              }
        }


  | Winner _
  | Tie _ ->
      state
;;



(* ====================================================== *)
(* MAKE_MOVE                                              *)
(* ====================================================== *)

let make_move
    (state : game_state)
    (move : move)
  : (game_state, move_error) Result.t
  =
  match state.decision with

  (* No more moves after the game has ended. *)
  | Winner _
  | Tie _ ->
      Error Game_is_over


  (* These both mean that somebody currently has a turn. *)
  | In_progress { whose_turn }
  | Final_turn whose_turn ->

      let hand =
        hand_of_player
          state
          whose_turn
      in

      match move with


      (* ============================================== *)
      (* 1. DRAW FROM DECK AND KEEP IT                 *)
      (* ============================================== *)

      | Draw_and_swap
          { drawn
          ; replace_index
          } ->

          if not (valid_index replace_index) then
            Error Illegal_card_position

          else
            (match state.draw_pile with

             | [] ->
                 Error Empty_draw_pile

             | top_card :: rest_of_deck ->

                 if drawn <> top_card then
                   Error Card_not_on_top

                 else
                   (match get_at hand replace_index with

                    | None ->
                        Error Illegal_card_position

                    | Some old_slot ->

                        let new_slot =
                          { card = drawn
                          ; face_up = true
                          }
                        in

                        let new_hand =
                          replace_at
                            hand
                            replace_index
                            new_slot
                        in

                        let new_state =
                          set_hand
                            state
                            whose_turn
                            new_hand
                        in

                        let new_state =
                          { new_state with
                            draw_pile =
                              rest_of_deck

                          ; discard_pile =
                              old_slot.card
                              :: state.discard_pile
                          }
                        in

                        Ok
                          (finish_turn
                             state.decision
                             whose_turn
                             new_state)))


      (* ============================================== *)
      (* 2. DRAW, DISCARD, THEN FLIP                   *)
      (* ============================================== *)

      | Draw_discard_and_flip
          { drawn
          ; flip_index
          } ->

          if not (valid_index flip_index) then
            Error Illegal_card_position

          else
            (match state.draw_pile with

             | [] ->
                 Error Empty_draw_pile

             | top_card :: rest_of_deck ->

                 if drawn <> top_card then
                   Error Card_not_on_top

                 else
                   (match get_at hand flip_index with

                    | None ->
                        Error Illegal_card_position

                    | Some slot when slot.face_up ->
                        Error Card_already_face_up

                    | Some slot ->

                        let flipped_slot =
                          { slot with
                            face_up = true
                          }
                        in

                        let new_hand =
                          replace_at
                            hand
                            flip_index
                            flipped_slot
                        in

                        let new_state =
                          set_hand
                            state
                            whose_turn
                            new_hand
                        in

                        let new_state =
                          { new_state with
                            draw_pile =
                              rest_of_deck

                          ; discard_pile =
                              drawn
                              :: state.discard_pile
                          }
                        in

                        Ok
                          (finish_turn
                             state.decision
                             whose_turn
                             new_state)))


      (* ============================================== *)
      (* 3. TAKE TOP DISCARD AND SWAP                  *)
      (* ============================================== *)

      | Take_discard_and_swap
          { taken
          ; replace_index
          } ->

          if not (valid_index replace_index) then
            Error Illegal_card_position

          else
            (match state.discard_pile with

             | [] ->
                 Error Empty_discard_pile

             | top_card :: rest_of_discard ->

                 if taken <> top_card then
                   Error Card_not_on_top

                 else
                   (match get_at hand replace_index with

                    | None ->
                        Error Illegal_card_position

                    | Some old_slot ->

                        let new_slot =
                          { card = taken
                          ; face_up = true
                          }
                        in

                        let new_hand =
                          replace_at
                            hand
                            replace_index
                            new_slot
                        in

                        let new_state =
                          set_hand
                            state
                            whose_turn
                            new_hand
                        in

                        let new_state =
                          { new_state with
                            discard_pile =
                              old_slot.card
                              :: rest_of_discard
                          }
                        in

                        Ok
                          (finish_turn
                             state.decision
                             whose_turn
                             new_state)))
;;