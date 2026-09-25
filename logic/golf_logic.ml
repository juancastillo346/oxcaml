type player =
  | P1
  | P2

type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades

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

type card =
  { rank : rank
  ; suit : suit
  }

(* A card in a player's 2x3 grid can be face-up or face-down. *)
type card_slot =
  { card : card
  ; face_up : bool
  }

(* Cards are stored in this order:
   0 1 2
   3 4 5
*)
type hand = card_slot list

type decision =
  | In_progress of { whose_turn : player }
  | Final_turn of player
  | Winner of
      { player : player
      ; p1_score : int
      ; p2_score : int
      }

type game_state =
  { p1_hand : hand
  ; p2_hand : hand
  ; draw_pile : card list
  ; discard_pile : card list
  ; decision : decision
  }

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


(* ------------------------- *)
(* INITIAL STATE             *)
(* ------------------------- *)

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

  ; decision = In_progress { whose_turn = P1 }
  }
;;


(* ------------------------- *)
(* INTERESTING STATE         *)
(* ------------------------- *)

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

  ; decision = In_progress { whose_turn = P2 }
  }
;;


(* ------------------------- *)
(* BEFORE FINAL TURN         *)
(* ------------------------- *)

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

  ; decision = Final_turn P2
  }
;;


(* ------------------------- *)
(* FINAL MOVE                *)
(* ------------------------- *)

(* P2 takes the 2 of Clubs from the discard pile
   and swaps it with the face-down Queen in slot 5. *)

let move_to_terminal_state : move =
  Take_discard_and_swap
    { taken = { rank = Two; suit = Clubs }
    ; replace_index = 5
    }
;;


(* ------------------------- *)
(* TERMINAL / END STATE      *)
(* ------------------------- *)

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
        ; p1_score = 26
        ; p2_score = 8
        }
  }
;;