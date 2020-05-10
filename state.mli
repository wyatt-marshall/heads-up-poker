(** The State module handles state transitions of the game as it moves
    through differnet hands of the poker game *)

(** Opens modules which are used in state. *)
open Poker
open Command

(** [hand1] is a card array of player 1's hand. *)
type hand1 = card array

(** [hand2] is a card array of player 2's hand. *)
type hand2 = card array

(** [table] is a card array of the cards displayed on the table. *)
type table = card array

(** [cards] is a cary array that represents the randomized 9 possible cards
    that will be drawn to be dealt to players and the table. *)
type cards = card array

(** [cash1] is an int representing casah amount of player 1. *)
type cash1 = int

(** [cash2] is an int representing casah amount of player 2. *)
type cash2 = int

(** [pot] is an int representing the total pot amount of the hand. *)
type pot = int

(** [ante] is an int representing the ante paid by each player at the 
    beginning of each hand. *)
type ante = int

(** [previous_bets_1] is an int representing the most recent bet of player 1
    is 0 if there was no previous bet or raise. *)
type previous_bets_1 = int

(** [previous_bets_2] is an int representing the most recent bet of player 2
    is 0 if there was no previous bet or raise. *)
type previous_bets_2 = int

(** [turn] indicates which player's turn it is. 1 represents Player 1's turn
    and -1 represents Player 2's turn. *)
type turn = int

(** [started] is an int indicating which player started that hand, the int
    value alternatse between 1 and -1 depending on who began the hand. *)
type started = int

(** [stage] is an int which indicates what stage of the game state is currently 
    at, Limited to [-1,0,1,2,3,4]

    Ante-check stage = -1
    Pre-deal stage = 0
    Post-deal stage = 1
    Post-flop stage = 2
    Post-turn stage = 3
    Post-river stage = 4
*)
type stage = int

(** [previous_move] represents the last made move object of that state. *)
type previous_move = move list

(** the abstract value type representing a state object. *)
type t = { hand1 : hand1 ; hand2 : hand2 ; 
           table : table ; cards : cards ; cash1 : cash1 ; 
           cash2 : cash2 ; pot : pot ; ante : ante ; 
           previous_bets_1 : previous_bets_1 ; previous_bets_2 : previous_bets_2 
         ; turn : turn ; started : started ; stage : stage ; 
           previous_move : previous_move} 

(** [set_stage] allows user to manually set the game state into a certain
    [stage]. *)
val set_stage : t -> stage -> t

(** [deal] takes in a state that has not dealt out any cards, and deals out 
    2 cards each and assigns them to [hand1] and [hand2] without dealing
    any cards onto [table]. *)
val deal : t -> t

(** [init_state] creates an object state t with given inputs of [cash1] 
    [cash2] [ante] and [turn]. *)
val init_state : cash1 -> cash2 -> ante -> turn -> t

(**[ flop] takes in an object that has already delt out its cards to hand1 
   and hand2, and inserts 3 card objects into card array [table]. *)
val flop : t -> t 

(** [turn] takes in an object state that has dealt hands and the flop dealt, 
    and adds another card object into the card array [table] which represents
    the flop of a poker hand. *)
val turn : t -> t 

(**[river] takes in an object state that has dealt hands and flop, turn dealt,
   and adds another card object into card array [table] which represents
   the river of a poker hand *)
val river : t -> t 

(**[winner] takes in an object with dealt cards and all 5 cards on the table,
   and determines who the winner is between two hands and outputs a winner
   state with the winner's money deposited into [cash1] or [cash2] *)
val river : t -> t 

(**[buyin] allows a player to buy in for int amount, which is 
   then deposited into respective [cash1] or [cash2] *)
val buyin : t -> int -> t

(**[fold] takes in a state and folds the current player turn's hand, 
   initializing a new hand based on the winner *)
val fold : t -> t 

(**[bet] takes in a state and bets a certain value [amt] int which
   is subtracted from that player's cash and deposited into pot *)
val bet : t -> ante -> t

(**[raise] takes in a state where a player just made a bet, and allows
   a player to raise the amount which is atleast double the previous bet*)
val raise : t -> previous_bets_1 -> t

(**[call] takes in a state where player just made a bet, and allows
   the player to call amount just bet *)
val call : t -> t 

(**[check] takes in a state and checks, essentially moving turn to the 
   next person after the current player *)
val check : t -> t 

(**[allin] allows the player to put forth all their money in a bet. If 
   the player has less money than what was previously bet, the difference
   in the previous bet and allin amount is calculated and returned to opposite
   player *)
val allin : t -> t 

(**[new_cards] takes in a state and [cmd] and checks if it is time to
   deal out new cards (flop, turn, river) if it is, then those respective
   card functions are applied to the st and returned, if not then original st
   is returned *)
val new_cards : t -> Command.move -> t
