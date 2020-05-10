(** The command module is responsible for defining the different moves that
    the players can use. It also defines a parsing function to take in player 
    input of type string and return the move the player tries to use, including
    helper functions to format the inputs correctly. *)

(** [move] is the type of moves a player can do. *)
type move = 
  |Call
  |Check
  |Fold 
  |Bet of int
  |Raise of int
  |Buy_in of int
  |Cards1
  |Cards2
  |Cash1
  |Cash2
  |Pot
  |Help
  |Quit
  |AllIn
  |Clear
  |Deal
  |Loop

(** [] *)
exception Invalid_move

(** [remove_dsign str] takes in a string and returns an int of the string, or 
    returns [-1] if the string isn't of the form ["int"] or ["$int"] where "int"
    represents any integer. *)
val remove_dsign : string -> int

(** [parse str] parses a string input from the player and returns the move that
    the player commands. Raises [Invalid_move] if it is an invalid move. *)
val parse : string -> move