type move = 
  (* Calling is matching the amount that has been put in by another player in 
     the form of a bet or a raise. *)
  |Call
  (* Checking is what one does if they wish to pass the action to the next
     player, but keep their cards. *)
  |Check
  (* Folding is not betting and discarding cards, out till next hand. *)
  |Fold 
  (* Betting is the opening bet of a hand. *)
  |Bet of int
  (* Raising is betting more than the previous bet on the table. *)
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

exception Invalid_move 

(** [str_to_strlst_helper str] returns the list of strings with all spaces and 
    empty strings removed. The list is in the same order as the original string.
    Requires: strlst is a list of strings *)
let str_to_strlst_helper strlst = 
  let rec aux acc = function
    |[] -> List.rev acc
    |h::t -> if h="" || h=" " then aux acc t  else aux (h::acc) t
  in aux [] strlst

(** [str_to_strlst str] splits a string at the spaces then returns a lsit of 
    strings with all spaces and empty strings removed.
    Requires: str is a string.   *)
let str_to_strlst str = 
  let str_lst = String.split_on_char ' ' str in
  str_to_strlst_helper str_lst

(** [is_phrase_wellformed move_phrase] returns true if [move_phrase] 
    satisfies the ctriteria given below, raises an error otherwise. 
    Criteria: Each element of the list represents a word of the move 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.

    A [move_phrase] is not permitted to be the empty list. *)
let rec is_phrase_wellformed strlst = 
  match strlst with 
  |[] -> true
  |h::t -> if h = " " || h = "" then raise(Invalid_move) else true 

(** [remove_dsign str] takes a string of the form of a dollar sign ($) followed
    by an integer and returns the integer as type int. 
    Example: [remove_dsign "$25"] returns [25]. *)
let remove_dsign str = 
  try int_of_string str with (Failure "int_of_string") -> 
    let list = String.split_on_char '$' str in 
    match list with 
    |""::[t] -> int_of_string t 
    |_ -> -1

let parse str = 
  let move = str_to_strlst str in
  match move with 
  |[] -> Loop
  |h::t -> 
    match h, t with 
    |"fold", [] -> Fold
    |"call", [] -> Call
    |"check", [] -> Check 
    |"raise", [] -> Raise (-1)
    |"raise", [bet] -> Raise (remove_dsign bet)
    |"bet", [bet] -> Bet (remove_dsign bet)
    |"quit", [] -> Quit
    |"buy", "in"::[] -> Buy_in (-1)
    |"buy", "in"::[amount] -> Buy_in (remove_dsign amount)
    |"help", [] -> Help
    |"pot", [] -> Pot
    |"clear", [] -> Clear 
    |"deal", [] -> Deal
    |"all", "in"::t -> AllIn
    |"p1", [x] -> if x = "cards" then Cards1 
      else if x = "cash" then Cash1
      else raise(Invalid_move)
    |"p2", [y] -> if y = "cards" then Cards2 
      else if y = "cash" then Cash2 
      else raise(Invalid_move)
    |_, _ -> raise(Invalid_move)


