open Poker
open Command
open State

(** [legal_bet st n] makes sure [n] is a legal bet, given state [st]. If it
    is not a legal bet, it corrects it to be so. *)
let legal_bet st n : Command.move = 
  if n = 0 then Check else if n > st.cash1 then AllIn else Bet n

(** [legal_raise st n] makes sure [n] is a legal raise, given state [st]. If it
    is not a legal raise, it corrects it to be so. *)
let legal_raise st n : Command.move = 
  if n > st.cash1 then AllIn else if n < 2*(match st.turn with
      | 1 -> st.previous_bets_2
      | _ -> st.previous_bets_1) then Call else Raise n

(** [legal_call st] is [Call] if the AI can call, and [Fold] otherwise. *)
let legal_call st : Command.move = 
  if st.cash2 = 0 then Call else if (match st.turn with
      | 1 -> st.previous_bets_2
      | _ -> st.previous_bets_1) > st.cash1 then AllIn else Call


let easy_strat (st:State.t) : Command.move = 
  if st.previous_move = [] then Check else 
    match List.hd st.previous_move with
    |Bet _ -> Call
    |Raise _ -> Call
    |_ -> Check

(**  [floor_five n] is 5 if [n] is less than 5, and n otherwise*)
let floor_five n = 
  if n < 5 then 5 else n

(** [ceil f] is the ceiling function*)
let ceil (f:float) = 
  if mod_float f 1.= 0. then int_of_float f else (int_of_float (f +. 1.))

(** [chen_strength in_hand] is the Chen strength of the preflop hand of a player,
    according to the algorithm designed by Bill Chen.*)
let chen_strength (in_hand:card list) : int= 
  let highest = List.hd (values_in_hand (highest_n 1 in_hand [])) in
  let highest_val = if highest = 12 then float_of_int 10 else if highest = 11 then float_of_int 8
    else if highest = 10 then float_of_int 7 else if highest = 9 then float_of_int 6 else
      (float_of_int highest)/.2. in 
  let suited = if List.hd (suits_in_hand in_hand) = List.hd (List.tl (suits_in_hand in_hand))
    then true else false in
  let pair = if (pairs in_hand []) <> [] then true else false in
  let snd = List.hd (List.tl (values_in_hand in_hand)) in 
  let gap = (highest - snd)-1 in
  let gap_pts = if gap = 0 then (-1) else if gap = 1 || gap = -1 then 0 else if gap = 2 then 
      2 else if gap = 3 then 4 else 5 in 
  if pair then (floor_five (int_of_float (2. *. highest_val))) - (gap_pts) else
  if suited then (ceil highest_val) + 2 - (gap_pts) else
    (ceil highest_val) - (gap_pts)

(** [remove_from_deck card deck] removes [card] from the [deck]. *)
let rec remove_from_deck cards deck = 
  match deck with
  |h::t -> if List.mem h cards then remove_from_deck cards t 
    else h::(remove_from_deck cards t)
  |[] -> []

(** [simulate_hand table hand] is the winner of a simulated hand given a current 
    [table] and a player's [hand]. Randomly selects remaining cards on the table, as well
    as the opponent's hand. *)
let rec simulate_hand (table:Poker.table) (hand:Poker.hand) : int = 
  let used = Array.to_list (Array.concat [table;hand]) in
  let remaining = remove_from_deck used (Array.to_list Poker.full_deck) in
  let shuffled = Poker.shuffle (Array.of_list remaining) (Array.of_list []) in
  let p2_hand = snd (Poker.deal shuffled (Array.of_list [])) in
  let used = Array.to_list (Array.concat [p2_hand;(Array.of_list used)]) in
  let remaining = remove_from_deck used (Array.to_list Poker.full_deck) in
  if Array.length table = 5 then if Poker.winner hand p2_hand table = "player 1" then 1 else 0
  else if Array.length table = 4 then let table = Poker.river (Array.of_list remaining) table in
    if Poker.winner hand p2_hand table = "player 1" then 1 else 0 
  else if Array.length table = 3 then let table1 = Poker.river (Array.of_list remaining) table in
    simulate_hand table1 hand else failwith "invalid simulation"

(** [pct_chance wins tries remaining table hand] is the percent chance
    of a given [hand] winning, given a current [table], as approximated by 
    iterating through [remaining] simulations and tracking the percentage of 
    [wins]. *)
let rec pct_chance wins tries remaining table hand = 
  if remaining = 0 then (float_of_int wins)/.(float_of_int tries) else
  if simulate_hand table hand = 1 then pct_chance (wins+1) (tries+1)
      (remaining-1) table hand else pct_chance (wins) (tries+1)
      (remaining-1) table hand

(** [bet_range min max] takes in two ints, [min] and [max],
    and returns an int between them. *)
let bet_range min max = 
  Random.self_init();
  min + (Random.int max-min)

(** [preflop_bet st last pot in_hand] is the appropriate preflop action for the
    CPU to take given the most recent move [last], the current state [st],
    the current size of the [pot] and what cards the player has [in_hand]. *)
let preflop_bet st last pot in_hand : Command.move=
  Random.self_init();
  if st.cash1 = 0 then Check else
    let strength = chen_strength in_hand in
    match last with
    |Some Check
    |None -> let r = Random.float 1. in 
      if strength >= 9 then if r > 0.8 then Check else 
          legal_bet st (bet_range (pot) (pot*4)) 
      else if strength >= 6 then if r > 0.7 then legal_bet st (bet_range (pot) (pot*4)) 
        else Check else if strength >= 4
      then if r > 0.65 then legal_bet st (bet_range (pot/2) (pot*3)) else Check
      else if r < 0.8 then Check else legal_bet st (bet_range (pot/2) (pot*2))
    |Some AllIn -> if strength >= 1 then AllIn else Fold
    | _ -> let r = Random.float 1. in 
      let bet_prop = (float_of_int (match st.turn with
          | 1 -> st.previous_bets_2
          | _ -> st.previous_bets_1)) /. (float_of_int st.pot) in
      let heuristic = 3 * strength - (int_of_float (bet_prop *. 10.)) in
      if heuristic >= 24 then if r > 0.8 then legal_call st else
          legal_raise st (bet_range (pot) (pot*2)) else if heuristic >= 19
      then if r > 0.75 then legal_raise st (bet_range (pot) (pot*2))
        else legal_call st else if heuristic >= 14
      then if r > 0.9 then legal_raise st (bet_range (pot) (pot*2))
        else if r > 0.85 then Fold else legal_call st else if heuristic < 9 
      then Fold else if r < 0.75 then
        Fold else legal_call st

(** [reactionary_bet chance st] is the action taken by the computer in response
    to a bet or raise, given a current state [st] and [chance] of winning. *)
let reactionary_bet chance st = 
  Random.self_init(); let r = Random.float 1. in
  let bet_prop = (float_of_int (match st.turn with
      | 1 -> st.previous_bets_2
      | _ -> st.previous_bets_1)) /. (float_of_int st.pot) in
  let heuristic = 2.5 *. chance -. bet_prop in
  if heuristic > 2.05 then legal_raise st (bet_range (st.pot/2) st.pot*4)
  else if heuristic > 1.80 then if r > 0.80 then legal_call st else 
      legal_raise st (bet_range (st.pot/3) st.pot*3) else 
  if heuristic > 1.65 then if r > 0.4 then legal_call st else 
      legal_raise st (bet_range (st.pot/4) st.pot*2) else 
  if heuristic > 1.5 then if r > 0.25 then legal_call st else 
      legal_raise st (bet_range (st.pot/4) st.pot) else 
  if heuristic > 1.35 then if r > 0.4 then legal_call st else 
    if r > 0.25 then legal_raise st (bet_range (st.pot/8) st.pot) else Fold else
  if heuristic < 1.15 then Fold else
  if r > 0.85 then legal_raise st (bet_range (st.pot/3) st.pot*3) else Fold

let hard_strat (st:State.t) : Command.move = 
  if st.cash1 = 0 then Check else
    let last = if st.previous_move = [] then None else
      if st.cash2 <= 0 then Some AllIn else Some (List.hd st.previous_move) in
    let num_cards = Array.length st.table + Array.length st.hand1 in
    if num_cards = 2 then preflop_bet st last st.pot (Array.to_list st.hand1) else
      let chance = (pct_chance 0 0 1200 st.table st.hand1) in
      match last with
      |Some Check
      |None -> 
        Random.self_init(); let r = Random.float 1. in
        if chance > 0.85 then if r > 0.85 then legal_bet st (bet_range (st.pot) st.pot*5) 
          else legal_bet st (bet_range (st.pot/3) st.pot*4) else 
        if chance > 0.75 then legal_bet st (bet_range (st.pot/3) st.pot*3) else 
        if chance > 0.65 then legal_bet st (bet_range (st.pot/4) st.pot*2) else 
        if chance > 0.50 then legal_bet st (bet_range (st.pot/8) st.pot) else 
        if chance < 0.25 then Check else
        if r > 0.8 then legal_bet st (bet_range (st.pot/3) st.pot*4) else Check
      |Some AllIn ->
        if st.previous_move = [] || List.hd st.previous_move = Check then Check 
        else if chance >= 0.75 then AllIn else Fold
      |_ -> reactionary_bet chance st
