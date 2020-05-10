(**change cards to deck **)
(**comments 

   Ability to buy back in when not enough 

   Add a element of state that just says what round it is
   ex, after 

   Maybe do a multi pronged state function - step through those elements **)

(**Open Poker and Command**)
open Poker
open Command

(**list of player 1's 2 card hand **)
type hand1 = card array

(**list of player 2;s 2 card hand **)
type hand2 = card array

(**list of cards dealt on table **)
type table = card array

(**list of 9 cards pulled from poker.deal that will be dealt, is a mutable
   array type so you can pull it out directly and will change**)
type cards = card array

(**value of player 1's cash **)
type cash1 = int

(**value of player 2's cash **)
type cash2 = int

(**value of pot **)
type pot = int

(**pre-determined ante amount from poker customization **)
type ante = int

(**indicates previous amount bet or raised for that given hand, used to track
   actions of player 1 during that given set**)
type previous_bets_1 = int

(**indicates previous amount bet or raised for that given hand, used to track
   actions of player 2 during that given set**)
type previous_bets_2 = int

(**indicates which player's turn it is, [1] for player 1 and [-1] for player 2,
   can only ever be [1] or [-1]**)
type turn = int

(**indicates which player started the hand, [1] for player 1 and [-1] for 
   player 2, only ever changed when initialized new hand with init_state**)
type started = int

(**indicates which stage of the game state is currently at 

   Limited to [0,1,2,3,4]

   Pre-deal stage = 0
   Post-deal stage = 1
   Post-flop stage = 2
   Post-turn stage = 3
   Post-river stage = 4

 **)
type stage = int

(**indicates what the previous move was stored as type Command ALWAYS a
   move [list] of length 1 cause there is only ever 1 previous move **)

type previous_move = move list

type t = { hand1 : hand1 ; hand2 : hand2 ; 
           table : table ; cards : cards ; cash1 : cash1 ; 
           cash2 : cash2 ; pot : pot ; ante : ante ; 
           previous_bets_1 : previous_bets_1 ; previous_bets_2 : previous_bets_2 
         ; turn : turn ; started : started ; stage : stage ; 
           previous_move : previous_move} 


(**returns player 1 hand **)
let hand1 st = 
  st.hand1

(**returns plauer 2 hand **)
let hand2 st = 
  st.hand2

(**returns cards out on table **)
let table st =
  st.table

(**returns player 1 cash **)
let cash1 st =
  st.cash1

(**returns player 2 cash  **)
let cash2 st =
  st.cash2

(**returns value of pot of that given hand**)
let pot st = 
  st.pot

(**returns hand of player whose current turn it is, helper function for main **)
let current_hand st = 
  match st.turn with 
  | 1 -> st.hand1
  | -1 -> st.hand2
  | _ -> failwith "Not possible"

(**sets stage to specific value **)
let set_stage st amt = 
  { hand1 = st.hand1 ; hand2 = st.hand2 ; 
    table = st.table ; cards = st.cards ; cash1 = st.cash1 ; 
    cash2 = st.cash2 ; pot = st.pot
  ; ante = st.ante ; previous_bets_1 = st.previous_bets_1 ; 
    previous_bets_2 = st.previous_bets_2
  ; turn = st.turn
  ; started = st.started ; stage = amt;
    previous_move = st.previous_move
  } 

(**BEGINNING OF NON TURN BASED STATE FUNCTIONS ex deal, flop, turn river 
   winner, buyin **)

(**deals 2 cards to each player by taking off first 4 elements of 
   array st.cards, then auto bets the ante of each player.  **)
let deal st = 

  print_endline "\nDealing hands now. "; 
  print_endline ("Each player pays an ante of $" ^ string_of_int st.ante ^ "\n")
  ;

  if (Array.length st.cards <> 9) then 
    ((print_endline "Dealing not possible"); st)
  else 


    let hand1 = snd (Poker.deal (st.cards) (Array.of_list ([]))) in 
    let remaining1 = fst (Poker.deal (st.cards) (Array.of_list ([]))) in 
    let hand2 = snd (Poker.deal (remaining1) (Array.of_list ([]))) in 
    let remainingcards = fst (Poker.deal (remaining1) (Array.of_list ([]))) in 
    {hand1 = hand1 ;  hand2 = hand2 ; table = Array.of_list ([]) ; 
     cards = remainingcards ;  cash1 = st.cash1 - st.ante ;
     cash2 = st.cash2 - st.ante;  pot = (2 * st.ante) ; ante = st.ante ; 
     previous_bets_1 = 0 ;
     previous_bets_2 = 0 ; turn = st.turn ; started = st.started ;stage = 1 ;
     previous_move = []
    }


(**initializes type state, always called when starting new hand therefore the 
   only information passing from one round to another is the amount of
   cash each player has left and their ante**)
let init_state cash1 cash2 ante started = 

  let cards = Poker.shuffle (Poker.full_deck) (Array.of_list ([])) in 

  (**state is initialized but cards are assigned to random array list of 9 
     cards called from Poker.shuffle **)
  { hand1 = Array.of_list ([]) ; hand2 = Array.of_list ([]) ; 
    table = Array.of_list ([]) ; cards = cards ; cash1 = cash1 ; 
    cash2 = cash2 ; pot = 0 ; ante = ante ; previous_bets_1 = 0 ;
    previous_bets_2 = 0 ; turn = started
  ; started = started ; stage = 0 ; previous_move = []} 

let flop st = 
  (**have assert statement to confirm length of array st.cards is 5 if fails
     then this action of flop is not possible **)
  if (Array.length st.cards <> 5) then ((print_endline "Flop not possible"); st)
  else 

    let flop = (Poker.flop (st.cards) (Array.of_list ([]))) in 
    let remainingcards = (fst flop) in 
    let table = (snd flop) in 
    print_endline "The cards on the table are:"; 
    print_endline (Array.to_list (table) |> hand_to_input |> pp3);

    (**table gets reassigned, st.cards gets reassigned, everything else
       stays untouched because no turn is made **)
    { hand1 = st.hand1 ; hand2 = st.hand2 ; 
      table = table ; cards = remainingcards ; cash1 = st.cash1 ; 
      cash2 = st.cash2 ; pot = st.pot
    ; ante = st.ante ; previous_bets_1 = 0 ; previous_bets_2 = 0 ; 
      turn = st.started ; started = st.started ; stage = 2; previous_move = []} 


let turn st = 

  (**checks to make sure turn is a valid move **)
  if (Array.length st.cards <> 2) then ((print_endline "Turn not possible"); st)
  else 

    let turn = (Poker.turn (st.cards) (st.table)) in 
    let remainingcards = (fst turn) in 
    let table = (snd turn) in
    print_endline "The cards on the table are:"; 
    print_endline (Array.to_list (table) |> hand_to_input |> pp4);
    { hand1 = st.hand1 ; hand2 = st.hand2 ; 
      table = table ; cards = remainingcards ; cash1 = st.cash1 ; 
      cash2 = st.cash2 ; pot = st.pot
    ; ante = st.ante ; previous_bets_1 = 0 ; previous_bets_2 = 0 ; 
      turn = st.started ; started = st.started ; stage = 3; previous_move = []} 

let river st = 
  (**checks to make sure turn is a valid move **)
  if (Array.length st.cards <> 1) 
  then ((print_endline "River not possible"); st)
  else 

    let table = (Poker.river (st.cards) (st.table)) in 
    print_endline "The cards on the table are:"; 
    print_endline (Array.to_list (table) |> hand_to_input |> pp5);
    { hand1 = st.hand1 ; hand2 = st.hand2 ; 
      table = table ; cards = Array.of_list ([]) ; cash1 = st.cash1 ; 
      cash2 = st.cash2 ; pot = st.pot
    ; ante = st.ante ; previous_bets_1 = 0 ; previous_bets_2 = 0 ; 
      turn = st.started ; started = st.started ; stage = 4 ; previous_move = []} 


(**declares winner between two hands and all table hands and declares winner 
   by moving pot over and resetting hands, not a turn based function
   because it takes in an input state [st] and spits out an init_state w/
   winner of hand taking previous pot and resetting of hands

   st.started also changes as the previous starter switches over **)

let winner_tie st = 
  print_string "Both players tied. Player 1 had"; 
  print_string (format_lst (Array.to_list st.hand1)); 
  print_endline "Player 2 had" ; 
  print_string (format_lst (Array.to_list st.hand2)); 
  let payout = (st.pot/2) in
  init_state (st.cash1 + payout) 
    (st.cash2 + payout) (st.ante) (-(st.started))

let winner st = 

  (**check winner check is appropriate **)
  if (Array.length st.cards <> 0) then 
    ((print_endline "Winner cannot be determined without final card"); st)
  else
    let hand1 = st.hand1 in let hand2 = st.hand2 in 
    let winner =Poker.winner hand1 hand2 (st.table) in match winner with 

    (**if player 1 wins then reassign pots and reset game accordingly **)
    |"player 1" -> 
      print_string "Player 1 wins with "; 
      print_string (format_lst (Array.to_list st.hand1)); 
      print_endline "";
      init_state (st.cash1 + st.pot) (st.cash2) (st.ante) (-(st.started))

    (**if player 2 wins then reassign pots and reset game accordingly **)
    |"player 2" ->
      print_string "Player 2 wins with "; 
      print_string (format_lst (Array.to_list st.hand2)); 
      print_endline "";
      init_state (st.cash1) (st.cash2 + st.pot) (st.ante) (-(st.started))

    (**if players tie then reassign pots and reset game accordingly **)
    |"tie" ->
      winner_tie st
    | _ -> failwith "Not Possible"


(**allows player 1 or player 2 to buy in for [amt] int, amt will be
   added to st.cash1 or st.cash2 depending on whose turn [st.turn] **)
let buyin st amt = 
  if (amt <= 0) then ((print_endline "Must buy in for more than $0" ; st)) else 

    match st.turn with
    | 1 -> { hand1 = st.hand1 ; hand2 = st.hand2 ; 
             table = st.table ; cards = st.cards ; cash1 = st.cash1 + amt; 
             cash2 = st.cash2 ; pot = st.pot
           ; ante = st.ante ; previous_bets_1 = st.previous_bets_1 ;
             previous_bets_2 = st.previous_bets_2
           ; turn = st.turn ; started = st.started ; stage = st.stage ;
             previous_move = st.previous_move} 
    | -1 -> 
      { hand1 = st.hand1 ; hand2 = st.hand2 ; 
        table = st.table ; cards = st.cards ; cash1 = st.cash1; 
        cash2 = st.cash2 + amt ; pot = st.pot
      ; ante = st.ante ; previous_bets_1 = st.previous_bets_1 ;
        previous_bets_2 = st.previous_bets_2 ; 
        turn = st.turn  ; started = st.started ; stage = st.stage ;
        previous_move = st.previous_move} 
    | _ -> failwith "Impossible"


(**END OF NON TURN BASED FUNCTIONS **)


(*BEGINNING OF TURN BASED FUNCTIONS **)

(**takes state and just creates a new initialized state with empty hands and 
   the pot distributed to winner, fold dependent on player turn**)
let fold st = 
  match st.turn with 
  (**check who folds dependent on that is who you change the pot value **)
  |1 -> print_endline "Player 1 folds, player 2 wins."; 
    init_state (st.cash1) (st.cash2 + st.pot) (st.ante) (-(st.started))
  |(-1) -> print_endline "Player 2 folds, player 1 wins."; 
    init_state (st.cash1 + st.pot) (st.cash2) (st.ante) (-(st.started))
  |_ -> failwith "Impossible"

(**if player 1 turn, then intialize new hand with player 1 cash
   (* same and player 2 gaining pot value **)
   then init_state (st.cash1) (st.cash2 + st.pot) (st.ante) (-(st.started))

   (**if player 2 turn, then initialize new hand with player 2 cash same
   and player 1 gaining pot value **)
   else init_state (st.cash1 + st.pot) (st.cash2) (st.ante) (-(st.started)) *)

let bet_helper_1 st amt = 
  if (amt <= st.cash1 + st.previous_bets_1)
  then (
    {hand1 = st.hand1 ; hand2 = st.hand2 ; 
     table = st.table ; cards = st.cards ;
     cash1 = (st.cash1 - amt) + st.previous_bets_1; 
     cash2 = st.cash2 ; pot = (st.pot + amt) - st.previous_bets_1 
    ; ante = st.ante ;
     previous_bets_1 = amt ;
     previous_bets_2 = st.previous_bets_2 ;
     turn = -(st.turn) ; started = st.started ; stage = st.stage ; 
     previous_move = [Bet amt]
    } )
  else ((print_endline "Not enough cash") ; st)

let bet_helper_2 st amt = 
  if (amt <= st.cash2 + st.previous_bets_2)
  then (
    { hand1 = st.hand1 ; hand2 = st.hand2 ; 
      table = st.table ; cards = st.cards ; 
      cash1 = (st.cash1) ; 
      cash2 = (st.cash2-amt) + st.previous_bets_2 ;
      pot = (st.pot + amt) - st.previous_bets_2
    ; ante = st.ante ; 
      previous_bets_1 = st.previous_bets_1 ;
      previous_bets_2 = amt ; 
      turn = -(st.turn) ; started = st.started ; stage = st.stage ; 
      previous_move = [Bet amt]
    } )

  else ((print_endline "Not enough cash") ; st)


(**takes in state and an amount and bets it dependent on player turn **)
let bet st amt = 

  (**checks if anyone is all in, cannot bet after an all in **)
  if (
    match st.turn with 
    | 1 -> st.cash2 = 0
    | _ -> st.cash1 = 0) then 
    (print_endline "Cannot bet when your oppoent is all in.
     You can only call, check, or fold" ; st)
  else
    (**If previous bet was made, you cannot bet again **)
  if (match st.turn with 
      | 1 -> st.previous_bets_2 > 0
      | _ -> st.previous_bets_1 > 0) 
  then ((print_endline "Must call, raise, or fold") ; st)

  else if (amt < st.ante) 
  then ((print_endline ("Please bet at least the ante amount of " 
                        ^ string_of_int st.ante)) ; st)
  else
    match st.turn with 
    | 1 -> bet_helper_1 st amt
    | -1 -> bet_helper_2 st amt 
    | _ -> failwith "Not possible"

(**inputs state where player just bet and sees if it is valid to raise specific
   amount according to rule that raises are atleast 2*previous_bet **)

let raise_helper_1 st amt = 
  if (amt > st.cash1 + st.previous_bets_1) 
  then ((print_endline "Not enough cash") ; st) else 
    ({ hand1 = st.hand1 ; hand2 = st.hand2 ; 
       table = st.table ; cards = st.cards ; 
       cash1 = (st.cash1 - amt) + st.previous_bets_1; 
       cash2 = st.cash2 ; pot = (st.pot + amt) - st.previous_bets_1 ;
       ante = st.ante ; 
       previous_bets_1 = amt ;
       previous_bets_2 = st.previous_bets_2 ;
       turn = -(st.turn) ; started = st.started ; stage = st.stage ;
       previous_move = [Raise amt]

     } )

let raise_helper_2 st amt = 

  (**Check if player2 has enough cash2 to raise the previous_bet**)
  if (amt > st.cash2 + st.previous_bets_2) 
  then ((print_endline "Not enough cash") ; st) else 
    (
      { hand1 = st.hand1 ; hand2 = st.hand2 ; 
        table = st.table ; cards = st.cards ; 
        cash1 = (st.cash1) ; 
        cash2 = st.cash2 - amt + st.previous_bets_2 ;
        pot = (st.pot + amt) - st.previous_bets_2 
      ; ante = st.ante ; 
        previous_bets_1 = st.previous_bets_1 ;
        previous_bets_2 = amt ;
        turn = -(st.turn) ; started = st.started ; stage = st.stage ;
        previous_move = [Raise amt] } )


let raise st amt = 

  if (match st.turn with 
      | 1 -> st.previous_bets_2 = 0
      | _ -> st.previous_bets_1 = 0) 
  then ((print_endline "Cannot raise without previous bet") ; st)
  else if ( match st.turn with 
      | 1 -> st.cash2 = 0
      | _ -> st.cash1 = 0) then 
    (print_endline "Cannot raise your opponent when they are all in. 
    You can only call, check, or fold" ; st)
  else if (match st.turn with 
      | 1 -> amt < 2 * st.previous_bets_2 
      | _ -> amt < 2 * st.previous_bets_1)
  then (print_endline ("Raise must be atleast twice the previous bet of " 
                       ^ string_of_int (match st.turn with 
                           | 1 -> (st.previous_bets_2) 
                           | _ -> (st.previous_bets_1))) ; st )
  else match st.turn with 
    | 1 -> raise_helper_1 st amt
    | -1 -> raise_helper_2 st amt
    | _ -> failwith "Invalid Command"


(**inputs state where player just bet/raised, checks to see if valid to call
   here if it does then returns new state with call, else does not return a 
   new state and says invalid command **)

let call_helper_1 st = 
  if (st.previous_bets_2 > st.cash1 + st.previous_bets_1) 
  then (print_endline ("Not enough cash to call"); st) 
  else 

    { 
      hand1 = st.hand1 ; hand2 = st.hand2 ; 
      table = st.table ; cards = st.cards ; 
      cash1 = (st.cash1 - st.previous_bets_2) + st.previous_bets_1 ; 
      cash2 = st.cash2 ; 
      pot = (st.pot + st.previous_bets_2) - st.previous_bets_1
      ; ante = st.ante ; 
      previous_bets_1 = st.previous_bets_2 ;
      previous_bets_2 = st.previous_bets_2 ; 
      turn = -(st.turn) ; started = st.started ; stage = st.stage ;
      previous_move = [Call]} 

let call_helper_2 st = 


  (**failsafe to check if player1 has enough 
     cash1 to call the previous_bet **)
  if (st.previous_bets_1  > st.cash2 + st.previous_bets_2) 
  then (print_endline ("Not enough cash to call"); st) 
  else 

    { 
      hand1 = st.hand1 ; hand2 = st.hand2 ; 
      table = st.table ; cards = st.cards ; 
      cash1 = (st.cash1) ; 
      cash2 = st.cash2 - st.previous_bets_1 + st.previous_bets_2 ; 
      pot = (st.pot + st.previous_bets_1) - st.previous_bets_2; 
      ante = st.ante ; 
      previous_bets_1 = st.previous_bets_1 ;
      previous_bets_2 = st.previous_bets_1 ; 
      turn = -(st.turn) ; started = st.started ; stage = st.stage ;
      previous_move = [Call]
    } 

let call st = 

  (**checks to see if another bet was made in prior action **)
  if (match st.turn with 
      | 1 -> st.previous_bets_2 = 0
      | _ -> st.previous_bets_1 = 0) then 
    ((print_endline "Cannot call without previous bet") ; st)
  else 

    match st.turn with 
    | 1 -> call_helper_1 st
    | -1 -> call_helper_2 st
    | _ -> failwith "Invalid Command"

(**check does not change anything, just moves the turn counter **)
let check st = 

  if (match st.turn with 
      | 1 -> st.previous_bets_2 > 0
      | _ -> st.previous_bets_1 > 0) 
  then ((print_endline "Must call, raise, or fold") ; st)
  else

    { hand1 = st.hand1 ; hand2 = st.hand2 ; 
      table = st.table ; cards = st.cards ; cash1 = st.cash1 ; 
      cash2 = st.cash2 ; pot = st.pot
    ; ante = st.ante ; previous_bets_1 = st.previous_bets_1
    ; previous_bets_2 = st.previous_bets_2 ; turn = -(st.turn)
    ; started = st.started ; stage = st.stage;

      (*reassign previous_move to Check*)
      previous_move = [Check]
    } 


(**puts the person whose turn it is all in **)

(**if a previous bet was made [call] or [raise] then allin puts all the
   cash of current player in, and removes any difference from the previous bet
   and returns it to player who made that bet, else just puts all in on
   its own cash **)

let allin_helper_1 st = 

  (print_endline "Player 1 is all in" ; 
   (**if your going all in and you have more than player 2's prev bet *)
   if (st.cash1 + st.previous_bets_1 > st.previous_bets_2) then
     { hand1 = st.hand1 ; hand2 = st.hand2 ; 
       table = st.table ; cards = st.cards ; cash1 = (st.cash1 - st.cash1) ; 
       cash2 = st.cash2 ; pot = (st.pot + st.cash1) ; ante = st.ante ; 
       previous_bets_1 = st.cash1 + st.previous_bets_1;
       previous_bets_2 = st.previous_bets_2 ; turn = -(st.turn) ; 
       started = st.started ;stage = st.stage ; previous_move = [Bet st.cash1]} 

   (*this scenario arises when you are going all in against a bet that 
     is more than what you have*)

   else (
     print_endline ("$" ^ (string_of_int (st.previous_bets_2
                                          - st.previous_bets_1 - st.cash1))^
                    " has been returned to Player 2's cash")
   ; { hand1 = st.hand1 ; hand2 = st.hand2 ; 
       table = st.table ; cards = st.cards ; cash1 = st.cash1 - st.cash1 ; 
       cash2 = st.cash2 + (st.previous_bets_2 -st.previous_bets_1 - st.cash1); 
       pot=st.pot-(st.previous_bets_2 - st.previous_bets_1 - st.cash1)+st.cash1
     ; ante = st.ante ; previous_bets_1 = st.cash1 + st.previous_bets_1;
       previous_bets_2 = st.cash1;
       turn = -(st.turn)
     ; started = st.started ; stage = st.stage;previous_move = [Bet st.cash1]}))


let allin_helper_2 st = 

  (
    print_endline "Player 2 is all in" ; 
    if (st.cash2 + st.previous_bets_2 > st.previous_bets_1) then

      (**hands table cards and randomized cards stay same **)
      {hand1 = st.hand1 ; hand2 = st.hand2 ; 
       table = st.table ; cards = st.cards ; cash1 = st.cash1 ; 
       cash2 = st.cash2 - st.cash2 ; pot = (st.pot + st.cash2) ;
       ante = st.ante ;  previous_bets_1 = st.previous_bets_1;
       previous_bets_2 = st.cash2 + st.previous_bets_2; 
       turn = -(st.turn) ; started = st.started ; stage = st.stage ; 
       previous_move = [Bet st.cash2] } 

    (**going all in against a bet that is more valuable than your cash **)
    else (
      print_endline ("$" ^ (string_of_int (st.previous_bets_1 
                                           - st.previous_bets_2 - st.cash2))
                     ^ " has been returned to Player 1's cash") ; 
      { hand1 = st.hand1 ; hand2 = st.hand2 ; table = st.table ; 
        cards = st.cards ; 
        cash1 = st.cash1 + (st.previous_bets_1 -st.previous_bets_2 - st.cash2) ; 
        cash2 = st.cash2 - st.cash2; 
        pot = st.pot-(st.previous_bets_1 -st.previous_bets_2-st.cash2) +st.cash2
      ; ante = st.ante ; previous_bets_1 = st.cash2 ; 
        previous_bets_2 = st.cash2 ; turn = -(st.turn)
      ;started = st.started ; stage = st.stage;previous_move = [Bet st.cash2]} )
  )



let allin st = 

  (**all in is in reaction to a bet or raise, must calculate final pot **)

  (match st.turn with 
   (**player 1 turn, matches the max amount of previous bet it can **)
   |1 -> 
     if (st.cash1 = 0) then (print_endline "You are already all in"; st ) else 
     if (st.cash2 = 0 && st.previous_bets_2 < st.cash1) 
     then (print_endline "Player 1 calls" ;  (call st)) 
     else allin_helper_1 st
   |_ -> 
     if (st.cash2 = 0) then (print_endline "You are already all in"; st ) else 
     if (st.cash1 = 0 && st.previous_bets_1 < st.cash2) 
     then (print_endline "Player 2 calls" ;  (call st)) 
     else allin_helper_2 st
  )


(**END OF TURN BASED FUNCTIONS **)


(**Helper FUnction for Mail to determine if time to update new cards **)

(**helper function that calls new cards, helps new_cards when matched **)

let newcards_helper st cmd = 
  match st.stage with 
  |1 -> (match cmd with
      | Call -> flop (call st)
      | Check -> flop st
      | AllIn -> flop (allin st)
      | _ -> st)
  |2 -> (match cmd with
      | Call -> turn (call st)
      | Check -> turn st
      | AllIn -> turn (allin st)
      | _ -> st)
  |3 -> (match cmd with
      | Call -> river (call st)
      | Check -> river st
      | AllIn -> river (allin st)
      | _ -> st)
  |4 -> (match cmd with
      | Call -> winner (call st)
      | Check -> winner st
      | AllIn -> winner (allin st)
      | _ -> st)
  | _ -> st

(**returns [true] if all in next cards are triggered, [false] otherwise **)
let allin_helper st = 
  (match st.previous_move with 
   | [] -> false 
   | h::t -> (match h with 
       | Bet int -> true 
       | _ -> false))

(**new cards helper to tel *)
let new_helper st = 

  match st.turn with 
  | 1 -> if st.previous_bets_2 > st.cash1 + st.previous_bets_1
    then true else false
  | _ -> if st.previous_bets_1 > st.cash2 + st.previous_bets_1 
    then true else false

let new_helper_2 st = 
  match st.previous_move with 
  | [] -> true
  | h::t -> (match h with
      | Bet amt -> false
      | Raise amt -> false
      | _ -> true
    )

(**input a state and outputs either
   [new state] with updated cards from flop, deal, turn, river, or returns 
   original state signifying it is not time for new cards**)
let new_cards st cmd = 

  match cmd with 

  (*If the action is Call, then automatically move to check which updated state 
    to return**)
  | Call -> (
      if (new_helper_2 st)
      then call st 
      else ( if (new_helper st)
             then (call st)
             else newcards_helper (st) (Call) ))

  (**If action is Check, then checks to see if previous move was also Check, if
     not then returns original state, if it is then return updated state **)
  | Check -> if st.previous_move <> [Check] then check st else (
      newcards_helper (st) (Check)
    )

  (**If action is AllIn, then checks to see if previous move was also
     an AllIn, if not then returns original state with [AllIn] applied **)
  | AllIn -> (
      match st.turn with 
      | 1 -> if (allin_helper st && st.cash2 = 0)
        then (newcards_helper (st) (AllIn))
        else (allin st)

      |_ -> if (allin_helper st && st.cash1 = 0)
        then (newcards_helper (st) (AllIn))
        else (allin st)

    )

  (**All other actions will not trigger a new set of cards, so return original 
     state **)
  | _ -> st


(** List.hd is not working **)







