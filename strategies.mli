(** Builds strategies for the CPU to use in playing against human opponents. *)

open Poker
open State
open Command

(** [easy_strat st] is the following move of the computer, given the 
    currrent state. For this particularly easy strategy, that means calling 
    all bets, and checking otherwise.*)
val easy_strat : State.t -> Command.move

(** [hard_strat st] is the following move of the computer, given the currrent state,
    according to an informed betting strategy. *)
val hard_strat : State.t -> Command.move