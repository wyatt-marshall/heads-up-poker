open Poker
open OUnit2

(** TESTING STRATEGY: Our testing strategy revolved mostly around testing the
    functions from the [Poker] module in OUnit, and then play testing the remainder 
    of the game. We thought this to be prudent, as we could not realistically
    play enough poker to catch all of the crazy edge cases (something like an Ace-low
    flush, for examply, is incredibly rare) that occur in evaluating a winner in poker. 
    Additionally, we wanted to make sure that the functions that deal out cards are dealing
    out the correct number of cards every time, as our internal system relies heavily
    on this. We felt we could play-test the rest sufficiently, as to capture all possible 
    player actions in this two-player game. So, for the test cases done in OUnit, 
    all of which were from the [Poker] module, we went with a black-box testing 
    procedure. We first wanted to make sure that hands that were a certain "pattern"
    evaluated correctly for their appropriate functions (ie a hand that is a 
    flush, evaluated by the [best_flush] function, returns the cards that form a flush).
    After that, we then wanted to make sure that the appropriate [winner] is selected,
    and tested that function extensively to ensure that all edge cases we could think of
    (as well as normal cases) were covered. For play testing, our strategy was more
    glass-box, as we tried to go through and do every command (and combination of
    commands) that would hit every line of code we wrote in [state], [main], and
    command. Lastly, to test [strategies], we mainly just played the game, and 
    made sure the AI was making reasonable choices. Since its actions are probabilistic,
    we could only hope for it to be somewhat reasonable, and legal, which it was. 
    Overall, we believe this to show the correctness of our system, as between the things
    we tested in OUnit and those we playtested, we tested all possible things that could
    happen in a poker game. *)



(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. We credit the author of A2.test
    for writing this function. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2


(* let make_win_test 
    (name :string)
    (h1 : hand) 
    (h2: hand) 
    (t: table)
    (expected_output : string) : test = 
   name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (winner h1 h2 t)) *)

let hand i1 i2 = 
  (Array.concat [(Array.sub full_deck i1 1);(Array.sub full_deck i2 1)])

let table i1 i2 i3 i4 i5 i6 i7= 
  (Array.concat [(Array.sub full_deck i1 1);(Array.sub full_deck i2 1);
                 (Array.sub full_deck i3 1);(Array.sub full_deck i4 1);(Array.sub full_deck i5 1);
                 (Array.sub full_deck i6 1);(Array.sub full_deck i7 1)])

let b_five i1 i2 i3 i4 i5 = 
  (Array.concat [(Array.sub full_deck i1 1);(Array.sub full_deck i2 1);
                 (Array.sub full_deck i3 1);(Array.sub full_deck i4 1);(Array.sub full_deck i5 1)])

let hand_lst i1 i2 = 
  Array.to_list (Array.concat [(Array.sub full_deck i1 1);(Array.sub full_deck i2 1)])

let table_lst i1 i2 i3 i4 i5 i6 i7= 
  Array.to_list (Array.concat [(Array.sub full_deck i1 1);(Array.sub full_deck i2 1);
                               (Array.sub full_deck i3 1);(Array.sub full_deck i4 1);(Array.sub full_deck i5 1);
                               (Array.sub full_deck i6 1);(Array.sub full_deck i7 1)])

let b_five_lst i1 i2 i3 i4 i5 = 
  Array.to_list (Array.concat [(Array.sub full_deck i1 1);(Array.sub full_deck i2 1);
                               (Array.sub full_deck i3 1);(Array.sub full_deck i4 1);(Array.sub full_deck i5 1)])

let tests = "poker test suite" >::: [
    "deal is right cards for hand" >:: (fun _ -> assert_equal (hand 4 2)
                                           (snd (deal (table 2 4 5 6 7 8 9) (Array.of_list []))));
    "deal is right cards for table" >:: (fun _ -> assert_equal (b_five 5 6 7 8 9)
                                            (fst (deal (table 2 4 5 6 7 8 9) (Array.of_list []))));
    "shuffle is 9 cards" >:: (fun _ -> assert_equal 9 
                                 (List.length 
                                    (Array.to_list (shuffle full_deck (Array.of_list [])))));
    "highest 5"  >:: (fun _ -> assert_equal 
                         true (cmp_set_like_lists 
                                 (b_five_lst 11 12 9 10 8) 
                                 (highest_n 5 (table_lst 1 2 8 9 10 11 12) [])));
    "not a flush"  >:: (fun _ -> assert_equal 
                           true (cmp_set_like_lists 
                                   ([]) 
                                   (best_flush (table_lst 1 13 14 2 44 45 46))));
    "hearts flush"  >:: (fun _ -> assert_equal 
                            true (cmp_set_like_lists 
                                    (b_five_lst 12 11 5 3 2) 
                                    (best_flush (table_lst 12 11 3 2 5 45 46))));
    "diamonds flush"  >:: (fun _ -> assert_equal 
                              true (cmp_set_like_lists 
                                      (b_five_lst 13 15 17 16 22) 
                                      (best_flush (table_lst 22 16 17 13 5 15 40))));
    "clubs flush"  >:: (fun _ -> assert_equal 
                           true (cmp_set_like_lists 
                                   (b_five_lst 30 35 32 37 29) 
                                   (best_flush (table_lst 29 37 32 35 1 2 30))));
    "indifferent to order"  >:: (fun _ -> assert_equal 
                                    true (cmp_set_like_lists 
                                            (b_five_lst 12 11 5 3 2) 
                                            (best_flush (table_lst 11 3 12 2 41 45 5))));
    "indifferent to order"  >:: (fun _ -> assert_equal 
                                    true (cmp_set_like_lists 
                                            (b_five_lst 12 11 5 3 2) 
                                            (best_flush (table_lst 3 45 12 2 41 11 5))));
    "not a straight" >:: (fun _ -> assert_equal 
                             true (cmp_set_like_lists ([]) 
                                     (best_straight (table_lst 1 2 7 10 12 8 6) [])));
    "straight" >:: (fun _ -> assert_equal 
                       true (cmp_set_like_lists (b_five_lst 12 3 28 40 0) 
                               (best_straight (table_lst 40 28 3 35 20 12 0) [])));
    "straight flush is straight" >:: (fun _ -> assert_equal 
                                         true (cmp_set_like_lists (b_five_lst 1 2 3 4 5) 
                                                 (best_straight (table_lst 1 2 3 5 4 22 23) [])));
    "straight flush is flush" >:: (fun _ -> assert_equal 
                                      true (cmp_set_like_lists (b_five_lst 1 2 3 4 5) 
                                              (best_flush (table_lst 1 2 3 5 4 22 23))));
    "ace-low straight" >:: (fun _ -> assert_equal 
                               true (cmp_set_like_lists (b_five_lst 1 2 3 4 0) 
                                       (best_straight (table_lst 1 2 3 0 4 22 23) [])));
    "higher straight" >:: (fun _ -> assert_equal 
                              true (cmp_set_like_lists (b_five_lst 1 2 3 4 5) 
                                      (best_straight (table_lst 1 2 3 0 4 5 23) [])));
    "on the board" >:: (fun _ -> assert_equal 
                           "tie" (winner (hand 12 11) (hand 10 9) (b_five 0 1 2 3 4)));
    "high card same 2 pair" >:: (fun _ -> assert_equal 
                                    "player 1" (winner (hand 48 51)
                                                  (hand 50 49) (b_five 0 1 3 14 13)));
    "flush beats straight" >:: (fun _ -> assert_equal 
                                   "player 1" (winner (hand 0 10) (hand 50 49) 
                                                 (b_five 0 1 3 14 13)));
    "straight beats pair" >:: (fun _ -> assert_equal 
                                  "player 2" (winner (hand 0 51) (hand 20 21) 
                                                (b_five 4 5 6 14 13)));
    "order on table doesn't matter" >:: (fun _ -> assert_equal 
                                            "player 2" (winner (hand 0 51) (hand 20 21) 
                                                          (b_five 6 5 13 14 4)));
    "better full house" >:: (fun _ -> assert_equal "player 2" 
                                (winner (hand 0 38) (hand 1 25) (b_five 51 12 14 13 35)));
    "pair with higher cards" >:: (fun _ -> assert_equal "player 1" 
                                     (winner (hand 38 4) (hand 19 3) (b_five 12 23 22 47 0)));
    "straight beats 2 pair" >:: (fun _ -> assert_equal "player 1" 
                                    (winner (hand 40 28) (hand 51 9) (b_five 12 35 3 17 0)));
    "ace low straight unsuited" >:: (fun _ -> assert_equal "player 1" 
                                        (winner (hand 40 28) (hand 51 9) (b_five 12 35 3 50 0)));
    "high card tie" >:: (fun _ -> assert_equal "tie" 
                            (winner (hand 1 11) (hand 14 24) (b_five 45 46 50 23 20)));
    "flush on board tie" >:: (fun _ -> assert_equal "tie" 
                                 (winner (hand 1 11) (hand 14 24) (b_five 43 45 48 50 51)));
    "better flush" >:: (fun _ -> assert_equal "player 1" 
                           (winner (hand 1 51) (hand 14 42) (b_five 43 45 48 50 49)));
    "royal flush better" >:: (fun _ -> assert_equal "player 1" 
                                 (winner (hand 1 51) (hand 14 46) (b_five 45 47 48 50 49)));
    "tie royal flush" >:: (fun _ -> assert_equal "tie" 
                              (winner (hand 1 46) (hand 14 42) (b_five 51 50 49 48 47)));
    "better 3 kind" >:: (fun _ -> assert_equal "player 1" 
                            (winner (hand 1 14) (hand 0 13) (b_five 51 50 26 27 49)));
    "better full house" >:: (fun _ -> assert_equal "player 1" 
                                (winner (hand 1 14) (hand 0 13) (b_five 12 25 26 27 49)));
    "FH indifferent to order" >:: (fun _ -> assert_equal "player 2" 
                                      (winner (hand 13 0) (hand 1 14) (b_five 49 25 27 26 12)));
    "best FH is on table" >:: (fun _ -> assert_equal "tie" 
                                  (winner (hand 1 14) (hand 0 13) (b_five 7 20 8 21 34)));
    "four kind beats 3 kind" >:: (fun _ -> assert_equal "player 1" 
                                     (winner (hand 1 14) (hand 0 13) (b_five 27 40 33 21 26)));
    "better 4 kind" >:: (fun _ -> assert_equal "player 1" 
                            (winner (hand 1 14) (hand 0 13) 
                               (b_five 27 40 33 26 39)));
    "low 4 kind high 3 kind" >:: (fun _ -> assert_equal "player 2" 
                                     (winner (hand 1 14) (hand 0 13) 
                                        (b_five 23 40 33 26 39)));
    "better two pair pocket" >:: (fun _ -> assert_equal "player 1" 
                                     (winner (hand 1 14) (hand 0 13) 
                                        (b_five 23 36 37 5 51)));
    "two pair pocket table mix" >:: (fun _ -> assert_equal "player 1" 
                                        (winner (hand 0 36) (hand 1 39) 
                                           (b_five 23 13 33 26 14)));
    "two pair table high pocket" >:: (fun _ -> assert_equal "player 1" 
                                         (winner (hand 51 0) (hand 5 7) 
                                            (b_five 23 40 8 21 50)));
    "same two pair same high" >:: (fun _ -> assert_equal "tie" 
                                      (winner (hand 12 0) (hand 51 13) 
                                         (b_five 26 8 21 22 49)));
    "high card same pair" >:: (fun _ -> assert_equal "player 2" 
                                  (winner (hand 12 15) (hand 25 24) 
                                     (b_five 51 42 44 45 34)));
    "flush tie break" >:: (fun _ -> assert_equal "player 1" 
                              (winner (hand 12 4) (hand 11 2) 
                                 (b_five 0 3 6 8 9)));
    "flush second tie break" >:: (fun _ -> assert_equal "player 1" 
                                     (winner (hand 7 4) (hand 5 2) 
                                        (b_five 0 3 6 8 9)));
    "straight flush better than straight" >:: (fun _ -> assert_equal "player 1" 
                                                  (winner (hand 1 2) (hand 14 15) 
                                                     (b_five 0 3 4 8 9)));
    "straight flush better than flush" >:: (fun _ -> assert_equal "player 1" 
                                               (winner (hand 2 4) (hand 11 12) 
                                                  (b_five 0 3 1 8 9)));
    "3 kinds beats pair" >:: (fun _ -> assert_equal "player 1" 
                                 (winner (hand 2 15) (hand 28 51) 
                                    (b_five 41 3 1 8 17)));
    "3 kinds beats lower 2 pair" >:: (fun _ -> assert_equal "player 1" 
                                         (winner (hand 12 25) (hand 0 10) 
                                            (b_five 51 23 13 8 9)));
    "3 kinds beats higher 2 pair" >:: (fun _ -> assert_equal "player 2" 
                                          (winner (hand 12 25) (hand 0 10) 
                                             (b_five 36 23 34 8 9)));
    "straight beats 3 kind" >:: (fun _ -> assert_equal "player 1" 
                                    (winner (hand 12 25) (hand 0 10) 
                                       (b_five 36 23 37 8 9)));

  ]




let _ = run_test_tt_main tests