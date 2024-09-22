(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw2.sml";

val test_aeo1 = all_except_option ("string", ["string"]) = SOME []
val test_aeo2 = all_except_option ("string", ["string", "hi", "sir"]) = SOME ["hi", "sir"]
val test_aeo3 = all_except_option ("string", ["hello", "string", "mister"]) =
  SOME ["hello", "mister"]
val test_aeo4 = all_except_option ("", ["", "hi"]) = SOME ["hi"]

val test_gss1 = get_substitutions1 ([["kek"],["there"]], "foo") = []
val test_gss2 = get_substitutions1 ([["foo", "hihi"],["there"]], "foo") = ["hihi"]
val test_gss3 = get_substitutions1 ([["foo", "hello", "there",
"mister"],["there"]], "foo") = ["hello", "there", "mister"]
val test_gss4 = get_substitutions1 ([["foo", "hello"],["there", "foo",
"mister"]], "foo") = ["hello", "there", "mister"]

val test_gs1 = get_substitutions2 ([["kek"],["there"]], "foo") = []
val test_gs2 = get_substitutions2 ([["foo", "hihi"],["there"]], "foo") = ["hihi"]
val test_gs3 = get_substitutions2 ([["foo", "hello", "there",
"mister"],["there"]], "foo") = ["hello", "there", "mister"]
val test_gs4 = get_substitutions2 ([["foo", "hello"],["there", "foo",
"mister"]], "foo") = ["hello", "there", "mister"]
val test_gs5 = get_substitutions2 ([["foo"],["there"]], "foo") = []

 val test4 = similar_names
 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
 {first="Fred", middle="W", last="Smith"}) = 
  [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
  {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith",
     middle="W"}]


val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test_rc1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test_rc2 = remove_card ([(Hearts, Ace), (Hearts, Ace), (Spades, Num 3)], (Hearts, Ace),
IllegalMove) = [(Hearts, Ace), (Spades, Num 3)]
val test_rc3 = (remove_card ([], (Hearts, Ace), IllegalMove) handle IllegalMove
=> [(Clubs, Ace)]) = [(Clubs, Ace)]

val test_asc1 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test_asc2 = all_same_color [(Hearts, Ace), (Spades, Ace)] = false 
val test_asc3 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Spades, Ace)] =
  false 

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

 val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3
             
 val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)

             
             

