(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw3.sml";

val test_oc1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test_oc2 = only_capitals ["A","B","C", "den"] = ["A","B","C"]
val test_oc3 = only_capitals ["very", "A", "cool", "B", "stuff", "C"] = ["A","B","C"]
val test_oc4 = only_capitals ["A","B","ho"] = ["A","B"]

val test_ls1 = longest_string1 ["A","bc","C"] = "bc"
val test_ls2 = longest_string1 ["Abracadabra","bc","C"] = "Abracadabra"
val test_ls3 = longest_string1 ["Ab","bc","C"] = "Ab"
val test_ls4 = longest_string1 ["A","bc","Cunt"] = "Cunt"

val test_lst1 = longest_string2 ["A","bc","C"] = "bc"
val test_lst2 = longest_string2 ["Abracadabra","bc","C"] = "Abracadabra"
val test_lst3 = longest_string2 ["Ab","bc","C"] = "bc"
val test_lst4 = longest_string2 ["A","bc","Cunt"] = "Cunt"

val test_lstt1 = longest_string3 ["A","bc","C"] = "bc"
val test_lstt2 = longest_string3 ["Abracadabra","bc","C"] = "Abracadabra"
val test_lstt3 = longest_string3 ["Ab","bc","C"] = "Ab"
val test_lstt4 = longest_string3 ["A","bc","Cunt"] = "Cunt"

val test_lsttt1 = longest_string4 ["A","bc","C"] = "bc"
val test_lsttt2 = longest_string4 ["Abracadabra","bc","C"] = "Abracadabra"
val test_lsttt3 = longest_string4 ["Ab","bc","C"] = "bc"
val test_lsttt4 = longest_string4 ["A","bc","Cunt"] = "Cunt"

val test_lc1 = longest_capitalized ["A","bc","C"] = "A" 
val test_lc2 = longest_capitalized ["A","bc","Cunt"] = "Cunt" 
val test_lc3 = longest_capitalized ["Abra","bc","Cunt"] = "Abra" 
val test_lc4 = longest_capitalized ["a","bc","c"] = "" 

val test_rs1 = rev_string "abc" = "cba"
val test_rs2 = rev_string "abcABC" = "CBAcba"
val test_rs3 = rev_string "abcCBA" = "ABCcba"
val test_rs4 = rev_string "hello world" = "dlrow olleh"

val test_fa1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test_fa2 = first_answer (fn x => if x > 4 then SOME x else NONE) [1,2,3,4,5] = 5
val test_fa3 = first_answer (fn x => if x > 1 then SOME x else NONE) [1,2,3,4,5] = 2
val test_fa4 = (first_answer (fn x => if x > 5 then SOME x else NONE)
[1,2,3,4,5] handle _ => 42) = 42

val test_aa1 = all_answers (fn x => if x = 1 then SOME [x] else NONE)
 [2,3,4,5,6,7] = NONE
val test_aa2 = all_answers (fn x => if x > 5 then SOME [x] else NONE)
 [2,3,4,5,6,7] = NONE 
val test_aa3 = all_answers (fn x => if x > 6 then SOME [x] else NONE)
 [2,3,4,5,6,7] = NONE 
val test_aa4 = all_answers (fn x => if x > 1 then SOME [x] else NONE)
 [2,3,4,5,6,7] = SOME [2, 3, 4, 5, 6, 7] 

val test9a1 = count_wildcards Wildcard = 1
val test9a2 = count_wildcards (TupleP([Wildcard, Wildcard])) = 2

val test9b1 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b2 = count_wild_and_variable_lengths (Variable("abc")) = 3
val test9b3 = count_wild_and_variable_lengths (TupleP [Variable("a"), Wildcard]) = 2

val test9c1 = count_some_var ("x", Variable("x")) = 1
val test9c2 = count_some_var ("x", (TupleP [Variable("x"), Variable("hih"),
Variable("x")])) = 2

(* val test9c3 = count_some_var ("x", Variable("x")) = 1 *)

val test_cp1 = check_pat (Variable("x")) = true
val test_cp2 = check_pat (TupleP [Variable("x"), Variable("x")]) = false 
val test_cp3 = check_pat (TupleP [Variable("hih"), Variable("hih"),
Variable("hoho")]) = false 
val test_cp4 = check_pat (Variable("x")) = true

val test_m1 = match (Const(1), UnitP) = NONE
val test_m2 = match (Const(1), ConstP(1)) = SOME[] 
val test_m3 = match (Const(1), Variable("shit")) = SOME [("shit", Const(1))] 
val test_m4 = match (Tuple([Const(1), Unit]), TupleP([ConstP(1), UnitP])) = SOME
[]
val test_m5 = match (Tuple([Const(1), Unit]), TupleP([ConstP(2), UnitP])) = NONE

val test12 = first_match Unit [UnitP] = SOME []
val test13 = first_match (Const(1)) [UnitP] = NONE
val test14 = first_match (Const(1)) [UnitP, ConstP(1)] = SOME []



