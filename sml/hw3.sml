(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

fun only_capitals xs =
  List.filter (fn x => Char.isUpper (String.sub (x, 0))) xs

fun longest_string1 xs =
  List.foldl (fn (x, y) => if String.size y >= String.size x then y else x) ""
  xs

fun longest_string2 xs =
  List.foldl (fn (x, y) => if String.size x >= String.size y then x else y) ""
  xs

fun longest_string_helper f =
  List.foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y) 
val longest_string4 = longest_string_helper (fn (x, y) => x >= y) 

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
  case xs of
       [] => raise NoAnswer
     | x::xs => case f x of
                     SOME v => v
                   | NONE => first_answer f xs


fun all_answers f xs =
  let 
    fun aux xs acc =
      case xs of
           [] => SOME acc
         | (x::xs) => case f x of
                         NONE => NONE 
                       | SOME x => (aux xs (acc @ x))
      in
        aux xs []
      end

datatype pattern = Wildcard
           | Variable of string
           | UnitP
           | ConstP of int
           | TupleP of pattern list
           | ConstructorP of string * pattern

datatype valu = Const of int
            | Unit
            | Tuple of valu list
            | Constructor of string * valu

fun g f1 f2 p =
    let 
      val r = g f1 f2 
    in
      case p of
          Wildcard          => f1 ()
        | Variable x        => f2 x
        | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
        | ConstructorP(_,p) => r p
        | _                 => 0
    end

fun count_wildcards p = 
  g (fn _ => 1) (fn _ => 0) p

fun count_wild_and_variable_lengths p =
  g (fn _ => 1) (fn x => String.size x) p

fun count_some_var (str, p) =
  g (fn _ => 0) (fn x => if x = str then 1 else 0) p

fun check_pat p =
  let fun h p xs =
    case p of
         Variable x => x :: xs
       | Wildcard => xs
       | TupleP ps => (List.foldl (fn (p, i) => (h p i)) [] ps) @ xs
       | ConstructorP(_, p) => h p xs
       | _ => []
  fun non_duplicate x xs =
    case xs of
         [] => true 
       | y::ys => if x = y then false else non_duplicate x ys
  fun check xs =
    case xs of
         [] => true
       | x::xs => if (non_duplicate x xs) then check xs else false
  in 
    check (h p [])
  end


(* datatype pattern = Wildcard
           | Variable of string
           | UnitP
           | ConstP of int
           | TupleP of pattern list
           | ConstructorP of string * pattern

datatype valu = Const of int
            | Unit
            | Tuple of valu list
            | Constructor of string * valu *)




(* fun match (valu, pat) =
  case pat of
       Variable x => SOME [x, valu]
     | Wildcard => SOME []
     | ConstP x => case valu of
                        Const y => if x = y then SOME [] else NONE 
                      | _ => NONE
     | UnitP => case valu of
                     Unit => SOME []
                   | _ => NONE
     | ConstructorP (s1, p1) => case valu of
                                   Constructor (s2, v1) => if (s1 = s2) then
                                     match (v1, p1)
                                   else NONE
                                 | _ => NONE
     | TupleP ps => case valu of
                         Tuple vs => NONE (* all_answers match (vs, ps) *)
                       | _ => NONE *)

fun match (valu, pat) = 
  case (valu, pat) of
       (Const x, ConstP y) => if x = y then SOME [] else NONE
     | (Unit, UnitP) => SOME [] 
     | (Constructor (s1, v), ConstructorP (s2, p)) => 
         if s1 = s2 then match (v,p) else NONE 
     | (Tuple vs, TupleP ps) => all_answers match (ListPair.zip(vs, ps))
     | (_, Wildcard) => SOME []
     | (v, Variable x) => SOME [(x, v)]
     | (_, _) => NONE

fun first_match valu pats =
  let val xs = List.map (fn x => (valu, x)) pats
  in
    (SOME (first_answer match xs)) handle NoAnswer => NONE
  end
  (*(first_answer match (List.map (fn x => (valu, x)) pats) handle NoAnswer =>
  * NONE)*)



(**** for the challenge problem only ****)

datatype typ = Anything
            | UnitT
            | IntT
            | TupleT of typ list
            | Datatype of string

(**** you can put all your code here ****)
