(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun reverse xs =
  let fun reverse_helper(xs, acc) =
      case xs of
           [] => acc
         | x::xs' => reverse_helper(xs', x::acc)
  in
    reverse_helper(xs, [])
  end


(* put your solutions for problem 1 here *)

fun all_except_option (str, xs) =
  let fun aux (ys, zs, flag) =
      case ys of
          [] => if flag then SOME (reverse(zs)) else NONE
        | y::ys' => if same_string (y, str) then
              aux(ys', zs, true)
            else aux(ys', y::zs, flag)
  in 
    aux (xs, [], false)
  end

fun get_substitutions1 (xs, str) =
    case xs of
         [] => []
       | x::xs' => 
           case all_except_option(str, x) of
                NONE => get_substitutions1(xs', str)
              | SOME y => y @ get_substitutions1(xs', str)

val x = 0

fun get_substitutions2 (xs, str) =
  let fun aux (xs, ys) =
    case xs of
         [] => reverse(ys)
       | x::xs' =>
           case all_except_option(str, x) of
                NONE => aux(xs', ys)
              | SOME y => aux(xs', reverse(y) @ ys)
  in
    aux(xs, [])
  end

fun similar_names (xs, name) =
  let 
    val {first=f, middle=s, last=t} = name
    fun gs_unwrap (xs, ys) =
      case xs of
           [] => ys 
         | x::xs' => gs_unwrap(xs', {first=x, middle=s, last=t}::ys)
  in
    name::reverse(gs_unwrap(get_substitutions2(xs, f), []))
  end





(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (suit, _) =
  case suit of
       Spades => Black
     | Clubs => Black
     | _ => Red

fun card_value (_, rank) =
  case rank of
       Ace => 11
     | Num i => i
     | _ => 10

fun remove_card (cs, c, e) =
  let fun helper (cs, ys, flag) =
    case cs of
         [] => if flag then ys else raise e
       | x::cs' => 
            if x = c andalso (not flag) then
             helper(cs', ys, true)
            else helper(cs', x::ys, flag)
  in
    reverse(helper(cs, [], false))
  end

fun all_same_color cs =
  case cs of
       [] => true
     | x::[] => true
     | x::y::xs' => all_same_color(y::xs') andalso (card_color(x) = card_color(y))

fun sum_cards cs =
  let fun helper (cs, sum) =
    case cs of
         [] => sum
       | card::cs' => helper(cs', sum + card_value(card))
  in
    helper(cs, 0)
  end

fun score (cs, goal) =
  let 
    val sum = sum_cards(cs)
    val prelim = if sum > goal then 3 * (sum - goal) else (goal - sum)
  in
    if all_same_color (cs) then
      prelim div 2
    else prelim
  end

fun officiate (cs, ms, goal) = 
  let 
    fun draw(cs, hand) =
      case cs of
           [] => (cs, hand)
         | x::xs' => (xs', x::hand)
    fun move(cs, hand, move) =
      case move of
           Discard card => (cs, remove_card (hand, card, IllegalMove))
         | Draw => draw(cs, hand)
    fun state (cs, ms, hand) =
    case ms of
         [] => score(hand, goal)
       | x::xs => 
           let 
             val (cs, hand) = move(cs, hand, x)
           in
             case (cs, hand) of
                  ([], _) => score(hand, goal)
                | (cs, hand) => 
                    if sum_cards (hand) > goal 
                    then score (hand, goal)
                    else state (cs, xs, hand)
          end
    in
      state (cs, ms, [])
    end

      
(* datatype move = Discard of card | Draw  *)
    
    
