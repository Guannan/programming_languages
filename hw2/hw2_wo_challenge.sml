(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str_pattern : string, str_list : string list) = 
    let fun iter_str_list (cur_list, acc) = 
	    case cur_list of
		[] => acc  
	      | first_str::rest_str => 
		if same_string (str_pattern,first_str) 
		then iter_str_list (rest_str, acc)  (*leaving out the strings matching str_pattern *)
		else iter_str_list (rest_str, acc @ [first_str])  (*append the strings that are different from str_pattern *)
    in
	if length str_list = length (iter_str_list (str_list,[]))
	then NONE
	else SOME (iter_str_list (str_list,[])) 
    end

fun get_substitutions1 (substitutions : string list list, str_pattern : string) = 
    case substitutions of
	[] => []
      | first_str_list::rest_str_list => 
	case all_except_option (str_pattern, first_str_list) of
	    NONE => get_substitutions1 (rest_str_list, str_pattern)
	  | SOME str_list => str_list @ get_substitutions1 (rest_str_list, str_pattern)

fun get_substitutions2 (substitutions : string list list, str_pattern : string) = 
    let fun iter_list_list (cur_list,acc) = 
	    case cur_list of 
		[] => acc
	      | first_list::rest_list => 
		case all_except_option (str_pattern, first_list) of
		    NONE => iter_list_list (rest_list, acc)
		  | SOME str_list => iter_list_list (rest_list, acc @ str_list) 
    in
	iter_list_list (substitutions, [])
    end

fun similar_names (substitutions : string list list, {first=f,middle=m,last=l}) = 
    let fun iter_sub_list (cur_list, acc) =  
	    case cur_list of
		[] => acc
	      | first_str::rest_str => iter_sub_list (rest_str, acc @  [{first=first_str,middle=m,last=l}])
    in
	iter_sub_list (get_substitutions2 (substitutions, f), [{first=f,middle=m,last=l}])
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

fun card_color (c) = 
    case c of
	(Diamonds,_) => Red
      | (Hearts,_) => Red
      | (_,_) => Black 

fun card_value (s,r) =
    case r of
	Ace => 11
      | Num i => i
      | _ => 10 

fun remove_card (cs, c, e) = 
    case cs of
	[] => raise e
      | first_c::rest_c => if first_c = c then rest_c else first_c::remove_card(rest_c,c,e) 

fun all_same_color (cs) = 
    case cs of
	[] => true
      | first_c::[] => true
      | first_c::second_c::rest_c => if card_color (first_c) = card_color (second_c) 
				     then true andalso all_same_color (second_c::rest_c)
				     else false

fun sum_cards (cs) = 
    let fun iter_card_list (card_list, acc) =
	    case card_list of
		[] => acc
	      | first_c::rest_c => iter_card_list (rest_c, acc + card_value first_c)
    in
	iter_card_list(cs, 0)
    end

fun score (cs, goal) =
    let fun preliminary_score (score, goal) =
	    case score > goal of
		true => 3 * (score - goal)
	      | false => goal - score
				    
	fun final_score (prelim_score, color_match) =
	    case color_match of
		true => prelim_score div 2
	      | false => prelim_score
    in
	final_score (preliminary_score (sum_cards (cs), goal), all_same_color (cs))
    end

fun officiate (cs, move, goal) =
    let fun turn (deck, mv, held) = 
	    case (deck, mv, held) of  
		(_, [], held) => score (held, goal)
	      | ([], Draw::rest_mv, held) => score (held, goal)
	      | (deck, Discard c::rest_mv, held) => turn (deck, rest_mv, remove_card (held, c, IllegalMove))
	      | (top_c::rest_c, Draw::rest_mv, held) => if sum_cards (top_c::held) > goal   (*greater or greater/equal to??*)
							then score (top_c::held, goal)
							else turn (rest_c, rest_mv, top_c::held)
    in
	turn (cs, move, [])
    end

