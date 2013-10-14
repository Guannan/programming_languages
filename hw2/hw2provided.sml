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
	iter_sub_list (get_substitutions2 (substitutions, f), [])
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

(*
val t_all_except_option = all_except_option("string", ["string"])
val t_all_except_option2 = all_except_option("str", ["string"])
val t_all_except_option3 = all_except_option("string", [])
val t_all_except_option4 = all_except_option("meme", ["string","meme","bob","joe"])
val t_all_except_option5 = all_except_option("", ["string","meme","","goe"])
val t_get_substitutions1 = get_substitutions1([["foo"],["there"]], "foo")
val t_get_substitutions1_2 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred")
val t_get_substitutions1_3 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff")
val t_get_substitutions2 = get_substitutions2([["foo"],["there"]], "foo")
val t_get_substitutions2_2 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred")
val t_get_substitutions2_3 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff")
*)

val t_similar_names = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})

