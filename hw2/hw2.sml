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

fun sum_cards_ace_to_1 (cs) = 
    let fun ace_to_1 (card_list) =
	    case card_list of
		[] => card_list
	      | first_card::rest_card => case first_card of
					     (Clubs, Ace) => (Clubs, Num 1)::ace_to_1 (rest_card) 
					   | (Diamonds, Ace) => (Diamonds, Num 1)::ace_to_1 (rest_card)
					   | (Hearts, Ace) => (Hearts, Num 1)::ace_to_1 (rest_card)
					   | (Spades, Ace) => (Spades, Num 1)::ace_to_1 (rest_card)
					   | (_,_) => first_card::ace_to_1 (rest_card)
	fun iter_card_list (card_list, acc) =
	    case card_list of
		[] => acc
	      | first_c::rest_c => iter_card_list (rest_c, acc + card_value first_c)
    in
	iter_card_list(ace_to_1 (cs), 0)
    end

(* another way, same result *) 
(*fun score_challenge (cs, goal) = 
    let fun ace_to_1 (card_list) =
	    case card_list of
		[] => card_list
	      | first_card::rest_card => case first_card of
					     (Clubs, Ace) => (Clubs, Num 1)::ace_to_1 (rest_card) 
					   | (Diamonds, Ace) => (Diamonds, Num 1)::ace_to_1 (rest_card)
					   | (Hearts, Ace) => (Hearts, Num 1)::ace_to_1 (rest_card)
					   | (Spades, Ace) => (Spades, Num 1)::ace_to_1 (rest_card)
					   | (_,_) => first_card::ace_to_1 (rest_card)    
    in
	if score (ace_to_1 (cs), goal) < score (cs, goal)
	then score (ace_to_1 (cs), goal)
	else score (cs, goal)
    end
*)

fun score_challenge (cs, goal) = 
    let fun preliminary_score (score, goal) =
	    case score > goal of
		true => 3 * (score - goal)
	      | false => goal - score
				    
	fun final_score (prelim_score, color_match) =
	    case color_match of
		true => prelim_score div 2
	      | false => prelim_score
    in
	if final_score (preliminary_score (sum_cards_ace_to_1 (cs), goal), all_same_color (cs)) < final_score (preliminary_score (sum_cards (cs), goal), all_same_color (cs))
	then final_score (preliminary_score (sum_cards_ace_to_1 (cs), goal), all_same_color (cs))
	else final_score (preliminary_score (sum_cards (cs), goal), all_same_color (cs))
    end

(*points taken off here - Your function returns an incorrect result when the game should end due to an empty move list with low score*)
fun officiate_challenge (cs, move, goal) =
    let fun turn (deck, mv, held) = 
	    case (deck, mv, held) of  
		(_, [], held) => score_challenge (held, goal)
	      | ([], Draw::rest_mv, held) => score_challenge (held, goal)
	      | (deck, Discard c::rest_mv, held) => turn (deck, rest_mv, remove_card (held, c, IllegalMove))
	      | (top_c::rest_c, Draw::rest_mv, held) => if (sum_cards (top_c::held) > goal) andalso (sum_cards_ace_to_1 (top_c::held) > goal)
							then score_challenge (top_c::held, goal)
							else turn (rest_c, rest_mv, top_c::held)
    in
	turn (cs, move, [])
    end

fun contain_val (cs, card_val) = 
    case cs of 
	[] => false
      | first_card::rest_card => if card_value first_card = card_val then true else contain_val (rest_card, card_val)

fun retrieve_card (cs, card_val, e) = 
    case cs of 
	[] => raise e
      | first_card::rest_card => if card_value first_card = card_val then first_card else retrieve_card (rest_card, card_val, e)

fun careful_player (cs, goal) = 
    let fun build_move (deck, held, moves_acc) = 
	    case deck of
		[] => moves_acc
	      | top_card::rest_card => if score (held, goal) = 0 
				       then moves_acc
				       else if (sum_cards (held) + 10) < goal 
				       then build_move (rest_card, top_card::held, moves_acc @ [Draw])
				       else if contain_val (held, (sum_cards (top_card::held) - goal)) = true
				       then build_move (rest_card, [top_card] @ (remove_card (held, (retrieve_card (held, (sum_cards (top_card::held) - goal), IllegalMove)), IllegalMove)), (moves_acc @ [Discard (retrieve_card (held, (sum_cards (top_card::held) - goal), IllegalMove))]) @ [Draw])
				       else moves_acc
    in
	build_move (cs ,[], [])
    end
	
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
val t_similar_names = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})
val t_card_color = card_color ((Clubs, Num 2))
val t_card_value = card_value ((Clubs, Num 2))
val t_remove_card = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove)
val t_all_same_color = all_same_color ([(Hearts, Ace), (Hearts, Ace)])
val t_sum_cards = sum_cards([(Clubs, Num 2),(Clubs, Num 2)])
val t_score = score ([(Hearts, Num 2),(Clubs, Num 4)],10)
val t_officiate = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw],15)
val t_officiate2 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],[Draw,Draw,Draw,Draw,Draw],42)
val t_officiate3 = officiate ([(Clubs,Jack),(Spades,Num(8))],[Draw,Discard(Clubs,Jack)],42)  (*should give an illegal move*)
val t_careful_player = careful_player ([(Diamonds,Ace),(Hearts,Ace),(Clubs,Ace),(Spades,Ace)], 33)
val t_careful_player2 = careful_player ([(Diamonds,Ace),(Hearts,Ace),(Clubs,Ace),(Spades,Ace)], 32)
val t_careful_player3 = careful_player ([(Diamonds,Num 3),(Hearts,Num 5),(Clubs,Num 9),(Spades,Ace)], 14)
val t_score_challenge = score_challenge ([(Hearts, Num 2),(Clubs, Num 4)],10)
val t_score_challenge2 = score_challenge ([(Hearts, Num 2),(Clubs, Ace)],10)
val t_score_challenge3 = score_challenge ([(Hearts, Num 2),(Hearts, Ace)],10)
val t_score_challenge4 = score_challenge ([(Hearts, Num 3),(Clubs, Ace)],13)
val t_officiate_challenge = officiate_challenge ([(Diamonds,Ace),(Hearts,Ace),(Clubs,Ace),(Spades,Ace)],[Draw,Draw,Draw,Draw,Draw],23)
val t_officiate_challenge2 = officiate_challenge ([(Diamonds,Ace),(Hearts,Num 3),(Clubs,Num 5),(Spades,Num 1)],[Draw,Draw,Draw,Draw,Draw],23)
val t_officiate_challenge3 = officiate_challenge ([(Diamonds,Ace),(Hearts,Ace),(Clubs,Num 4),(Spades,Num 2)],[Draw,Draw,Draw,Draw,Draw],23)
*)
