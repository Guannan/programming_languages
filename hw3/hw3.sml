(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** you can put all your code here ****)

fun only_capitals (strings : string list) = (*can just write 'strings' without typing*)
    List.filter (fn str => (Char.isUpper o String.sub) (str,0)) strings  (*same as, List.filter (fn str => Char.isUpper (String.sub (str, 0))) strings *)

fun longest_string1 strings = 
    List.foldl (fn (cur_str, acc) => if (String.size cur_str > String.size acc) then cur_str else acc) "" strings

fun longest_string2 strings = 
    List.foldl (fn (cur_str, acc) => if (String.size cur_str >= String.size acc) then cur_str else acc) "" strings

(*(int * int -> bool) -> string list -> string*)
fun longest_string_helper f strings =
    foldl (fn (cur_str, acc) => if f (String.size cur_str, String.size acc) then cur_str else acc) "" strings

(*string list -> string*)
val longest_string3 = longest_string_helper (fn (cur_str_size, acc_size) => cur_str_size > acc_size)
val longest_string4 = longest_string_helper (fn (cur_str_size, acc_size) => cur_str_size >= acc_size)

(* different way, although different type def, ('a * string -> string) -> 'a list -> string*)
(*fun longest_string_helper f strings =
    foldl f "" strings
*)
(*string list -> string*)
(*val longest_string3 = longest_string_helper (fn (cur_str, acc) => if (String.size cur_str > String.size acc) then cur_str else acc)
val longest_string4 = longest_string_helper (fn (cur_str, acc) => if (String.size cur_str >= String.size acc) then cur_str else acc)
*)

val longest_capitalized = longest_string1 o only_capitals
(*wrapped functions, val longest_capitalized = fn strings => (longest_string1 o only_capitals) strings *)

fun rev_string str = (String.implode o List.rev o String.explode) str

(*val-binding version*)
(*val rev_string = implode o rev o explode
*)

fun first_answer f input_list =
    case input_list of
        [] => raise NoAnswer
      | elem::rest_elem => case f elem of
			       NONE => first_answer f rest_elem
			     | SOME output_val => output_val

fun all_answers f input_list  =
    let fun accumulate_some f (cur_list, acc)  =
            case cur_list of
		[] => SOME acc
              | elem::rest_elem => case f elem of
                             SOME output_val => accumulate_some f (rest_elem,  acc @ output_val)
                           | NONE => NONE 
    in
	accumulate_some f (input_list, [])
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

fun count_wildcards pattern = 
    g (fn () => 1) (fn var_s => 0) pattern

fun count_wild_and_variable_lengths pattern =
    g (fn () => 1) (fn var_s => String.size var_s) pattern

fun count_some_var (str, pattern) = 
    g (fn () => 0) (fn var_s => if str = var_s then 1 else 0) pattern 

fun check_pat pattern = 
    let
        fun concat_variables pattern =
            case pattern of
                Variable s => [s]
              | TupleP ps => List.foldl (fn (cur_pattern, acc) => (concat_variables cur_pattern) @ acc) [] ps
	      | ConstructorP (_,p) => concat_variables p
	      | _ => []

	(* this may also work *)
	(*fun concat_variables pattern =
	    case pattern
             of Variable s => [s]
              | TupleP ps => List.concat (map concat_variables ps)
              | ConstructorP (_, p) => get_variables p
              | _ => []
	 *)

        fun check_var_dup var_list =
            case var_list of
                [] => true
              | cur_var::rest_var => if List.exists (fn iter_var => iter_var = cur_var) rest_var then false else check_var_dup rest_var
    in
        (check_var_dup o concat_variables) pattern
    end

fun match (valu, pattern) =
    case (valu,pattern) of
	(_, Wildcard) => SOME []
      | (_, Variable v) => SOME [(v, valu)]
      | (Unit, UnitP) => SOME []
      | (Const c, ConstP cp) => if (c = cp) then SOME [] else NONE 
      | (Tuple vs, TupleP ps) => if List.length (vs) = List.length (ps) then all_answers match (ListPair.zip (vs, ps)) else NONE
      | (Constructor (str1,v), ConstructorP (str2,p)) => if (str1 = str2) then match (v,p) else NONE
      |  _  => NONE

(* notice input type def of: valu -> pattern list , without () as in (valu * pattern list) , currying implied here*)
fun first_match v pattern_list =
    SOME (first_answer (fn p => match (v, p)) pattern_list)
    handle NoAnswer => NONE

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

fun typ_conversion pattern = 
    case pattern of
	Wildcard => Anything
      | TupleP ps => TupleT (List.map typ_conversion ps)
      | UnitP => UnitT
      | ConstP cp => IntT
      | Variable v => Datatype v
      | ConstructorP (_, p) => typ_conversion p

(*finding the most lenient type*)
fun typ_compare (typ1,typ2) = 
    case (typ1, typ2) of
	(TupleT typs1, TupleT typs2) => if List.length (typs1) = List.length (typs2) then TupleT (List.map typ_compare (ListPair.zip (typs1, typs2))) else Anything 
      | (TupleT typs, Anything) => TupleT typs
      | (Anything, TupleT typs) => TupleT typs
      | (UnitT, UnitT) => UnitT
      | (IntT, IntT) => IntT
      | (Datatype typ1_name, Datatype typ2_name) => if (typ1_name = typ2_name) then Datatype typ1_name else Anything
      | (_,_) => Anything

fun typecheck_patterns (constructor_list, pattern_list) =
    let fun find_lenient_typ f (cur_pattern_list, acc) =
            case cur_pattern_list of
		[] => SOME acc  (*acc = most_lenient_typ*)
              | cur_pattern::rest_pattern => find_lenient_typ f (rest_pattern, f (typ_conversion (cur_pattern), acc))
    in
	find_lenient_typ typ_compare (pattern_list, typ_conversion (hd pattern_list))
    end
	

(*
val t_only_capitals = only_capitals ["A","B","C"]
val t_only_capitals2 = only_capitals ["bobo", "Acorn", "sammy", "Cake"]
val t_longest_string1 = longest_string1 ["A","bc","C"]
val t_longest_string1_2 = longest_string1 ["A","bc","C","cc","dd"]
val t_longest_string1_3 = longest_string1 ["A","bc","C","ddd"]
val t_longest_string2 = longest_string2 ["A","bc","C"]
val t_longest_string2_2 = longest_string2 ["A","bc","C","cc","dd"]
val t_longest_string2_3 = longest_string2 ["A","bc","C","ddd"]
val t_longest_string2_4 = longest_string2 []
val t_longest_string3 = longest_string3 ["A","bc","C"]
val t_longest_string3_2 = longest_string3 ["A","bc","C","cc","dd"]
val t_longest_string3_3 = longest_string3 ["A","bc","C","ddd"]
val t_longest_string3_4 = longest_string3 []
val t_longest_string4 = longest_string4 ["A", "B", "C"]
val t_longest_string4_2 = longest_string4 ["A","bc","C","cc","dd"]
val t_longest_string4_3 = longest_string4 ["A","bc","C","ddd"]
val t_longest_string4_4 = longest_string4 []
val t_longest_capitalized = longest_capitalized ["A","bc","C"]
val t_longest_capitalized2 = longest_capitalized ["A","bc","C","DD"]
val t_longest_capitalized3 = longest_capitalized ["A","bc","C","EEE","FFF"]
val t_rev_string = rev_string "abc"
val t_first_answer = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5]
val t_all_answers = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7]
val t_all_answers2 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7,1]
val t_all_answers3 = all_answers (fn x => if x = 1 then SOME [x] else NONE) []
val t_all_answers4 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,2,3,4]
val t_all_answers5 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,1,1,1]
val t_count_wildcards = count_wildcards Wildcard
val t_count_wildcards2 = count_wildcards (Variable ("aaa"))
val t_count_wildcards3 = count_wildcards (TupleP [Variable "aaa", Wildcard, Variable "bbb", Wildcard])
val t_count_wildcards4 = count_wildcards (ConstructorP ("aaa", TupleP [Wildcard, Variable "bbb"]))
val t_count_wild_and_variable_lengths = count_wild_and_variable_lengths (Variable("a"))
val t_count_wild_and_variable_lengths2 = count_wild_and_variable_lengths (Variable("bbb"))
val t_count_wild_and_variable_lengths3 = count_wild_and_variable_lengths (Variable("ZZZsd"))
val t_count_wild_and_variable_lengths4 = count_wild_and_variable_lengths Wildcard
val t_count_wild_and_variable_lengths5 = count_wild_and_variable_lengths (ConstructorP("blah", TupleP[Wildcard, Variable("ZZZsd")]))
val t_count_wild_and_variable_lengths6 = count_wild_and_variable_lengths UnitP
val t_count_some_var = count_some_var ("x", Variable("x"))
val t_count_some_var2 = count_some_var ("x", Variable("wzzze"))
val t_count_some_var3 = count_some_var("x", Wildcard)
val t_count_some_var4 = count_some_var("x", UnitP)
val t_count_some_var5 = count_some_var("x", TupleP[Wildcard, Variable "x", Variable "x"])
val t_count_some_var6 = count_some_var("x", ConstructorP("x", TupleP[Wildcard, Variable "x", TupleP[Variable "x", Variable "woz"]]))
val t_check_pat = check_pat (Variable("x"))
val t_check_pat2 = check_pat (TupleP[Wildcard, Variable "x", ConstructorP("x", Variable "y")])
val t_check_pat3 = check_pat (TupleP[Wildcard, Variable "x", ConstructorP("y", Variable "x")])
val t_check_pat4 = check_pat (TupleP[Wildcard, Wildcard, UnitP, ConstP 11])
val t_match = match (Const(1), UnitP)
val t_match2 = match(Tuple [Const 1, Unit, Constructor ("boo", Const 19)], Wildcard)
val t_match3 = match (Const 88, ConstP 88)
val t_match4 = match (Unit, UnitP)
val t_match5 = match (Constructor ("c", Const 13), ConstructorP("c", Variable "b"));  (*SOME [("b", Const 13)]*)
val t_match6 = match (Tuple [Const 13, Const 17], TupleP [Variable "uno", Variable "dos"]); (*SOME [("uno", Const 13), ("dos", Const 17)]*)
val t_first_match = first_match Unit [UnitP]
val t_first_match2 = first_match (Const 1) [ConstP 1]
val t_first_match3 = first_match (Const 1) [ConstP 11]
val t_first_match4 = first_match (Const 22) [ConstP 2, Variable "x", Variable "w", ConstP 22]
*)

val t_typ_conversion = typ_conversion Wildcard
val t_typ_conversion2 = typ_conversion (Variable "dummy")
val t_typ_conversion3 = typ_conversion (TupleP [Wildcard, Variable "dummy", UnitP, ConstP 16])
val t_typ_conversion3 = typ_conversion (TupleP [Wildcard, TupleP [Wildcard, Wildcard]])
val t_typecheck_patterns = typecheck_patterns ([], [TupleP [Variable("x"), Variable("y")], TupleP [Wildcard, Wildcard]])
val t_typecheck_patterns2 = typecheck_patterns ([], [TupleP [Wildcard,Wildcard], TupleP[Wildcard,TupleP[Wildcard,Wildcard]]])

