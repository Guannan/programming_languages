(*Guannan Ren*)
(*Programming Languages - Homework 1*)
(*Instructor - Dan Grossman*)

fun is_older (date1 : int*int*int, date2 : int*int*int) =
    if (#1 date1) < (#1 date2) 
    then true
    else if (#1 date1) = (#1 date2) andalso (#2 date1) < (#2 date2)
    then true
    else if (#1 date1) = (#1 date2) andalso (#2 date1) = (#2 date2) andalso (#3 date1) < (#3 date2)
    then true
    else false

(*count the dates with matching month*)
fun number_in_month (dates : (int*int*int) list, month : int) = 
    if null dates
    then 0
    else 
	if (#2 (hd dates)) = month
	then 1+number_in_month(tl dates,month)
	else number_in_month(tl dates,month)

fun number_in_months (dates : (int*int*int) list, months : int list) = 
    if null months
    then 0
    else number_in_month (dates,hd months) + number_in_months (dates,tl months) (*dates matching a single month + recursive results for other months*)

(*list the dates with matching month*)
fun dates_in_month (dates : (int*int*int) list, month : int) = 
    if null dates
    then []
    else
	if (#2 (hd dates)) = month
	then hd(dates) :: dates_in_month(tl dates,month)
	else dates_in_month(tl dates,month)

fun dates_in_months (dates : (int*int*int) list, months : int list) = 
    if null months
    then []
    else dates_in_month (dates,hd months) @ dates_in_months (dates,tl months) (*dates matching a single month + recursive results for other months *)

fun get_nth (strings : string list, index : int) = 
    if index = 1
    then hd strings
    else get_nth (tl strings, index - 1)

fun date_to_string (date : int*int*int) = 
    let val month_conv = ["January","February","March","April","May","June","July","August","September","October","November","December"]  
    in
	get_nth(month_conv,(#2 date))^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, num_list : int list) = 
    if sum <= hd num_list 
    then 0
    else 1+number_before_reaching_sum(sum-hd(num_list),tl num_list)

fun what_month (day_of_year : int) = 
    let val day_conv = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	1+number_before_reaching_sum (day_of_year, day_conv)
    end

fun month_range (day1 : int, day2 : int) = 
    if day1 > day2
    then []
    else what_month(day1) :: month_range (day1+1, day2)

fun oldest (dates : (int*int*int) list) = 
    if null dates
    then NONE
    else let
	fun check_older(dates : (int*int*int) list) = 
	    if null (tl dates)
	    then hd dates
	    else let val tl_ans = check_older (tl dates)
		 in
		     if is_older (hd dates, tl_ans) = true
		     then hd dates
		     else tl_ans
		 end
    in
	SOME (check_older dates)
    end


fun reasonable_date (date : int*int*int) = 
    let val days_in_month = ["31","28","31","30","31","30","31","31","30","31","30","31"]
	val leap_days_in_month = ["31","29","31","30","31","30","31","31","30","31","30","31"]
    in
	if (#1 date > 0) andalso (#2 date >= 1) andalso (#2 date <= 12)  (*check for valid year/month*)
	then
	    (*check leap year*)
	    if (#1 date mod 400 = 0) orelse ((#1 date mod 4 = 0) andalso (#1 date mod 100 <> 0))
	    then (*leap year*)
		if (#3 date > 0) andalso (#3 date <= valOf (Int.fromString(get_nth(leap_days_in_month, (#2 date)))))  (*check for valid day of month*)
		then true
		else false
	    else (*non-leap year*)
		if (#3 date > 0) andalso (#3 date <= valOf (Int.fromString(get_nth(days_in_month, (#2 date)))))  (*check for valid day of month*)
		then true
		else false
	else false
    end

fun get_last (items : int list) = 
    if null items
    then []
    else if null (tl items)
    then [hd items]
    else get_last (tl items)

fun remove_duplicates (items : int list) = 
    if null items
    then []
    else if null (tl items)
    then [hd items]
    else
	let val tl_items = tl items
	in
	    if hd items = hd(tl_items)
	    then remove_duplicates (tl_items)   (*found same element, keep checking with the same element*)	    
	    else hd(tl_items) :: remove_duplicates (hd (items) :: tl (tl_items))
	end

fun remove_duplicates_main (items : int list) = 
    if null items
    then []
    else
	let val marker = hd items  (*place marker to tell when to stop calling remove dup*)
	    fun loop_del_dup (input_list : int list, marker : int) = 
		if null input_list
		then []
		else if marker = hd (input_list)
		then input_list
		else loop_del_dup (remove_duplicates(input_list),marker)
	in
	    loop_del_dup (remove_duplicates(items),marker)
	end

fun number_in_months_challenge (dates : (int*int*int) list, months : int list) = 
    if null months
    then 0
    else
	let val months_no_dup = remove_duplicates_main (months)
	in
	    number_in_months (dates, months_no_dup)
	end

fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) = 
    if null months
    then []
    else
	let val months_no_dup = remove_duplicates_main (months)
	in
	    dates_in_months (dates, months_no_dup)
	end

(*
val t_is_older = is_older ((1,2,3),(2,3,4))
val t_number_in_month = number_in_month ([(2012,2,28),(2013,12,1)],2)
val t_number_in_months = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4])
val t_dates_in_month = dates_in_month ([(2012,2,28),(2013,12,1)],2)
val t_dates_in_months = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4])
val t_get_nth = get_nth (["hi", "there", "how", "are", "you"], 2)
val t_date_to_string = date_to_string ((2013, 6, 1))
val t_number_before_reaching_sum = number_before_reaching_sum (15, [1,2,3,4,5])
val t_what_month = what_month (121)
val t_month_range = month_range (31,34)
val t_oldest = oldest ([(2012,2,28),(2011,3,31),(2011,4,28)])
val t_oldest2 = oldest ([(10,10,13),(11,2,10),(13,1,1)])
val t_reasonable_date = reasonable_date((1,1,1))
val t_reasonable_date2 = reasonable_date((2000,2,28))
val t_reasonable_date3 = reasonable_date((2000,2,29))
val t_reasonable_date4 = reasonable_date((2004,2,29))
val t_reasonable_date = reasonable_date((2100,2,29))
val dup_removed = remove_duplicates_main ([1,1,2,2,3,4,5])
val dup_remove2 = remove_duplicates_main ([2,3,5,2,2,1,3])
val t_get_last = get_last ([1,2,3,4,5])
val t_number_in_months_challenge = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,2,4,4,3])
val t_dates_in_months_challenge = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[1,2,1,3,4,9,3,5,3,3,2])
*)