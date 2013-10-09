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

val t_is_older = is_older ((1,2,3),(2,3,4))
val t_number_in_month = number_in_month ([(2012,2,28),(2013,12,1)],2)
val t_number_in_months = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4])
val t_dates_in_month = dates_in_month ([(2012,2,28),(2013,12,1)],2)
val t_dates_in_months = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4])
val t_get_nth = get_nth (["hi", "there", "how", "are", "you"], 2)


