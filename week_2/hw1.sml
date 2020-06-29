(*1*)
(* first only check for year, then month, then day *)
fun is_older (d1: int*int*int, d2: int*int*int) =
	(#1 d1 < #1 d2) orelse
	(#1 d1 = #1 d2) andalso (#2 d1 < #2 d2) orelse
	(#1 d1 = #1 d2) andalso (#2 d1 = #2 d2) andalso (#3 d1 < #3 d2)

(*2*)
fun number_in_month (dates: (int*int*int) list, month: int) =
	if null dates (*base case*)
	then 0
	else
		if #2 (hd dates) = month
		then 1 + number_in_month(tl dates, month)
		else number_in_month(tl dates, month)

(*3*)
fun number_in_months (dates: (int*int*int) list, months: int list) =
	if null months
	then 0
	else
		number_in_month(dates, hd months) + number_in_months(dates, tl months)

(*4*)
fun dates_in_month (dates: (int*int*int) list, month: int) =
	if null dates
	then []
	else
		if #2 (hd dates) = month
		then (hd dates)::dates_in_month(tl dates, month)
		else dates_in_month(tl dates, month)

(*5*)
fun dates_in_months (dates: (int*int*int) list, months: int list) =
	if null months
	then []
	else
		dates_in_month(dates, hd months)@dates_in_months(dates, tl months)

(*6*) (*recursion is sexy*)
fun get_nth (strings: string list, n: int) =
	if n = 1
	then hd strings
	else get_nth(tl strings, n-1)

(*7*)
fun date_to_string (date: int*int*int) =
	let val months = ["January", "February", "March", "April", "May", "June", "July", "August",
		"September", "October", "November", "December"]
	in
		get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end

(* was thinking of using this helper function in 8 but found a better solution
fun sum_till_n (n: int, numList: int list) =
	if n = 1
	then hd numList
	else (hd numList) + sum_till_n(n-1, tl numList) *)

(*8*)
fun number_before_reaching_sum (sum: int, numbers: int list) =
	if sum <= (hd numbers)
	then 0
	else 1 + number_before_reaching_sum(sum - (hd numbers), tl numbers)

(*9*)
fun what_month (day: int) =
	let val mapping = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in number_before_reaching_sum(day, mapping) + 1
	end

(*10*)
fun month_range (day1: int, day2: int) = 
	if day1 > day2
	then []
	else what_month(day1)::month_range(day1+1, day2)

(*11 *)
(*same logic as max1 in lecture copied*)
fun oldest (dates: (int*int*int) list) =
	if null dates
	then NONE
	else 
		let val tl_ans = oldest(tl dates)
		in
			if isSome tl_ans andalso is_older(valOf tl_ans, hd dates)
			then tl_ans
			else SOME (hd dates)
		end

(*challenge ones will complete later*)
fun number_in_months_challenge() = null
fun dates_in_months_challenge() = null
fun reasonable_date() = null