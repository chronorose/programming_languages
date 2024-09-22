fun is_older (x1: int*int*int, x2: int*int*int) = 
  if (#1 x1 < #1 x2)
  then true
  else if (#1 x1 = #1 x2) andalso (#2 x1 < #2 x2)
  then true
  else if (#1 x1 = #1 x2) andalso (#2 x1 = #2 x2) andalso (#3 x1 < #3 x2)
  then true
  else false
  

fun number_in_month (dates: (int*int*int) list, month: int) =
  let 
    fun cmp_helper (date: (int*int*int)) = 
      (#2 date) = month
    fun iter_helper (dates: (int*int*int) list) =
      if null dates
      then 0
      else 
        let val recursion_val = iter_helper(tl dates)
        in
          if (cmp_helper(hd dates)) 
          then 1 + recursion_val 
          else recursion_val 
        end
  in
    iter_helper(dates)
  end

fun number_in_months (dates: (int*int*int) list, months: int list) = 
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int*int*int) list, month: int) = 
  let
    fun cmp_helper (date: (int*int*int)) =
      #2 date = month
  in
    if null dates
    then []
    else
      let 
        val recursion_val = dates_in_month(tl dates, month)
      in
        if cmp_helper (hd dates)
        then (hd dates) :: recursion_val
        else recursion_val
      end
  end

fun dates_in_months(dates: (int*int*int) list, months: int list) = 
  if null months
  then [] 
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (strings: string list, pos: int) = 
  if pos = 1
  then hd strings
  else get_nth(tl strings, pos - 1)

fun date_to_string (date: int*int*int) =
  let 
    val months = ["January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"]
  in
    get_nth (months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^
    Int.toString(#1 date)
  end

fun number_before_reaching_sum (sum: int, ints: int list) =
  let 
    fun helper (cur_i: int, cur_sum: int, cur_ints: int list) = 
      if cur_sum + (hd cur_ints) >= sum
      then cur_i
      else helper (cur_i + 1, cur_sum + (hd cur_ints), tl cur_ints)
  in
    helper(0, 0, ints)
  end

fun what_month (day: int) =
  let
    val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum (day, days_in_month) + 1
  end

fun month_range (day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates: (int*int*int) list) = 
  if null dates
  then NONE
  else
    let 
      fun oldest_helper(cur_dates: (int*int*int) list, cur_oldest: int*int*int) =
        if null cur_dates
        then SOME cur_oldest
        else 
          if is_older((hd cur_dates), cur_oldest)
          then oldest_helper(tl cur_dates, hd cur_dates)
          else oldest_helper(tl cur_dates, cur_oldest)
    in
      oldest_helper(dates, hd dates)
    end

fun deduplicate(xs: int list) = 
  let 
    fun was (xs: int list, elem: int) =
      if null xs
      then false
      else if hd xs = elem
      then true
      else was(tl xs, elem)
    fun deduplicate_helper(cur_xs: int list, cur_new: int list) =
      if null cur_xs
      then cur_new
      else if was(cur_new, hd cur_xs)
      then deduplicate_helper(tl cur_xs, cur_new)
      else deduplicate_helper(tl cur_xs, hd cur_xs :: cur_new)
  in
    deduplicate_helper(xs, [])
  end

fun number_in_months_challenge (dates: (int*int*int) list, months: int list) = 
  let 
    val new_months = deduplicate(months)
  in
    number_in_months(dates, new_months)
  end

fun dates_in_months_challenge (dates: (int*int*int) list, months: int list) =
  let val new_months = reverse (deduplicate (months))
  in
    dates_in_months(dates, new_months)
  end

fun get_nth_int (xs: int list, pos: int) = 
  if pos = 1
  then hd xs 
  else get_nth_int(tl xs, pos - 1)

fun reasonable_date(date: (int*int*int)) =
  let
    val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    val days_in_month_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    fun leap_year (date: (int*int*int)) =
      if ((#1 date) mod 400 = 0) orelse (#1 date mod 4 = 0 andalso #1 date mod
      100 <> 0)
      then true
      else false
    val is_month = 
    #2 date <= 12 andalso #2 date >= 0
    val days_in_actual_month =
      if not is_month 
      then 0
      else if leap_year(date)
      then get_nth_int (days_in_month_leap, #2 date)
      else get_nth_int (days_in_month, #2 date)
  in
    if (#1 date <= 0) 
    then false
    else if #3 date > days_in_actual_month
    then false
    else is_month
  end
