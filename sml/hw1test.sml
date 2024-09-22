use "hw1.sml";

val test_is_older1 = is_older ((1, 2, 3), (2, 4, 6)) = true
val test_is_older2 = is_older ((1, 2, 3), (1, 2, 3)) = false 
val test_is_older3 = is_older ((2, 4, 6), (1, 2, 3)) = false 
val test_is_older4 = is_older ((1, 2, 3), (1, 2, 4)) = true 

val test_number_is_month1 = number_in_month ([(1, 2, 3), (1, 3, 4)], 2) = 1
val test_number_is_month2 = number_in_month ([(1, 2, 3), (1, 2, 4)], 2) = 2
val test_number_is_month3 = number_in_month ([(1, 3, 3), (1, 4, 4)], 2) = 0
val test_number_is_month4 = number_in_month ([(1, 2, 3), (1, 2, 4), (3, 3, 5),
(2012, 2, 3), (2013, 6, 4)], 2) = 3
val test_number_is_month5 = number_in_month ([], 2) = 0

val test_nims1 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test_nims2 = number_in_months ([(1, 2, 3), (1, 2, 4), (3, 3, 5),
(2012, 2, 3), (2013, 6, 4)], [2, 3, 6]) = 5

val test_dim1 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test_dim2 = dates_in_month ([(2012,2,28),(2013,12,1), (2013, 2, 31), (2018,
2, 300)],2) = [(2012,2,28), (2013, 2, 31), (2018, 2, 300)]
val test_dim3 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test_dims1 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test_dims2 = dates_in_months ([(2012,1,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2011,3,31),(2011,4,28)]
val test_dims3 = dates_in_months ([],[]) = []
val test_dims4 = dates_in_months ([(1, 2, 3), (1, 3, 4), (1, 222, 3)],[222]) =
  [(1, 222, 3)]

val test_getnth1 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test_getnth2 = get_nth (["hi", "there", "how", "are", "you"], 4) = "are"
val test_getnth3 = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"

val test_nbrs1 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test_nbrs2 = number_before_reaching_sum (10, [5,5,3,4,5]) = 1
val test_nbrs3 = number_before_reaching_sum (10, [10,2,3,4,5]) = 0
val test_nbrs4 = number_before_reaching_sum (10, [6,2,3,4,5]) = 2

val test_wm1 = what_month 70 = 3
val test_wm2 = what_month 1 = 1
val test_wm3 = what_month 10 = 1 
val test_wm4 = what_month 32 = 2
val test_wm5 = what_month 31 = 1
val test_wm6 = what_month 71 = 3
val test_wm7 = what_month 72 = 3

val test_mr1 = month_range (31, 34) = [1,2,2,2]
val test_mr2 = month_range (29, 34) = [1,1,1,2,2,2]
val test_mr3 = month_range (28, 34) = [1,1,1,1,2,2,2]

val test_oldest1 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test_oldest2 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)])
val test_oldest3 = oldest([(2012,2,28),(2011,3,31),(2011,4,28), (2013, 5, 27)])

val test_nims1c = number_in_months_challenge
([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,3,3,3,3,3,3,4])
val test_nims2c = number_in_months_challenge ([(1, 2, 3), (1, 2, 4), (3, 3, 5),
(2012, 2, 3), (2013, 6, 4)], [2, 3, 3, 3, 3, 3, 6])

val test_dims1c =
  dates_in_months_challenge([(2012,4,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,3,3,3,4])
val test_dims2c = dates_in_months ([(2012,1,28),(2013,12,1),(2011,4,31),(2011,4,28)],[2,3,4]) = [(2011,4,31),(2011,4,28)]

val test_reverse = reverse([1, 2, 3, 4, 5, 6])

val test_dedup = deduplicate([1, 1, 1, 1, 2, 2, 2, 2, 2])

val test_dts1 = date_to_string (2013, 6, 1) = "June 1, 2013"

fun tests xs = 
  if null xs
  then true
  else (hd xs = true andalso tests(tl xs))

val tests_is_older = 
  tests([test_is_older1, test_is_older2, test_is_older3, test_is_older4])

val test_rd1 = reasonable_date((100, 0, 27))

