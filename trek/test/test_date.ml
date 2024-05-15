open Backend
open Date
open OUnit2

let test_create _ =
  let date = create 2024 April 27 in
  assert_equal 2024 (get_year date) ~msg:"Test create: year";
  (match get_month date with
  | April -> ()
  | _ -> assert_failure "Test create: month");
  assert_equal 27 (get_day date) ~msg:"Test create: day"

let test_current_date _ =
  let current = current_date () in
  let now = Unix.localtime (Unix.time ()) in
  assert_equal (now.tm_year + 1900) (get_year current)
    ~msg:"Test current_date: year";
  let current_month =
    match get_month current with
    | January -> 1
    | February -> 2
    | March -> 3
    | April -> 4
    | May -> 5
    | June -> 6
    | July -> 7
    | August -> 8
    | September -> 9
    | October -> 10
    | November -> 11
    | December -> 12
  in
  assert_equal (now.tm_mon + 1) current_month ~msg:"Test current_date: month";
  assert_equal now.tm_mday (get_day current) ~msg:"Test current_date: day"

let test_parse_date _ =
  let date = parse_date "2024-04-27" in
  assert_equal 2024 (get_year date) ~msg:"Test parse_date: year";
  (match get_month date with
  | April -> ()
  | _ -> assert_failure "Test parse_date: month");
  assert_equal 27 (get_day date) ~msg:"Test parse_date: day"

let test_days_in_month _ =
  assert_equal 31
    (days_in_month 2024 January)
    ~msg:"Test days_in_month: January";
  assert_equal 29
    (days_in_month 2024 February)
    ~msg:"Test days_in_month: February (leap year)";
  assert_equal 28
    (days_in_month 2023 February)
    ~msg:"Test days_in_month: February (non-leap year)";
  assert_equal 31 (days_in_month 2024 March) ~msg:"Test days_in_month: March";
  assert_equal 30 (days_in_month 2024 April) ~msg:"Test days_in_month: April";
  assert_equal 31 (days_in_month 2024 May) ~msg:"Test days_in_month: May";
  assert_equal 30 (days_in_month 2024 June) ~msg:"Test days_in_month: June";
  assert_equal 31 (days_in_month 2024 July) ~msg:"Test days_in_month: July";
  assert_equal 31 (days_in_month 2024 August) ~msg:"Test days_in_month: August";
  assert_equal 30
    (days_in_month 2024 September)
    ~msg:"Test days_in_month: September";
  assert_equal 31
    (days_in_month 2024 October)
    ~msg:"Test days_in_month: October";
  assert_equal 30
    (days_in_month 2024 November)
    ~msg:"Test days_in_month: November";
  assert_equal 31
    (days_in_month 2024 December)
    ~msg:"Test days_in_month: December"

let test_day_of_week _ =
  let date = create 2024 April 26 in
  assert_equal 5
    (match day_of_week date with
    | Friday -> 5
    | _ -> -1)
    ~msg:"Test day_of_week: Friday";
  let date = create 2024 April 27 in
  assert_equal 6
    (match day_of_week date with
    | Saturday -> 6
    | _ -> -1)
    ~msg:"Test day_of_week: Saturday";
  let date = create 2024 April 29 in
  assert_equal 1
    (match day_of_week date with
    | Monday -> 1
    | _ -> -1)
    ~msg:"Test day_of_week: Monday";
  let date = create 2024 May 1 in
  assert_equal 3
    (match day_of_week date with
    | Wednesday -> 3
    | _ -> -1)
    ~msg:"Test day_of_week: Wednesday"

let test_last_day _ =
  let last_day_april_2024 = last_day April 2024 in
  assert_equal 2024 (get_year last_day_april_2024) ~msg:"Test last_day: year";
  (match get_month last_day_april_2024 with
  | April -> ()
  | _ -> assert_failure "Test last_day: month");
  assert_equal 30 (get_day last_day_april_2024) ~msg:"Test last_day: day"

let test_next_month _ =
  let next_m, next_y = next_month April 2024 in
  assert_equal 5
    (match next_m with
    | May -> 5
    | _ -> -1)
    ~msg:"Test next_month: month";
  assert_equal 2024 next_y ~msg:"Test next_month: year";
  let next_m, next_y = next_month December 2024 in
  assert_equal 1
    (match next_m with
    | January -> 1
    | _ -> -1)
    ~msg:"Test next_month: month (year change)";
  assert_equal 2025 next_y ~msg:"Test next_month: year (year change)"

let test_prev_month _ =
  let prev_m, prev_y = prev_month April 2024 in
  assert_equal 3
    (match prev_m with
    | March -> 3
    | _ -> -1)
    ~msg:"Test prev_month: month";
  assert_equal 2024 prev_y ~msg:"Test prev_month: year";
  let prev_m, prev_y = prev_month January 2024 in
  assert_equal 12
    (match prev_m with
    | December -> 12
    | _ -> -1)
    ~msg:"Test prev_month: month (year change)";
  assert_equal 2023 prev_y ~msg:"Test prev_month: year (year change)"

let test_next_day _ =
  let next_day_april_27_2024 = next_day (create 2024 April 27) in
  assert_equal 2024 (get_year next_day_april_27_2024) ~msg:"Test next_day: year";
  (match get_month next_day_april_27_2024 with
  | April -> ()
  | _ -> assert_failure "Test next_day: month");
  assert_equal 28 (get_day next_day_april_27_2024) ~msg:"Test next_day: day";
  let next_day_dec_31_2023 = next_day (create 2023 December 31) in
  assert_equal 2024
    (get_year next_day_dec_31_2023)
    ~msg:"Test next_day: year (year change)";
  (match get_month next_day_dec_31_2023 with
  | January -> ()
  | _ -> assert_failure "Test next_day: month (year change)");
  assert_equal 1
    (get_day next_day_dec_31_2023)
    ~msg:"Test next_day: day (year change)"

let test_prev_day _ =
  let prev_day_april_27_2024 = prev_day (create 2024 April 27) in
  assert_equal 2024 (get_year prev_day_april_27_2024) ~msg:"Test prev_day: year";
  (match get_month prev_day_april_27_2024 with
  | April -> ()
  | _ -> assert_failure "Test prev_day: month");
  assert_equal 26 (get_day prev_day_april_27_2024) ~msg:"Test prev_day: day";
  let prev_day_jan_1_2024 = prev_day (create 2024 January 1) in
  assert_equal 2023
    (get_year prev_day_jan_1_2024)
    ~msg:"Test prev_day: year (year change)";
  (match get_month prev_day_jan_1_2024 with
  | December -> ()
  | _ -> assert_failure "Test prev_day: month (year change)");
  assert_equal 31
    (get_day prev_day_jan_1_2024)
    ~msg:"Test prev_day: day (year change)"

let test_format_date _ =
  let date = create 2024 April 27 in
  assert_equal "2024-04-27" (format_date date)
    ~msg:"Test format_date: normal date";
  let date = create 2023 February 28 in
  assert_equal "2023-02-28" (format_date date)
    ~msg:"Test format_date: February in non-leap year";
  let date = create 2024 February 29 in
  assert_equal "2024-02-29" (format_date date)
    ~msg:"Test format_date: February in leap year";
  let date = create 2024 December 1 in
  assert_equal "2024-12-01" (format_date date) ~msg:"Test format_date: December"

let test_is_weekend _ =
  let date = create 2024 April 27 in
  assert_equal true (is_weekend date) ~msg:"Test is_weekend: Saturday";
  let date = create 2024 April 28 in
  assert_equal true (is_weekend date) ~msg:"Test is_weekend: Sunday";
  let date = create 2024 April 26 in
  assert_equal false (is_weekend date) ~msg:"Test is_weekend: Friday";
  let date = create 2024 April 30 in
  assert_equal false (is_weekend date) ~msg:"Test is_weekend: Tuesday"

let test_is_leap_year _ =
  assert_equal true (is_leap_year 2024)
    ~msg:"Test is_leap_year: 2024 (leap year)";
  assert_equal false (is_leap_year 2023)
    ~msg:"Test is_leap_year: 2023 (non-leap year)";
  assert_equal false (is_leap_year 2100)
    ~msg:"Test is_leap_year: 2100 (non-leap year)";
  assert_equal true (is_leap_year 2000)
    ~msg:"Test is_leap_year: 2000 (leap year)"

let suite =
  "Date Tests"
  >::: [
         "test_create" >:: test_create;
         "test_current_date" >:: test_current_date;
         "test_parse_date" >:: test_parse_date;
         "test_days_in_month" >:: test_days_in_month;
         "test_day_of_week" >:: test_day_of_week;
         "test_last_day" >:: test_last_day;
         "test_next_month" >:: test_next_month;
         "test_prev_month" >:: test_prev_month;
         "test_next_day" >:: test_next_day;
         "test_prev_day" >:: test_prev_day;
         "test_format_date" >:: test_format_date;
         "test_is_weekend" >:: test_is_weekend;
         "test_is_leap_year" >:: test_is_leap_year;
       ]

let () =
  run_test_tt_main suite;
  print_endline "test_date passed all tests"
