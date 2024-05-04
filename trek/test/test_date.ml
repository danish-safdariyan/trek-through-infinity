open Backend
open Date

let failed_tests = ref []

let assert_equal expected actual message =
  if expected = actual then Printf.printf "Test passed: %s\n" message
  else begin
    Printf.printf "Test failed: %s\nExpected: %s\nActual: %s\n" message
      (string_of_int expected) (string_of_int actual);
    failed_tests := message :: !failed_tests
  end

let assert_equal_string expected actual message =
  if expected = actual then Printf.printf "Test passed: %s\n" message
  else begin
    Printf.printf "Test failed: %s\nExpected: %s\nActual: %s\n" message expected
      actual;
    failed_tests := message :: !failed_tests
  end

let assert_equal_bool expected actual message =
  if expected = actual then Printf.printf "Test passed: %s\n" message
  else begin
    Printf.printf "Test failed: %s\nExpected: %b\nActual: %b\n" message expected
      actual;
    failed_tests := message :: !failed_tests
  end

let test_create () =
  let date = create 2024 April 27 in
  assert_equal 2024 (get_year date) "Test create: year";
  (match get_month date with
  | April -> Printf.printf "Test create: month passed\n"
  | _ -> Printf.printf "Test create: month failed\n");
  assert_equal 27 (get_day date) "Test create: day"

let test_current_date () =
  let current = current_date () in
  let now = Unix.localtime (Unix.time ()) in
  assert_equal (now.tm_year + 1900) (get_year current) "Test current_date: year";
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
  assert_equal (now.tm_mon + 1) current_month "Test current_date: month";
  assert_equal now.tm_mday (get_day current) "Test current_date: day"

let test_parse_date () =
  let date = parse_date "2024-04-27" in
  assert_equal 2024 (get_year date) "Test parse_date: year";
  (match get_month date with
  | April -> Printf.printf "Test parse_date: month passed\n"
  | _ -> Printf.printf "Test parse_date: month failed\n");
  assert_equal 27 (get_day date) "Test parse_date: day"

let test_days_in_month () =
  assert_equal 31 (days_in_month 2024 January) "Test days_in_month: January";
  assert_equal 29
    (days_in_month 2024 February)
    "Test days_in_month: February (leap year)";
  assert_equal 28
    (days_in_month 2023 February)
    "Test days_in_month: February (non-leap year)";
  assert_equal 31 (days_in_month 2024 March) "Test days_in_month: March";
  assert_equal 30 (days_in_month 2024 April) "Test days_in_month: April";
  assert_equal 31 (days_in_month 2024 May) "Test days_in_month: May";
  assert_equal 30 (days_in_month 2024 June) "Test days_in_month: June";
  assert_equal 31 (days_in_month 2024 July) "Test days_in_month: July";
  assert_equal 31 (days_in_month 2024 August) "Test days_in_month: August";
  assert_equal 30 (days_in_month 2024 September) "Test days_in_month: September";
  assert_equal 31 (days_in_month 2024 October) "Test days_in_month: October";
  assert_equal 30 (days_in_month 2024 November) "Test days_in_month: November";
  assert_equal 31 (days_in_month 2024 December) "Test days_in_month: December"

let test_day_of_week () =
  let date = create 2024 April 26 in
  assert_equal 5
    (match day_of_week date with
    | Friday -> 5
    | _ -> -1)
    "Test day_of_week: Friday";
  let date = create 2024 April 27 in
  assert_equal 6
    (match day_of_week date with
    | Saturday -> 6
    | _ -> -1)
    "Test day_of_week: Saturday";
  let date = create 2024 April 29 in
  assert_equal 1
    (match day_of_week date with
    | Monday -> 1
    | _ -> -1)
    "Test day_of_week: Sunday";
  let date = create 2024 May 1 in
  assert_equal 3
    (match day_of_week date with
    | Wednesday -> 3
    | _ -> -1)
    "Test day_of_week: Tuesday"

let test_last_day () =
  let last_day_april_2024 = last_day April 2024 in
  assert_equal 2024 (get_year last_day_april_2024) "Test last_day: year";
  (match get_month last_day_april_2024 with
  | April -> Printf.printf "Test last_day: month passed\n"
  | _ -> Printf.printf "Test last_day: month failed\n");
  assert_equal 30 (get_day last_day_april_2024) "Test last_day: day"

let test_next_month () =
  let next_m, next_y = next_month April 2024 in
  assert_equal 5
    (match next_m with
    | May -> 5
    | _ -> -1)
    "Test next_month: month";
  assert_equal 2024 next_y "Test next_month: year";
  let next_m, next_y = next_month December 2024 in
  assert_equal 1
    (match next_m with
    | January -> 1
    | _ -> -1)
    "Test next_month: month (year change)";
  assert_equal 2025 next_y "Test next_month: year (year change)"

let test_prev_month () =
  let prev_m, prev_y = prev_month April 2024 in
  assert_equal 3
    (match prev_m with
    | March -> 3
    | _ -> -1)
    "Test prev_month: month";
  assert_equal 2024 prev_y "Test prev_month: year";
  let prev_m, prev_y = prev_month January 2024 in
  assert_equal 12
    (match prev_m with
    | December -> 12
    | _ -> -1)
    "Test prev_month: month (year change)";
  assert_equal 2023 prev_y "Test prev_month: year (year change)"

let test_next_day () =
  let next_day_april_27_2024 = next_day (create 2024 April 27) in
  assert_equal 2024 (get_year next_day_april_27_2024) "Test next_day: year";
  (match get_month next_day_april_27_2024 with
  | May -> Printf.printf "Test next_day: month passed\n"
  | _ -> Printf.printf "Test next_day: month failed\n");
  assert_equal 28 (get_day next_day_april_27_2024) "Test next_day: day";
  let next_day_dec_31_2023 = next_day (create 2023 December 31) in
  assert_equal 2024
    (get_year next_day_dec_31_2023)
    "Test next_day: year (year change)";
  (match get_month next_day_dec_31_2023 with
  | January -> Printf.printf "Test next_day: month (year change) passed\n"
  | _ -> Printf.printf "Test next_day: month (year change) failed\n");
  assert_equal 1
    (get_day next_day_dec_31_2023)
    "Test next_day: day (year change)"

let test_prev_day () =
  let prev_day_april_27_2024 = prev_day (create 2024 April 27) in
  assert_equal 2024 (get_year prev_day_april_27_2024) "Test prev_day: year";
  (match get_month prev_day_april_27_2024 with
  | April -> Printf.printf "Test prev_day: month passed\n"
  | _ -> Printf.printf "Test prev_day: month failed\n");
  assert_equal 26 (get_day prev_day_april_27_2024) "Test prev_day: day";
  let prev_day_jan_1_2024 = prev_day (create 2024 January 1) in
  assert_equal 2023
    (get_year prev_day_jan_1_2024)
    "Test prev_day: year (year change)";
  (match get_month prev_day_jan_1_2024 with
  | December -> Printf.printf "Test prev_day: month (year change) passed\n"
  | _ -> Printf.printf "Test prev_day: month (year change) failed\n");
  assert_equal 31
    (get_day prev_day_jan_1_2024)
    "Test prev_day: day (year change)"

let test_format_date () =
  let date = create 2024 April 27 in
  assert_equal_string "2024-04-27" (format_date date)
    "Test format_date: normal date";
  let date = create 2023 February 28 in
  assert_equal_string "2023-02-28" (format_date date)
    "Test format_date: February in non-leap year";
  let date = create 2024 February 29 in
  assert_equal_string "2024-02-29" (format_date date)
    "Test format_date: February in leap year";
  let date = create 2024 December 1 in
  assert_equal_string "2024-12-01" (format_date date)
    "Test format_date: December"

let test_is_weekend () =
  let date = create 2024 April 27 in
  assert_equal_bool true (is_weekend date) "Test is_weekend: Saturday";
  let date = create 2024 April 28 in
  assert_equal_bool true (is_weekend date) "Test is_weekend: Sunday";
  let date = create 2024 April 26 in
  assert_equal_bool false (is_weekend date) "Test is_weekend: Friday";
  let date = create 2024 April 30 in
  assert_equal_bool false (is_weekend date) "Test is_weekend: Wednesday"

let test_is_leap_year () =
  assert_equal_bool true (is_leap_year 2024)
    "Test is_leap_year: 2024 (leap year)";
  assert_equal_bool false (is_leap_year 2023)
    "Test is_leap_year: 2023 (non-leap year)";
  assert_equal_bool false (is_leap_year 2100)
    "Test is_leap_year: 2100 (non-leap year)";
  assert_equal_bool true (is_leap_year 2000)
    "Test is_leap_year: 2000 (leap year)"

let () =
  test_create ();
  test_current_date ();
  test_parse_date ();
  test_days_in_month ();
  test_day_of_week ();
  test_last_day ();
  test_next_month ();
  test_prev_month ();
  test_next_day ();
  test_prev_day ();
  test_format_date ();
  test_is_weekend ();
  test_is_leap_year ();

  if List.length !failed_tests > 0 then begin
    Printf.printf "\n%d tests failed:\n" (List.length !failed_tests);
    List.iter (fun msg -> Printf.printf " - %s\n" msg) !failed_tests
  end
  else print_endline "\nAll tests passed!"
