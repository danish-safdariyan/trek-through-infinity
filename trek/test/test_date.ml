open Trek
open Date
include Date

let run_tests () =
  let assert_equal expected actual message =
    if expected = actual then Printf.printf "[PASS] %s\n" message
    else
      Printf.printf "[FAIL] %s - Expected: %s, Actual: %s\n" message
        (match expected with
        | Date.January -> "January"
        | Date.February -> "February"
        | Date.March -> "March"
        | Date.April -> "April"
        | Date.May -> "May"
        | Date.June -> "June"
        | Date.July -> "July"
        | Date.August -> "August"
        | Date.September -> "September"
        | Date.October -> "October"
        | Date.November -> "November"
        | Date.December -> "December")
        (match actual with
        | Date.January -> "January"
        | Date.February -> "February"
        | Date.March -> "March"
        | Date.April -> "April"
        | Date.May -> "May"
        | Date.June -> "June"
        | Date.July -> "July"
        | Date.August -> "August"
        | Date.September -> "September"
        | Date.October -> "October"
        | Date.November -> "November"
        | Date.December -> "December")
  in

  (* Test create function *)
  let test_create () =
    let date = Date.create 2024 Date.April 23 in
    assert_equal 2024 (Date.get_year date) "Create function - Year";
    assert_equal Date.April (Date.get_month date) "Create function - Month";
    assert_equal 23 (Date.get_day date) "Create function - Day"
  in

  (* Test current_date function *)
  let test_current_date () =
    let current_date = Date.current_date () in
    let now = Unix.localtime (Unix.time ()) in
    assert_equal (now.tm_year + 1900) (Date.current_date ())
      "Current date function - Year";
    assert_equal
      (match now.tm_mon with
      | 0 -> Date.January
      | 1 -> Date.February
      | 2 -> Date.March
      | 3 -> Date.April
      | 4 -> Date.May
      | 5 -> Date.June
      | 6 -> Date.July
      | 7 -> Date.August
      | 8 -> Date.September
      | 9 -> Date.October
      | 10 -> Date.November
      | _ -> Date.December)
      current_date.month "Current date function - Month";
    assert_equal now.tm_mday current_date.day "Current date function - Day"
  in

  (* Test parse_date function *)
  let test_parse_date () =
    let date = Date.parse_date "2024-04-23" in
    assert_equal 2024 date.year "Parse date function - Year";
    assert_equal Date.April date.month "Parse date function - Month";
    assert_equal 23 date.day "Parse date function - Day"
  in

  (* Test days_in_month function *)
  let test_days_in_month () =
    assert_equal 31
      (Date.days_in_month 2024 Date.January)
      "Days in month function - January";
    assert_equal 29
      (Date.days_in_month 2020 Date.February)
      "Days in month function - February (Leap year)";
    assert_equal 28
      (Date.days_in_month 2021 Date.February)
      "Days in month function - February (Non-leap year)";
    assert_equal 30
      (Date.days_in_month 2024 Date.April)
      "Days in month function - April";
    assert_equal 31
      (Date.days_in_month 2024 Date.December)
      "Days in month function - December"
  in

  (* Test day_of_week function *)
  let test_day_of_week () =
    let date = Date.create 2024 Date.April 23 in
    assert_equal Date.Sunday (Date.day_of_week date)
      "Day of week function - April 23, 2024"
  in

  (* Run all test cases *)
  test_create ();
  test_current_date ();
  test_parse_date ();
  test_days_in_month ();
  test_day_of_week ()

(* Run the test suite *)
let () = run_tests ()
