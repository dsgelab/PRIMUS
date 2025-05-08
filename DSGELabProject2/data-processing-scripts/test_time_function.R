# This function is for determining whether the later date is later the same week than
# the earlier date or the next week. The function is used in upper_respiratory_summary.R
# to connect diagnosis events (earlier date) to prescription events (later date). This
# file contains test cases to make the function works properly (see unit testing).

later_this_week_or_next_week <- function(earlier_date, later_date) {
  return (
    later_date >= earlier_date & 
      as.numeric(difftime(later_date, earlier_date, units = "days")) <= 
      (7 - lubridate::wday(earlier_date, week_start = 1)) + 7
  )
}

assert <- function(test, expected) {
  if (test == expected) {
    print("PASS")
  } else {
    print(paste0("FAIL: result: ", test, ", expected: ", expected))
  }
}

assert(later_this_week_or_next_week(as.Date("2025-04-28"), as.Date("2025-04-29")), TRUE)
assert(later_this_week_or_next_week(as.Date("2025-04-28"), as.Date("2025-05-02")), TRUE)
assert(later_this_week_or_next_week(as.Date("2025-04-28"), as.Date("2025-05-04")), TRUE)
assert(later_this_week_or_next_week(as.Date("2025-04-28"), as.Date("2025-05-05")), TRUE)
assert(later_this_week_or_next_week(as.Date("2025-04-28"), as.Date("2025-05-08")), TRUE)
assert(later_this_week_or_next_week(as.Date("2025-04-28"), as.Date("2025-05-11")), TRUE)
assert(later_this_week_or_next_week(as.Date("2025-04-28"), as.Date("2025-05-12")), FALSE)
assert(later_this_week_or_next_week(as.Date("2025-04-28"), as.Date("2026-04-29")), FALSE)
assert(later_this_week_or_next_week(as.Date("2025-04-28"), as.Date("2025-04-27")), FALSE)

assert(later_this_week_or_next_week(as.Date("2024-12-29"), as.Date("2024-12-30")), TRUE)
assert(later_this_week_or_next_week(as.Date("2024-12-29"), as.Date("2025-01-01")), TRUE)
assert(later_this_week_or_next_week(as.Date("2024-12-29"), as.Date("2025-01-05")), TRUE)
assert(later_this_week_or_next_week(as.Date("2024-12-29"), as.Date("2025-12-29")), FALSE)
assert(later_this_week_or_next_week(as.Date("2024-12-29"), as.Date("2025-01-06")), FALSE)
assert(later_this_week_or_next_week(as.Date("2024-12-29"), as.Date("2024-12-27")), FALSE)
assert(later_this_week_or_next_week(as.Date("2024-12-29"), as.Date("2025-12-30")), FALSE)

