test_that("dateparse_parse handles ISO format", {
  res <- dateparse_parse("2020-01-15")
  expect_equal(res$date, as.Date("2020-01-15"))
  expect_equal(res$datestr, "2020-01-15")
})

test_that("dateparse_parse handles compact YYYYMMDD", {
  res <- dateparse_parse("20200115")
  expect_equal(res$date, as.Date("2020-01-15"))
})

test_that("dateparse_parse handles European DD/MM/YYYY", {
  res <- dateparse_parse("15/01/2020")
  expect_equal(res$date, as.Date("2020-01-15"))
})

test_that("dateparse_parse handles Stata text format", {
  res <- dateparse_parse("15jan2020")
  expect_equal(res$date, as.Date("2020-01-15"))
})

test_that("dateparse_parse respects explicit format", {
  res <- dateparse_parse("01/15/2020", format = "MDY")
  expect_equal(res$date, as.Date("2020-01-15"))
})

test_that("dateparse_parse errors on empty string", {
  expect_error(dateparse_parse(""), "Empty date string")
})

test_that("dateparse_parse errors on unparseable", {
  expect_error(dateparse_parse("not-a-date"), "Could not parse date")
})

test_that("dateparse_window creates lookback window", {
  dt <- data.frame(id = 1:3, indexdate = as.Date("2020-06-15"))
  res <- dateparse_window(dt, "indexdate", lookback = 365)
  expect_true("window_start" %in% names(res$data))
  expect_true("window_end" %in% names(res$data))
  expect_equal(res$data$window_start[1], as.Date("2020-06-15") - 365)
  expect_equal(res$data$window_end[1], as.Date("2020-06-15") - 1)
})

test_that("dateparse_window creates followup window", {
  dt <- data.frame(id = 1:3, indexdate = as.Date("2020-06-15"))
  res <- dateparse_window(dt, "indexdate", followup = 180)
  expect_equal(res$data$window_start[1], as.Date("2020-06-15") + 1)
  expect_equal(res$data$window_end[1], as.Date("2020-06-15") + 180)
})

test_that("dateparse_window creates both windows", {
  dt <- data.frame(id = 1, indexdate = as.Date("2020-06-15"))
  res <- dateparse_window(dt, "indexdate", lookback = 365, followup = 180)
  expect_equal(res$data$window_start[1], as.Date("2020-06-15") - 365)
  expect_equal(res$data$window_end[1], as.Date("2020-06-15") + 180)
})

test_that("dateparse_window errors when neither specified", {
  dt <- data.frame(id = 1, indexdate = as.Date("2020-06-15"))
  expect_error(dateparse_window(dt, "indexdate"), "Must specify")
})

test_that("dateparse_validate validates correctly", {
  res <- dateparse_validate("2010-01-01", "2020-12-31")
  expect_equal(res$start_date, as.Date("2010-01-01"))
  expect_equal(res$end_date, as.Date("2020-12-31"))
  expect_equal(res$span_days, as.integer(as.Date("2020-12-31") - as.Date("2010-01-01")) + 1L)
  expect_true(res$span_years > 10)
})

test_that("dateparse_validate errors when start > end", {
  expect_error(dateparse_validate("2020-01-01", "2019-01-01"), "after end date")
})

test_that("dateparse_inwindow identifies dates in window", {
  dt <- data.frame(
    id = 1:5,
    event_date = as.Date(c("2020-01-01", "2020-06-15", "2020-12-31", "2021-01-01", "2019-12-31"))
  )
  res <- dateparse_inwindow(dt, "event_date", "2020-01-01", "2020-12-31")
  expect_equal(res$data$in_window, c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(res$info$n_inwindow, 3)
})

test_that("dateparse_inwindow works with variable boundaries", {
  dt <- data.frame(
    id = 1:3,
    event_date = as.Date(c("2020-03-15", "2020-07-01", "2021-01-01")),
    win_start = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01")),
    win_end = as.Date(c("2020-06-30", "2020-06-30", "2020-06-30"))
  )
  res <- dateparse_inwindow(dt, "event_date", "win_start", "win_end")
  expect_equal(res$data$in_window, c(TRUE, FALSE, FALSE))
})

test_that("dateparse_filerange computes year ranges", {
  res <- dateparse_filerange("2010-01-01", "2020-12-31", lookback = 730, followup = 365)
  expect_equal(res$index_start_year, 2010)
  expect_equal(res$index_end_year, 2020)
  expect_equal(res$file_start_year, 2008)
  expect_equal(res$file_end_year, 2021)
})
