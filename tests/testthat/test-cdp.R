test_that("cdp detects confirmed progression (baseline <= 5.5)", {
  dt <- data.frame(
    id = rep(1, 5),
    edss = c(3.0, 3.0, 4.0, 4.5, 4.0),
    visit_date = as.Date(c("2019-06-01", "2020-01-01", "2020-06-01",
                           "2021-01-01", "2021-06-01")),
    dxdate = as.Date("2019-01-01")
  )
  res <- cdp(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
             dxdate = "dxdate", confirmdays = 180, baselinewindow = 730)
  # Baseline: 3.0 (first EDSS within 730 days of dx)
  # Threshold: 1.0 (baseline <= 5.5)
  # First progression: 2020-06-01 (EDSS=4.0, change=1.0 >= 1.0)
  # Confirmation: min EDSS >= 180 days after 2020-06-01 = after 2020-11-28
  #   2021-01-01: 4.5, 2021-06-01: 4.0 -> min = 4.0 >= 3.0 + 1.0 = 4.0 -> confirmed
  expect_equal(nrow(res$data), 1)
  expect_equal(res$data$cdp_date[1], as.Date("2020-06-01"))
  expect_equal(res$info$N_persons, 1)
})

test_that("cdp uses 0.5 threshold when baseline > 5.5", {
  dt <- data.frame(
    id = rep(1, 4),
    edss = c(6.0, 6.5, 6.5, 7.0),
    visit_date = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01", "2021-06-01")),
    dxdate = as.Date("2019-06-01")
  )
  res <- cdp(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
             dxdate = "dxdate", confirmdays = 180, baselinewindow = 730)
  # Baseline: 6.0 (> 5.5), threshold: 0.5
  # First progression: 2020-06-01 (6.5, change=0.5 >= 0.5)
  # Confirmation: after 2020-11-28: 2021-01-01 (6.5), 2021-06-01 (7.0)
  # min = 6.5 >= 6.0 + 0.5 -> confirmed
  expect_equal(nrow(res$data), 1)
  expect_equal(res$data$cdp_date[1], as.Date("2020-06-01"))
})

test_that("cdp rejects unconfirmed progression", {
  dt <- data.frame(
    id = rep(1, 4),
    edss = c(3.0, 4.0, 2.5, 3.0),
    visit_date = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01", "2021-06-01")),
    dxdate = as.Date("2019-06-01")
  )
  res <- cdp(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
             dxdate = "dxdate", confirmdays = 180, baselinewindow = 730)
  # Baseline: 3.0, threshold: 1.0
  # First prog: 2020-06-01 (4.0, change=1.0)
  # Confirmation: after 2020-11-28: 2021-01-01 (2.5)
  # min = 2.5 < 4.0 -> NOT confirmed
  expect_equal(nrow(res$data), 0)
})

test_that("cdp falls back to earliest EDSS if none in baseline window", {
  dt <- data.frame(
    id = rep(1, 4),
    edss = c(2.0, 3.5, 3.5, 3.5),
    visit_date = as.Date(c("2015-01-01", "2020-06-01", "2021-01-01", "2021-06-01")),
    dxdate = as.Date("2020-01-01")
  )
  res <- cdp(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
             dxdate = "dxdate", confirmdays = 180, baselinewindow = 30)
  # No EDSS within 30 days of dxdate=2020-01-01 -> fallback to earliest: 2015-01-01, EDSS=2.0
  # First prog: 2020-06-01 (3.5, change=1.5 >= 1.0)
  # Confirmed by 3.5 at 2021-01-01 and 2021-06-01
  expect_equal(nrow(res$data), 1)
})

test_that("cdp handles multiple persons", {
  dt <- data.frame(
    id = c(rep(1, 3), rep(2, 3)),
    edss = c(2.0, 4.0, 4.0, 3.0, 3.5, 3.0),
    visit_date = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01",
                           "2020-01-01", "2020-06-01", "2021-01-01")),
    dxdate = as.Date("2019-06-01")
  )
  res <- cdp(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
             dxdate = "dxdate", confirmdays = 180, baselinewindow = 730)
  # Person 1: baseline=2.0, prog at 2020-06-01, confirmed at 2021-01-01
  # Person 2: baseline=3.0, no progression (3.5 < 3.0+1.0=4.0)
  expect_equal(nrow(res$data), 1)
  expect_equal(res$data$id[1], 1)
})

test_that("cdp returns correct info", {
  dt <- data.frame(
    id = rep(1, 3),
    edss = c(3.0, 4.0, 4.5),
    visit_date = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01")),
    dxdate = as.Date("2019-06-01")
  )
  res <- cdp(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
             dxdate = "dxdate")
  expect_equal(res$info$confirmdays, 180)
  expect_equal(res$info$baselinewindow, 730)
})
