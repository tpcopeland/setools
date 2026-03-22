test_that("sustainedss finds sustained threshold crossing", {
  dt <- data.frame(
    id = rep(1, 5),
    edss = c(2.0, 3.0, 4.0, 4.5, 4.0),
    visit_date = as.Date(c("2020-01-01", "2020-06-01", "2020-12-01", "2021-04-01", "2021-08-01"))
  )
  res <- sustainedss(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
                     threshold = 4.0, confirmwindow = 182)
  # First >= 4.0 at 2020-12-01, confirmed by 4.5 at 2021-04-01 (121 days)
  # and 4.0 at 2021-08-01 (243 days) -- 4.5 is within window, 4.0 at last date
  # lowest_after = 4.0, last_window check: within [2020-12-02, 2021-06-01]
  # 2021-04-01 is in window: edss 4.5. lowest_after = 4.5 >= 4.0, sustained!
  expect_equal(nrow(res$data), 1)
  expect_equal(res$data$sustained_dt[1], as.Date("2020-12-01"))
})

test_that("sustainedss rejects unsustained events", {
  dt <- data.frame(
    id = rep(1, 4),
    edss = c(2.0, 4.0, 2.0, 2.0),
    visit_date = as.Date(c("2020-01-01", "2020-06-01", "2020-09-01", "2021-01-01"))
  )
  res <- sustainedss(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
                     threshold = 4.0, confirmwindow = 182)
  # EDSS hits 4.0 at 2020-06-01, but drops to 2.0 at 2020-09-01 -> not sustained
  expect_equal(nrow(res$data), 0)
})

test_that("sustainedss handles multiple persons", {
  dt <- data.frame(
    id = c(rep(1, 4), rep(2, 3)),
    edss = c(2.0, 6.0, 6.5, 6.0, 3.0, 4.0, 2.0),
    visit_date = as.Date(c("2020-01-01", "2020-06-01", "2020-10-01", "2021-01-01",
                           "2020-01-01", "2020-06-01", "2020-10-01"))
  )
  res <- sustainedss(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
                     threshold = 6.0, confirmwindow = 182)
  # Person 1: >= 6.0 at 2020-06-01, confirmed (6.5 in window)
  # Person 2: never reaches 6.0
  expect_equal(nrow(res$data), 1)
  expect_equal(res$data$id[1], 1)
  expect_equal(res$data$sustained_dt[1], as.Date("2020-06-01"))
})

test_that("sustainedss iterates to convergence", {
  # Event at first date rejected, second event found
  dt <- data.frame(
    id = rep(1, 6),
    edss = c(2.0, 4.0, 3.0, 3.0, 4.0, 4.5),
    visit_date = as.Date(c("2020-01-01", "2020-03-01", "2020-06-01",
                           "2020-09-01", "2021-01-01", "2021-07-01"))
  )
  res <- sustainedss(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
                     threshold = 4.0, confirmwindow = 182)
  # First >= 4.0 at 2020-03-01, but drops to 3.0 in window -> rejected
  # After update, second >= 4.0 at 2021-01-01, confirmed by 4.5 at 2021-07-01
  expect_equal(res$data$sustained_dt[1], as.Date("2021-01-01"))
  expect_true(res$info$iterations >= 2)
})

test_that("sustainedss returns correct info", {
  dt <- data.frame(
    id = rep(1, 3),
    edss = c(2.0, 4.0, 4.5),
    visit_date = as.Date(c("2020-01-01", "2020-06-01", "2020-12-01"))
  )
  res <- sustainedss(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
                     threshold = 4.0, confirmwindow = 182)
  expect_equal(res$info$threshold, 4.0)
  expect_equal(res$info$confirmwindow, 182)
  expect_equal(res$info$N_events, 1)
})
