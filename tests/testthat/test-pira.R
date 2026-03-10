test_that("pira classifies CDP outside relapse window as PIRA", {
  dt <- data.frame(
    id = rep(1, 4),
    edss = c(3.0, 4.0, 4.0, 4.5),
    visit_date = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01", "2021-06-01")),
    dxdate = as.Date("2019-06-01")
  )
  relapses <- data.frame(
    id = 1,
    relapse_date = as.Date("2018-01-01")  # relapse far from CDP
  )
  res <- pira(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
              dxdate = "dxdate", relapses = relapses)
  expect_equal(res$info$N_cdp, 1)
  expect_equal(res$info$N_pira, 1)
  expect_equal(res$info$N_raw, 0)
  expect_false(is.na(res$data$pira_date[1]))
  expect_true(is.na(res$data$raw_date[1]))
})

test_that("pira classifies CDP within relapse window as RAW", {
  dt <- data.frame(
    id = rep(1, 4),
    edss = c(3.0, 4.0, 4.0, 4.5),
    visit_date = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01", "2021-06-01")),
    dxdate = as.Date("2019-06-01")
  )
  relapses <- data.frame(
    id = 1,
    relapse_date = as.Date("2020-05-15")  # 17 days before CDP date of 2020-06-01
  )
  res <- pira(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
              dxdate = "dxdate", relapses = relapses,
              windowbefore = 90, windowafter = 30)
  # CDP at 2020-06-01, relapse at 2020-05-15
  # Window: [2020-05-15 - 90, 2020-05-15 + 30] = [2020-02-14, 2020-06-14]
  # 2020-06-01 is in window -> RAW
  expect_equal(res$info$N_cdp, 1)
  expect_equal(res$info$N_pira, 0)
  expect_equal(res$info$N_raw, 1)
})

test_that("pira with no relapses classifies all as PIRA", {
  dt <- data.frame(
    id = rep(1, 3),
    edss = c(3.0, 4.0, 4.5),
    visit_date = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01")),
    dxdate = as.Date("2019-06-01")
  )
  relapses <- data.frame(id = integer(0), relapse_date = as.Date(character(0)))
  res <- pira(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
              dxdate = "dxdate", relapses = relapses)
  expect_equal(res$info$N_pira, 1)
  expect_equal(res$info$N_raw, 0)
})

test_that("pira handles no CDP events", {
  dt <- data.frame(
    id = rep(1, 3),
    edss = c(3.0, 3.5, 3.0),
    visit_date = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01")),
    dxdate = as.Date("2019-06-01")
  )
  relapses <- data.frame(id = 1, relapse_date = as.Date("2020-04-01"))
  res <- pira(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
              dxdate = "dxdate", relapses = relapses)
  expect_equal(res$info$N_cdp, 0)
  expect_equal(res$info$N_pira, 0)
  expect_equal(res$info$N_raw, 0)
})

test_that("pira handles multiple persons", {
  dt <- data.frame(
    id = c(rep(1, 3), rep(2, 3)),
    edss = c(3.0, 4.0, 4.5, 2.0, 3.0, 3.5),
    visit_date = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01",
                           "2020-01-01", "2020-06-01", "2021-01-01")),
    dxdate = as.Date("2019-06-01")
  )
  relapses <- data.frame(
    id = c(1, 2),
    relapse_date = as.Date(c("2018-01-01", "2020-05-20"))
  )
  res <- pira(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
              dxdate = "dxdate", relapses = relapses,
              windowbefore = 90, windowafter = 30)
  # Person 1: CDP confirmed, relapse far away -> PIRA
  # Person 2: CDP at 2020-06-01, change 1.0 >= 1.0, confirmed at 2021-01-01
  #   relapse 2020-05-20 window [2020-02-19, 2020-06-19] -> includes 2020-06-01 -> RAW
  expect_equal(res$info$N_cdp, 2)
  expect_equal(res$info$N_pira, 1)
  expect_equal(res$info$N_raw, 1)
})

test_that("pira returns correct info fields", {
  dt <- data.frame(
    id = rep(1, 3),
    edss = c(3.0, 4.0, 4.5),
    visit_date = as.Date(c("2020-01-01", "2020-06-01", "2021-01-01")),
    dxdate = as.Date("2019-06-01")
  )
  relapses <- data.frame(id = integer(0), relapse_date = as.Date(character(0)))
  res <- pira(dt, idvar = "id", edssvar = "edss", datevar = "visit_date",
              dxdate = "dxdate", relapses = relapses)
  expect_equal(res$info$windowbefore, 90)
  expect_equal(res$info$windowafter, 30)
  expect_equal(res$info$confirmdays, 180)
  expect_equal(res$info$baselinewindow, 730)
})
