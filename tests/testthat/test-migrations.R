test_that("migrations excludes emigrated-before-start individuals", {
  master <- data.frame(
    id = 1:4,
    study_start = as.Date(c("2010-01-01", "2010-01-01", "2010-01-01", "2010-01-01"))
  )
  migfile <- data.frame(
    id = 1:3,
    out_1 = as.Date(c("2008-01-01", "2011-06-01", "2005-01-01")),
    in_1 = as.Date(c(NA, NA, "2006-01-01")),
    out_2 = as.Date(c(NA, NA, NA)),
    in_2 = as.Date(c(NA, NA, NA))
  )
  res <- migrations(master, migfile)
  # Person 1: emigrated 2008, no return -> excluded (type 1)
  # Person 2: emigrated 2011 (after start) -> censored, not excluded
  # Person 3: emigrated 2005, returned 2006 -> not excluded (returned before start)
  # Person 4: not in migration file -> kept
  expect_true(1L %in% res$info$N_excluded_emigrated)
  expect_false(1 %in% res$data$id)
  expect_true(2 %in% res$data$id)
  expect_true(4 %in% res$data$id)
})

test_that("migrations excludes immigration-after-start individuals", {
  master <- data.frame(
    id = 1:3,
    study_start = as.Date("2010-01-01")
  )
  migfile <- data.frame(
    id = 1:2,
    out_1 = as.Date(c(NA, "2012-01-01")),
    in_1 = as.Date(c("2011-06-01", NA))
  )
  res <- migrations(master, migfile)
  # Person 1: only migration is in_1 after study_start -> excluded (type 2)
  # Person 2: emigrated after start -> censored
  expect_equal(res$info$N_excluded_inmigration, 1)
  expect_false(1 %in% res$data$id)
  expect_true(2 %in% res$data$id)
})

test_that("migrations computes censoring dates", {
  master <- data.frame(
    id = 1:2,
    study_start = as.Date("2010-01-01")
  )
  migfile <- data.frame(
    id = 1:2,
    out_1 = as.Date(c("2012-06-15", "2015-03-01")),
    in_1 = as.Date(c(NA, NA))
  )
  res <- migrations(master, migfile)
  expect_equal(res$info$N_censored, 2)
  expect_equal(res$data[res$data$id == 1, ]$migration_out_dt, as.Date("2012-06-15"))
})

test_that("migrations handles no cohort members in migration file", {
  master <- data.frame(id = 1:2, study_start = as.Date("2010-01-01"))
  migfile <- data.frame(id = 99, out_1 = as.Date("2012-01-01"), in_1 = as.Date(NA))
  res <- migrations(master, migfile)
  expect_equal(res$info$N_excluded_total, 0)
  expect_equal(nrow(res$data), 2)
})

test_that("migrations preserves master columns", {
  master <- data.frame(
    id = 1:3,
    study_start = as.Date("2010-01-01"),
    age = c(45, 50, 55),
    sex = c("M", "F", "M"),
    stringsAsFactors = FALSE
  )
  migfile <- data.frame(id = integer(0), out_1 = as.Date(character(0)),
                        in_1 = as.Date(character(0)))
  res <- migrations(master, migfile)
  expect_true("age" %in% names(res$data))
  expect_true("sex" %in% names(res$data))
})
