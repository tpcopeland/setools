test_that("covarclose extracts closest values", {
  master <- data.frame(
    id = 1:2,
    indexdate = as.Date(c("2015-06-15", "2015-06-15"))
  )
  using <- data.frame(
    id = c(1, 1, 1, 2, 2),
    obs_date = as.Date(c("2014-01-01", "2015-03-01", "2016-01-01", "2013-01-01", "2015-12-01")),
    income = c(300, 350, 400, 200, 250)
  )
  res <- covarclose(master, using, idvar = "id", indexdate = "indexdate",
                    datevar = "obs_date", vars = "income")
  # Person 1: closest is 2015-03-01 (106 days), income=350
  # Person 2: closest is 2015-12-01 (169 days), income=250
  expect_equal(res$data[res$data$id == 1, ]$income, 350)
  expect_equal(res$data[res$data$id == 2, ]$income, 250)
})

test_that("covarclose prefer=before works", {
  master <- data.frame(id = 1, indexdate = as.Date("2015-06-15"))
  using <- data.frame(
    id = c(1, 1),
    obs_date = as.Date(c("2015-06-10", "2015-06-20")),
    val = c(10, 20)
  )
  res <- covarclose(master, using, idvar = "id", indexdate = "indexdate",
                    datevar = "obs_date", vars = "val", prefer = "before")
  expect_equal(res$data$val, 10)
})

test_that("covarclose prefer=after works", {
  master <- data.frame(id = 1, indexdate = as.Date("2015-06-15"))
  using <- data.frame(
    id = c(1, 1),
    obs_date = as.Date(c("2015-06-10", "2015-06-20")),
    val = c(10, 20)
  )
  res <- covarclose(master, using, idvar = "id", indexdate = "indexdate",
                    datevar = "obs_date", vars = "val", prefer = "after")
  expect_equal(res$data$val, 20)
})

test_that("covarclose yearformat works", {
  master <- data.frame(id = 1, indexdate = as.Date("2015-06-15"))
  using <- data.frame(
    id = c(1, 1, 1),
    year = c(2014, 2015, 2016),
    income = c(300, 350, 400)
  )
  res <- covarclose(master, using, idvar = "id", indexdate = "indexdate",
                    datevar = "year", vars = "income", yearformat = TRUE)
  # 2015-07-01 is closest to 2015-06-15 (16 days)
  expect_equal(res$data$income, 350)
})

test_that("covarclose handles no matches", {
  master <- data.frame(id = 1, indexdate = as.Date("2015-06-15"))
  using <- data.frame(id = 99, obs_date = as.Date("2015-01-01"), val = 10)
  res <- covarclose(master, using, idvar = "id", indexdate = "indexdate",
                    datevar = "obs_date", vars = "val")
  expect_true(is.na(res$data$val))
})

test_that("covarclose impute fills missing values", {
  master <- data.frame(id = 1, indexdate = as.Date("2015-06-15"))
  using <- data.frame(
    id = c(1, 1, 1),
    obs_date = as.Date(c("2014-01-01", "2015-06-01", "2016-01-01")),
    val = c(100, NA, 300)
  )
  res <- covarclose(master, using, idvar = "id", indexdate = "indexdate",
                    datevar = "obs_date", vars = "val", impute = TRUE)
  # Closest is 2015-06-01. Imputed from forward fill: 100
  expect_equal(res$data$val, 100)
})

test_that("covarclose extracts multiple variables", {
  master <- data.frame(id = 1, indexdate = as.Date("2015-06-15"))
  using <- data.frame(
    id = 1, obs_date = as.Date("2015-06-01"), income = 350, education = 3
  )
  res <- covarclose(master, using, idvar = "id", indexdate = "indexdate",
                    datevar = "obs_date", vars = c("income", "education"))
  expect_equal(res$data$income, 350)
  expect_equal(res$data$education, 3)
})
