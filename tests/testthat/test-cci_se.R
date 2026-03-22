test_that("cci_se computes basic CCI from ICD-10 codes", {
  dt <- data.frame(
    pid = c(1, 1, 2, 2, 3),
    icd_code = c("I21", "E110", "J44", "I50", "Z99"),
    diag_date = as.Date(c("2010-01-01", "2010-06-01", "2015-03-01", "2015-04-01", "2020-01-01")),
    stringsAsFactors = FALSE
  )
  res <- cci_se(dt, id = "pid", icd = "icd_code", date = "diag_date")
  expect_equal(nrow(res$data), 3)
  expect_true("charlson" %in% names(res$data))
  # Person 1: MI (1) + diabetes uncomplicated (1) = 2
  expect_equal(res$data[res$data$pid == 1, ]$charlson, 2)
  # Person 2: COPD (1) + CHF (1) = 2
  expect_equal(res$data[res$data$pid == 2, ]$charlson, 2)
  # Person 3: no matching code = 0
  expect_equal(res$data[res$data$pid == 3, ]$charlson, 0)
})

test_that("cci_se applies diabetes hierarchy", {
  # Both uncomplicated and complicated diabetes -> only complicated counts
  dt <- data.frame(
    pid = c(1, 1),
    icd_code = c("E110", "E112"),
    diag_date = as.Date(c("2010-01-01", "2010-06-01")),
    stringsAsFactors = FALSE
  )
  res <- cci_se(dt, id = "pid", icd = "icd_code", date = "diag_date", components = TRUE)
  expect_equal(res$data$cci_diab, 0)      # cleared
  expect_equal(res$data$cci_diabcomp, 1)   # kept
  expect_equal(res$data$charlson, 2)       # weight 2 for diabcomp
})

test_that("cci_se applies cancer hierarchy", {
  dt <- data.frame(
    pid = c(1, 1),
    icd_code = c("C50", "C77"),
    diag_date = as.Date(c("2010-01-01", "2010-06-01")),
    stringsAsFactors = FALSE
  )
  res <- cci_se(dt, id = "pid", icd = "icd_code", date = "diag_date", components = TRUE)
  expect_equal(res$data$cci_cancer, 0)  # cleared by metastatic
  expect_equal(res$data$cci_mets, 1)    # kept
  expect_equal(res$data$charlson, 6)    # weight 6 for mets
})

test_that("cci_se applies liver hierarchy", {
  # Mild liver + ascites -> severe
  dt <- data.frame(
    pid = c(1, 1),
    icd_code = c("K73", "R18"),
    diag_date = as.Date(c("2010-01-01", "2010-06-01")),
    stringsAsFactors = FALSE
  )
  res <- cci_se(dt, id = "pid", icd = "icd_code", date = "diag_date", components = TRUE)
  expect_equal(res$data$cci_livmild, 0)  # cleared
  expect_equal(res$data$cci_livsev, 1)   # promoted
  expect_equal(res$data$charlson, 3)     # weight 3 for severe liver
})

test_that("cci_se handles YYYYMMDD date format", {
  dt <- data.frame(
    pid = c(1, 1),
    icd_code = c("I21", "I50"),
    diag_date = c(20100101, 20100601),
    stringsAsFactors = FALSE
  )
  res <- cci_se(dt, id = "pid", icd = "icd_code", date = "diag_date", dateformat = "yyyymmdd")
  expect_equal(res$data$charlson, 2)  # MI + CHF
})

test_that("cci_se handles string YMD date format", {
  dt <- data.frame(
    pid = c(1),
    icd_code = c("I21"),
    diag_date = c("2010-01-15"),
    stringsAsFactors = FALSE
  )
  res <- cci_se(dt, id = "pid", icd = "icd_code", date = "diag_date", dateformat = "ymd")
  expect_equal(res$data$charlson, 1)
})

test_that("cci_se handles ICD codes with dots", {
  dt <- data.frame(
    pid = 1,
    icd_code = "I21.0",
    diag_date = as.Date("2010-01-01"),
    stringsAsFactors = FALSE
  )
  res <- cci_se(dt, id = "pid", icd = "icd_code", date = "diag_date")
  expect_equal(res$data$charlson, 1)  # MI detected despite dot
})

test_that("cci_se returns correct info", {
  dt <- data.frame(
    pid = c(1, 1, 2, 3),
    icd_code = c("I21", "I50", "Z99", "I21"),
    diag_date = as.Date(c("2010-01-01", "2010-06-01", "2010-01-01", "2010-01-01")),
    stringsAsFactors = FALSE
  )
  res <- cci_se(dt, id = "pid", icd = "icd_code", date = "diag_date")
  expect_equal(res$info$N_input, 4)
  expect_equal(res$info$N_patients, 3)
  expect_equal(res$info$N_any, 2)  # persons 1 and 3 have CCI > 0
})

test_that("cci_se handles ICD-9 codes", {
  dt <- data.frame(
    pid = 1,
    icd_code = "410",
    diag_date = as.Date("1990-01-01"),
    stringsAsFactors = FALSE
  )
  res <- cci_se(dt, id = "pid", icd = "icd_code", date = "diag_date")
  expect_equal(res$data$charlson, 1)  # MI via ICD-9
})
