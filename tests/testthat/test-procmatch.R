test_that("procmatch_match exact matching works", {
  dt <- data.frame(
    id = 1:5,
    proc1 = c("AA001", "BB002", "AA001", "CC003", "BB002"),
    proc2 = c("DD004", "AA001", "EE005", "FF006", "GG007"),
    stringsAsFactors = FALSE
  )
  res <- procmatch_match(dt, codes = c("AA001", "BB002"), procvars = c("proc1", "proc2"))
  expect_equal(res$data$proc_match, c(1L, 1L, 1L, 0L, 1L))
  expect_equal(res$info$n_codes, 2)
  expect_equal(res$info$n_matches, 4)
})

test_that("procmatch_match prefix matching works", {
  dt <- data.frame(
    id = 1:4,
    proc1 = c("AA001", "AA002", "BB001", "CC001"),
    stringsAsFactors = FALSE
  )
  res <- procmatch_match(dt, codes = "AA", procvars = "proc1", prefix = TRUE)
  expect_equal(res$data$proc_match, c(1L, 1L, 0L, 0L))
})

test_that("procmatch_match handles case insensitivity", {
  dt <- data.frame(id = 1:3, proc1 = c("aa001", "Aa001", "BB001"), stringsAsFactors = FALSE)
  res <- procmatch_match(dt, codes = "AA001", procvars = "proc1")
  expect_equal(res$data$proc_match, c(1L, 1L, 0L))
})

test_that("procmatch_match comma-separated codes work", {
  dt <- data.frame(id = 1:3, proc1 = c("AA001", "BB002", "CC003"), stringsAsFactors = FALSE)
  res <- procmatch_match(dt, codes = "AA001, BB002", procvars = "proc1")
  expect_equal(res$data$proc_match, c(1L, 1L, 0L))
})

test_that("procmatch_first finds first occurrence per person", {
  dt <- data.frame(
    id = c(1, 1, 1, 2, 2),
    proc1 = c("AA001", "BB002", "AA001", "CC003", "AA001"),
    visit_date = as.Date(c("2020-01-01", "2020-06-01", "2020-12-01", "2020-03-01", "2020-09-01")),
    stringsAsFactors = FALSE
  )
  res <- procmatch_first(dt, codes = "AA001", procvars = "proc1",
                         datevar = "visit_date", idvar = "id")
  # Person 1: first match at 2020-01-01
  # Person 2: first match at 2020-09-01
  expect_equal(res$info$n_persons, 2)
  p1 <- res$data[res$data$id == 1, ]
  expect_true(all(p1$proc_ever == 1))
  expect_equal(p1$proc_first_dt[1], as.Date("2020-01-01"))
  p2 <- res$data[res$data$id == 2, ]
  expect_equal(p2$proc_first_dt[1], as.Date("2020-09-01"))
})

test_that("procmatch_first handles no matches", {
  dt <- data.frame(
    id = c(1, 2),
    proc1 = c("XX001", "YY002"),
    visit_date = as.Date(c("2020-01-01", "2020-06-01")),
    stringsAsFactors = FALSE
  )
  res <- procmatch_first(dt, codes = "AA001", procvars = "proc1",
                         datevar = "visit_date", idvar = "id")
  expect_equal(res$info$n_persons, 0)
  expect_true(all(res$data$proc_ever == 0))
})
