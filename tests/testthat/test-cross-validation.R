# Cross-validation tests: setools vs Stata setools
# Tests designed to verify R implementation matches Stata behavior,
# and to probe edge cases where the implementations may diverge.

library(data.table)

# =============================================================================
# CCI: ICD code coverage and hierarchy
# =============================================================================

test_that("cci_se matches Stata for basic ICD-10 multi-comorbidity patient", {
  # Patient with MI (I21), diabetes uncomplicated (E100), and cancer (C50)
  dt <- data.table(
    id = rep("P1", 3),
    icd = c("I21", "E100", "C50"),
    date = as.Date(c("2010-01-01", "2010-06-01", "2011-01-01"))
  )
  res <- cci_se(dt, id = "id", icd = "icd", date = "date")
  # Expected: MI=1 (w1), diab=1 (w1), cancer=1 (w2) = 4
  expect_equal(res$data$charlson, 4L)
})

test_that("cci_se matches Stata hierarchy: diabetes complicated clears uncomplicated", {
  dt <- data.table(
    id = rep("P1", 2),
    icd = c("E100", "E102"),
    date = as.Date(c("2010-01-01", "2010-06-01"))
  )
  res <- cci_se(dt, id = "id", icd = "icd", date = "date", components = TRUE)
  expect_equal(res$data$cci_diab, 0L)
  expect_equal(res$data$cci_diabcomp, 1L)
  # Score: diabcomp weight=2
  expect_equal(res$data$charlson, 2L)
})

test_that("cci_se matches Stata hierarchy: metastatic clears non-metastatic cancer", {
  dt <- data.table(
    id = rep("P1", 2),
    icd = c("C50", "C77"),
    date = as.Date(c("2010-01-01", "2010-06-01"))
  )
  res <- cci_se(dt, id = "id", icd = "icd", date = "date", components = TRUE)
  expect_equal(res$data$cci_cancer, 0L)
  expect_equal(res$data$cci_mets, 1L)
  # Score: mets weight=6
  expect_equal(res$data$charlson, 6L)
})

test_that("cci_se matches Stata hierarchy: mild liver + ascites -> severe liver", {
  dt <- data.table(
    id = rep("P1", 2),
    icd = c("K703", "R18"),
    date = as.Date(c("2010-01-01", "2010-06-01"))
  )
  res <- cci_se(dt, id = "id", icd = "icd", date = "date", components = TRUE)
  expect_equal(res$data$cci_livmild, 0L)
  expect_equal(res$data$cci_livsev, 1L)
  # Score: livsev weight=3
  expect_equal(res$data$charlson, 3L)
})

test_that("cci_se ICD-7 cancer code 191 is recognized", {
  # ICD-7 code 191 = malignant neoplasm of brain
  # Year <= 1968 triggers ICD-7 matching
  dt <- data.table(
    id = "P1",
    icd = "191",
    date = 19650101L
  )
  res <- cci_se(dt, id = "id", icd = "icd", date = "date",
                dateformat = "yyyymmdd", components = TRUE)
  # This should match cancer (ICD-7 range includes 140-204)
  # Stata includes " 191" in its cancer ICD-7 regex
  expect_equal(res$data$cci_cancer, 1L,
               info = "ICD-7 code 191 should match cancer component (matches Stata)")
  expect_equal(res$data$charlson, 2L)  # cancer weight=2
})

test_that("cci_se ICD-9 codes match Stata for all 19 components", {
  # One code per component in ICD-9 era (1987-1997)
  codes <- c(
    "410",   # mi
    "428",   # chf
    "440",   # pvd
    "430",   # cevd
    "491",   # copd
    "490",   # pulm
    "714",   # rheum
    "290",   # dem
    "342",   # plegia
    "250A",  # diab
    "250D",  # diabcomp
    "582",   # renal
    "070",   # livmild
    "789F",  # ascites
    "456A",  # livsev
    "531",   # pud
    "140",   # cancer
    "196",   # mets
    "079J"   # aids
  )
  dt <- data.table(
    id = rep("P1", length(codes)),
    icd = codes,
    date = rep(19900101L, length(codes))
  )
  res <- cci_se(dt, id = "id", icd = "icd", date = "date",
                dateformat = "yyyymmdd", components = TRUE)

  # After hierarchy: diab cleared (diabcomp present), cancer cleared (mets present),
  # livmild -> livsev (livmild + ascites -> severe)
  expect_equal(res$data$cci_mi, 1L)
  expect_equal(res$data$cci_chf, 1L)
  expect_equal(res$data$cci_pvd, 1L)
  expect_equal(res$data$cci_cevd, 1L)
  expect_equal(res$data$cci_copd, 1L)
  expect_equal(res$data$cci_pulm, 1L)
  expect_equal(res$data$cci_rheum, 1L)
  expect_equal(res$data$cci_dem, 1L)
  expect_equal(res$data$cci_plegia, 1L)
  expect_equal(res$data$cci_diab, 0L)       # cleared by diabcomp
  expect_equal(res$data$cci_diabcomp, 1L)
  expect_equal(res$data$cci_renal, 1L)
  expect_equal(res$data$cci_livmild, 0L)     # cleared by livsev
  expect_equal(res$data$cci_livsev, 1L)      # promoted via ascites
  expect_equal(res$data$cci_pud, 1L)
  expect_equal(res$data$cci_cancer, 0L)      # cleared by mets
  expect_equal(res$data$cci_mets, 1L)
  expect_equal(res$data$cci_aids, 1L)

  # Score: 1+1+1+1+1+1+1+1+2+0+2+2+0+3+1+0+6+6 = 30
  expect_equal(res$data$charlson, 30L)
})

test_that("cci_se handles ICD version transitions correctly at 1997 boundary", {
  # 1997 is overlap year: both ICD-9 and ICD-10 should match
  # In Stata: v9 = (yr >= 1987 & yr <= 1997), v10 = (yr >= 1997)
  # Code I21 is ICD-10 MI; code 410 is ICD-9 MI
  dt <- data.table(
    id = c("P1", "P2"),
    icd = c("I21", "410"),
    date = c(19970601L, 19970601L)
  )
  res <- cci_se(dt, id = "id", icd = "icd", date = "date",
                dateformat = "yyyymmdd", components = TRUE)
  # Both should match MI=1 at the 1997 overlap
  expect_equal(res$data[id == "P1", cci_mi], 1L)
  expect_equal(res$data[id == "P2", cci_mi], 1L)
})

test_that("cci_se handles ICD codes with dots like Stata", {
  dt <- data.table(
    id = "P1",
    icd = "I21.0",
    date = as.Date("2010-01-01")
  )
  res <- cci_se(dt, id = "id", icd = "icd", date = "date", components = TRUE)
  expect_equal(res$data$cci_mi, 1L)
})

test_that("cci_se multi-patient Charlson scores match expected values", {
  # Simulate 5 patients with known comorbidity profiles
  dt <- data.table(
    id = c("A", "A", "B", "C", "C", "C", "D", "E"),
    icd = c("I21", "E100", "I50", "C50", "C77", "I21", "J43", "E102"),
    date = as.Date(c("2005-01-01", "2005-06-01", "2010-03-15",
                      "2008-01-01", "2008-06-01", "2009-01-01",
                      "2015-09-01", "2020-04-01"))
  )
  res <- cci_se(dt, id = "id", icd = "icd", date = "date")
  scores <- res$data[order(id)]

  expect_equal(scores[id == "A", charlson], 2L)  # MI(1) + diab(1)
  expect_equal(scores[id == "B", charlson], 1L)  # CHF(1)
  expect_equal(scores[id == "C", charlson], 7L)  # mets(6, cancer cleared) + MI(1)
  expect_equal(scores[id == "D", charlson], 1L)  # COPD(1)
  expect_equal(scores[id == "E", charlson], 2L)  # diabcomp(2)
})

# =============================================================================
# MIGRATIONS: Exclusion types and censoring
# =============================================================================

test_that("migrations Type 1 exclusion matches Stata (emigrated before start, never returned)", {
  master <- data.table(
    id = c("A", "B", "C"),
    study_start = as.Date(c("2010-01-01", "2010-01-01", "2010-01-01"))
  )
  migfile <- data.table(
    id = c("A", "B"),
    out_1 = as.Date(c("2005-01-01", "2008-01-01")),
    in_1 = as.Date(c(NA, NA)),
    out_2 = as.Date(c(NA, NA)),
    in_2 = as.Date(c(NA, NA))
  )
  res <- migrations(master, migfile)
  # A and B emigrated before 2010 and never returned -> excluded
  expect_equal(res$info$N_excluded_emigrated, 2L)
  expect_equal(nrow(res$data), 1L)
  expect_equal(res$data$id, "C")
})

test_that("migrations Type 2 exclusion matches Stata (immigration after start only)", {
  master <- data.table(
    id = c("A", "B"),
    study_start = as.Date(c("2010-01-01", "2010-01-01"))
  )
  migfile <- data.table(
    id = c("A"),
    out_1 = as.Date(NA),
    in_1 = as.Date("2012-06-01"),
    out_2 = as.Date(NA),
    in_2 = as.Date(NA)
  )
  res <- migrations(master, migfile)
  # A's only migration is immigration after study start -> excluded
  expect_equal(res$info$N_excluded_inmigration, 1L)
  expect_equal(nrow(res$data), 1L)
  expect_equal(res$data$id, "B")
})

test_that("migrations Type 3: abroad at baseline (emigrated before start, returned after)", {

  # Person emigrated before study start and returned after
  # Stata catches this as Exclusion Type 3
  # R currently does NOT have this exclusion
  master <- data.table(
    id = c("A", "B"),
    study_start = as.Date(c("2010-01-01", "2010-01-01"))
  )
  migfile <- data.table(
    id = c("A"),
    out_1 = as.Date("2008-01-01"),  # emigrated before study start
    in_1 = as.Date("2012-06-01"),   # returned after study start
    out_2 = as.Date(NA),
    in_2 = as.Date(NA)
  )
  res <- migrations(master, migfile)

  # Stata excludes A as Type 3 (abroad at baseline)
  expect_equal(res$info$N_excluded_abroad, 1L,
               info = "Person abroad at baseline should be excluded (matches Stata Type 3)")
  expect_equal(nrow(res$data), 1L,
               info = "Only person B should remain after excluding person A who was abroad at baseline")
  expect_equal(res$data$id, "B")
})

test_that("migrations censoring date matches Stata for emigration after study start", {
  master <- data.table(
    id = c("A", "B"),
    study_start = as.Date(c("2010-01-01", "2010-01-01"))
  )
  migfile <- data.table(
    id = c("A"),
    out_1 = as.Date("2015-06-15"),  # emigrates during study
    in_1 = as.Date(NA),             # never returns
    out_2 = as.Date(NA),
    in_2 = as.Date(NA)
  )
  res <- migrations(master, migfile)
  expect_equal(res$info$N_censored, 1L)
  expect_equal(res$data[id == "A", migration_out_dt], as.Date("2015-06-15"))
  expect_true(is.na(res$data[id == "B", migration_out_dt]))
})

test_that("migrations preserves master columns", {
  master <- data.table(
    id = c("A", "B"),
    study_start = as.Date(c("2010-01-01", "2010-01-01")),
    age = c(45L, 62L),
    sex = c("M", "F")
  )
  migfile <- data.table(
    id = character(0), out_1 = as.Date(character(0)),
    in_1 = as.Date(character(0)),
    out_2 = as.Date(character(0)), in_2 = as.Date(character(0))
  )
  res <- migrations(master, migfile)
  expect_true("age" %in% names(res$data))
  expect_true("sex" %in% names(res$data))
  expect_equal(nrow(res$data), 2L)
})

test_that("migrations complex scenario: multiple migrations per person", {
  master <- data.table(
    id = c("A", "B", "C"),
    study_start = as.Date(c("2010-01-01", "2010-01-01", "2010-01-01"))
  )
  migfile <- data.table(
    id = c("A", "B", "C"),
    # A: emigrated 2012, returned 2013, emigrated 2015 (censor at 2015)
    out_1 = as.Date(c("2012-06-01", "2005-01-01", "2011-01-01")),
    in_1 = as.Date(c("2013-01-01", NA, NA)),
    out_2 = as.Date(c("2015-03-01", NA, NA)),
    in_2 = as.Date(c(NA, NA, NA))
  )
  res <- migrations(master, migfile)
  # B: emigrated 2005, never returned -> Type 1 exclusion
  expect_equal(res$info$N_excluded_emigrated, 1L)
  # A: should be censored (emigration after study start)
  # C: emigrated 2011, never returned -> censored at 2011-01-01
  expect_true(!is.na(res$data[id == "C", migration_out_dt]))
})

# =============================================================================
# SUSTAINEDSS: Iterative algorithm and edge cases
# =============================================================================

test_that("sustainedss basic sustained threshold crossing matches Stata", {
  dt <- data.table(
    id = rep("P1", 4),
    edss = c(2.0, 4.5, 4.0, 4.5),
    date = as.Date(c("2010-01-01", "2010-06-01", "2010-09-01", "2010-12-01"))
  )
  res <- sustainedss(dt, idvar = "id", edssvar = "edss", datevar = "date",
                     threshold = 4.0)
  expect_equal(nrow(res$data), 1L)
  expect_equal(res$data$sustained_dt, as.Date("2010-06-01"))
})

test_that("sustainedss rejects unsustained events like Stata", {
  dt <- data.table(
    id = rep("P1", 4),
    edss = c(2.0, 4.5, 2.0, 4.5),
    date = as.Date(c("2010-01-01", "2010-06-01", "2010-09-01", "2011-06-01"))
  )
  res <- sustainedss(dt, idvar = "id", edssvar = "edss", datevar = "date",
                     threshold = 4.0)
  # First event at 2010-06-01 should be rejected (drops to 2.0 within window)
  # After rejection, edss at 2010-06-01 replaced with last_window value
  # Then second crossing at 2011-06-01 should be found
  expect_equal(res$data$sustained_dt, as.Date("2011-06-01"))
})

test_that("sustainedss same-date duplicate EDSS at window end", {
  # This tests the discrepancy: Stata uses min() for last_window,
  # R uses max(). When there are two EDSS values on the same date
  # at the end of the confirmation window, this matters.
  dt <- data.table(
    id = rep("P1", 5),
    edss = c(2.0, 4.5, 3.5, 4.5, 3.5),
    date = as.Date(c("2010-01-01", "2010-06-01", "2010-09-01",
                      "2010-09-01", "2010-12-01"))
  )
  # Two measurements on 2010-09-01: 3.5 and 4.5
  # lastdt_window = 2010-09-01 (within 182 days of 2010-06-01)
  # Stata: last_window = min(3.5, 4.5) = 3.5. Since 3.5 < 4.0, NOT sustained.
  # R: last_window = max(3.5, 4.5) = 4.5. Since 4.5 >= 4.0, sustained.
  res <- sustainedss(dt, idvar = "id", edssvar = "edss", datevar = "date",
                     threshold = 4.0)

  # Stata uses min() for last_window (conservative with same-date duplicates).
  # R should match: min(3.5, 4.5) = 3.5 < threshold 4.0 -> NOT sustained.
  # The first event is rejected. Search continues to 2010-12-01 (3.5 < 4.0)
  # -> no sustained event found.
  expect_equal(nrow(res$data), 0L,
               info = "Same-date duplicates: min() at window end should reject (matches Stata)")
})

test_that("sustainedss iterates correctly for multiple rejections", {
  # 3 attempts at crossing threshold, first two rejected
  dt <- data.table(
    id = rep("P1", 8),
    edss = c(2.0, 4.5, 2.0, 4.5, 2.5, 4.5, 4.0, 5.0),
    date = as.Date(c("2010-01-01", "2010-03-01", "2010-06-01",
                      "2010-09-01", "2010-12-01",
                      "2011-06-01", "2011-09-01", "2011-12-01"))
  )
  res <- sustainedss(dt, idvar = "id", edssvar = "edss", datevar = "date",
                     threshold = 4.0)
  expect_true(res$info$iterations >= 2L,
              info = "Should require multiple iterations to reject initial crossings")
  # Final sustained date should be 2011-06-01 (confirmed by 4.0 and 5.0 after)
  expect_equal(res$data$sustained_dt, as.Date("2011-06-01"))
})

test_that("sustainedss with no confirmation measurements (no data after threshold)", {
  dt <- data.table(
    id = rep("P1", 2),
    edss = c(2.0, 4.5),
    date = as.Date(c("2010-01-01", "2010-06-01"))
  )
  res <- sustainedss(dt, idvar = "id", edssvar = "edss", datevar = "date",
                     threshold = 4.0)
  # Stata: when no observations in confirmation window, NOT sustained
  # (lowest_after is missing, so not_sustained condition requires !missing(lowest_after))
  # R should behave the same way
  # Actually: ss_lowest_ will be NA (no obs in window), ss_not_sust_ requires
  # !is.na(ss_lowest_), so NOT rejected. Then sustained_dt = 2010-06-01.
  # This matches Stata where absence of disconfirmation = sustained.
  expect_equal(nrow(res$data), 1L)
  expect_equal(res$data$sustained_dt, as.Date("2010-06-01"))
})

test_that("sustainedss multiple persons match Stata", {
  dt <- data.table(
    id = c(rep("A", 3), rep("B", 3), rep("C", 3)),
    edss = c(2.0, 4.5, 4.0,      # A: sustained at 4 (confirmed)
             1.0, 3.0, 3.5,       # B: never reaches 4
             3.0, 4.0, 2.0),      # C: reaches 4 but drops
    date = as.Date(c("2010-01-01", "2010-06-01", "2010-12-01",
                      "2010-01-01", "2010-06-01", "2010-12-01",
                      "2010-01-01", "2010-06-01", "2010-12-01"))
  )
  res <- sustainedss(dt, idvar = "id", edssvar = "edss", datevar = "date",
                     threshold = 4.0)
  # A: sustained (4.0 confirms 4.5 in window)
  expect_true("A" %in% res$data$id)
  expect_equal(res$data[id == "A", sustained_dt], as.Date("2010-06-01"))
  # B: never reaches threshold
  expect_false("B" %in% res$data$id)
})

# =============================================================================
# CDP: Baseline determination and confirmation
# =============================================================================

test_that("cdp baseline from window matches Stata", {
  dt <- data.table(
    id = rep("P1", 5),
    edss = c(2.0, 3.0, 4.5, 4.0, 4.5),
    date = as.Date(c("2008-01-01", "2009-01-01", "2011-01-01",
                      "2011-06-01", "2012-01-01")),
    dx = as.Date(rep("2008-06-01", 5))
  )
  res <- cdp(dt, idvar = "id", edssvar = "edss", datevar = "date",
             dxdate = "dx", baselinewindow = 730)
  # Baseline window: 2008-06-01 to 2010-06-01
  # First EDSS in window: 3.0 at 2009-01-01 (2008-01-01 is before dx)
  # Wait: 2008-01-01 < 2008-06-01, so NOT in window
  # 2009-01-01 is in [2008-06-01, 2010-06-01] -> baseline = 3.0
  # Threshold: 3.0 <= 5.5 -> requires 1.0 increase -> need >= 4.0
  # First prog: 2011-01-01 (4.5 >= 4.0)
  # Confirmation: min EDSS at >= 2011-01-01 + 180 = 2011-06-30
  # At 2012-01-01: 4.5 >= 4.0 -> confirmed
  expect_equal(nrow(res$data), 1L)
  expect_equal(res$data$cdp_date, as.Date("2011-01-01"))
})

test_that("cdp fallback to earliest EDSS when no measurement in window", {
  # 2008-01-01 + 730 = 2009-12-31, so 2010-01-01 is just outside
  # No EDSS within window [2008-01-01, 2009-12-31] except 2005-01-01 (before dx)
  # -> fallback to earliest: 1.0 at 2005-01-01
  dt <- data.table(
    id = rep("P1", 4),
    edss = c(1.0, 2.5, 3.0, 2.5),
    date = as.Date(c("2005-01-01", "2010-01-01", "2010-07-01", "2011-06-01")),
    dx = as.Date(rep("2008-01-01", 4))
  )
  res <- cdp(dt, idvar = "id", edssvar = "edss", datevar = "date",
             dxdate = "dx", baselinewindow = 730)
  # Baseline: 1.0 (earliest available, fallback)
  # Threshold: 1.0 (baseline <= 5.5)
  # Need >= 2.0 for progression
  # 2.5 at 2010-01-01 meets threshold -> first prog
  # Confirm: min EDSS at >= 2010-01-01 + 180 = 2010-06-30
  # 3.0 at 2010-07-01, 2.5 at 2011-06-01 -> min = 2.5 >= 2.0 -> confirmed
  expect_equal(nrow(res$data), 1L)
  expect_equal(res$data$cdp_date, as.Date("2010-01-01"))
})

test_that("cdp threshold changes at EDSS > 5.5 boundary", {
  # Test that threshold changes from 1.0 to 0.5 at baseline > 5.5
  dt <- data.table(
    id = rep("P1", 3),
    edss = c(6.0, 6.5, 6.5),
    date = as.Date(c("2010-01-01", "2010-07-01", "2011-07-01")),
    dx = as.Date(rep("2009-06-01", 3))
  )
  res <- cdp(dt, idvar = "id", edssvar = "edss", datevar = "date",
             dxdate = "dx")
  # Baseline: 6.0 (> 5.5, threshold = 0.5)
  # 6.5 >= 6.0 + 0.5 = 6.5 -> progression
  # Confirm: 6.5 at 2011-07-01 >= 6.5 -> confirmed
  expect_equal(nrow(res$data), 1L)
  expect_equal(res$data$cdp_date, as.Date("2010-07-01"))
})

test_that("cdp unconfirmed progression is rejected like Stata", {
  dt <- data.table(
    id = rep("P1", 4),
    edss = c(2.0, 4.0, 2.5, 2.0),
    date = as.Date(c("2010-01-01", "2010-07-01", "2011-02-01", "2011-07-01")),
    dx = as.Date(rep("2008-06-01", 4))
  )
  res <- cdp(dt, idvar = "id", edssvar = "edss", datevar = "date",
             dxdate = "dx")
  # Baseline: 2.0 at 2010-01-01 (first in window [2008-06-01, 2010-06-01])
  # Threshold: 1.0 (baseline <=5.5)
  # First prog: 2010-07-01 (4.0 >= 3.0, after baseline date)
  # Confirm check: min EDSS at >= 2010-07-01 + 180 = 2010-12-28
  # At 2011-02-01: 2.5, at 2011-07-01: 2.0
  # min(2.5, 2.0) = 2.0 < 3.0 -> NOT confirmed
  expect_equal(nrow(res$data), 0L)
})

test_that("cdp roving baseline resets after confirmed progression", {
  dt <- data.table(
    id = rep("P1", 6),
    edss = c(2.0, 3.5, 3.5, 5.0, 5.0, 5.5),
    date = as.Date(c("2010-01-01", "2010-07-01", "2011-01-01",
                      "2011-07-01", "2012-01-01", "2012-07-01")),
    dx = as.Date(rep("2009-06-01", 6))
  )
  res <- cdp(dt, idvar = "id", edssvar = "edss", datevar = "date",
             dxdate = "dx", roving = TRUE, allevents = TRUE)
  # First CDP: baseline 2.0, threshold 1.0, prog at 3.5 (2010-07-01),
  # confirmed by 3.5 at 2011-01-01
  # Second CDP: new baseline 5.0 at 2011-07-01, threshold 1.0 (<=5.5),
  # prog at 5.5 should NOT meet threshold (5.5 < 5.0 + 1.0 = 6.0)
  # Wait actually: second baseline = first EDSS after CDP event at 2010-07-01
  # After dropping rows up to 2010-07-01: first remaining is 2011-01-01 (3.5)
  # Wait no - let me re-think...
  # With roving, after first CDP at 2010-07-01:
  # Drop rows <= 2010-07-01, new baseline = 3.5 at 2011-01-01
  # Need >= 3.5 + 1.0 = 4.5
  # 5.0 at 2011-07-01 >= 4.5 -> progression
  # Confirm: 5.0 at 2012-01-01 >= 4.5 -> confirmed (second CDP)
  expect_true(nrow(res$data) >= 1L)
  expect_equal(res$data[event_num == 1, cdp_date], as.Date("2010-07-01"))
})

test_that("cdp multiple persons produce correct results", {
  dt <- data.table(
    id = c(rep("A", 3), rep("B", 3)),
    edss = c(2.0, 4.0, 4.0,  # A: confirmed
             3.0, 3.5, 3.0), # B: not confirmed (3.5 < 3.0 + 1.0)
    date = as.Date(c("2010-01-01", "2010-07-01", "2011-07-01",
                      "2010-01-01", "2010-07-01", "2011-07-01")),
    dx = as.Date(rep("2009-06-01", 6))
  )
  res <- cdp(dt, idvar = "id", edssvar = "edss", datevar = "date",
             dxdate = "dx")
  expect_true("A" %in% res$data$id)
  expect_false("B" %in% res$data$id)
})

# =============================================================================
# PIRA: Relapse window classification
# =============================================================================

test_that("pira classifies CDP outside relapse window as PIRA", {
  dt <- data.table(
    id = rep("P1", 3),
    edss = c(2.0, 4.0, 4.0),
    date = as.Date(c("2010-01-01", "2010-07-01", "2011-07-01")),
    dx = as.Date(rep("2009-06-01", 3))
  )
  rel <- data.table(
    id = "P1",
    relapse_date = as.Date("2015-01-01")  # far from CDP
  )
  res <- pira(dt, idvar = "id", edssvar = "edss", datevar = "date",
              dxdate = "dx", relapses = rel)
  expect_equal(res$info$N_pira, 1L)
  expect_equal(res$info$N_raw, 0L)
  expect_false(is.na(res$data$pira_date[1]))
  expect_true(is.na(res$data$raw_date[1]))
})

test_that("pira classifies CDP within relapse window as RAW", {
  dt <- data.table(
    id = rep("P1", 3),
    edss = c(2.0, 4.0, 4.0),
    date = as.Date(c("2010-01-01", "2010-07-01", "2011-07-01")),
    dx = as.Date(rep("2009-06-01", 3))
  )
  rel <- data.table(
    id = "P1",
    relapse_date = as.Date("2010-06-15")  # 16 days before CDP -> within 90-day window
  )
  res <- pira(dt, idvar = "id", edssvar = "edss", datevar = "date",
              dxdate = "dx", relapses = rel)
  expect_equal(res$info$N_raw, 1L)
  expect_equal(res$info$N_pira, 0L)
})

test_that("pira with no relapses classifies all CDP as PIRA", {
  dt <- data.table(
    id = rep("P1", 3),
    edss = c(2.0, 4.0, 4.0),
    date = as.Date(c("2010-01-01", "2010-07-01", "2011-07-01")),
    dx = as.Date(rep("2009-06-01", 3))
  )
  rel <- data.table(
    id = character(0),
    relapse_date = as.Date(character(0))
  )
  res <- pira(dt, idvar = "id", edssvar = "edss", datevar = "date",
              dxdate = "dx", relapses = rel)
  expect_equal(res$info$N_pira, 1L)
  expect_equal(res$info$N_raw, 0L)
})

test_that("pira window boundary: exactly at windowbefore boundary", {
  dt <- data.table(
    id = rep("P1", 3),
    edss = c(2.0, 4.0, 4.0),
    date = as.Date(c("2010-01-01", "2010-07-01", "2011-07-01")),
    dx = as.Date(rep("2009-06-01", 3))
  )
  # CDP at 2010-07-01; relapse exactly 90 days later = 2010-09-29
  # Window: [2010-09-29 - 90, 2010-09-29 + 30] = [2010-07-01, 2010-10-29]
  # 2010-07-01 IS inside this window (exactly at windowbefore boundary)
  rel <- data.table(
    id = "P1",
    relapse_date = as.Date("2010-09-29")
  )
  res <- pira(dt, idvar = "id", edssvar = "edss", datevar = "date",
              dxdate = "dx", relapses = rel,
              windowbefore = 90L, windowafter = 30L)
  # Stata uses inrange() which is inclusive on both ends
  expect_equal(res$info$N_raw, 1L,
               info = "CDP at exact windowbefore boundary should be RAW (inclusive)")
})

test_that("pira multiple persons with mixed PIRA/RAW", {
  dt <- data.table(
    id = c(rep("A", 3), rep("B", 3)),
    edss = c(2.0, 4.0, 4.0, 2.0, 4.0, 4.0),
    date = as.Date(c("2010-01-01", "2010-07-01", "2011-07-01",
                      "2010-01-01", "2010-07-01", "2011-07-01")),
    dx = as.Date(rep("2009-06-01", 6))
  )
  rel <- data.table(
    id = c("A"),
    relapse_date = as.Date("2010-06-01")  # within window for A's CDP
  )
  res <- pira(dt, idvar = "id", edssvar = "edss", datevar = "date",
              dxdate = "dx", relapses = rel)
  expect_equal(res$info$N_cdp, 2L)
  expect_equal(res$info$N_raw, 1L)    # A is RAW
  expect_equal(res$info$N_pira, 1L)   # B is PIRA
})

# =============================================================================
# COVARCLOSE: Closest value extraction
# =============================================================================

test_that("covarclose closest value matches Stata", {
  master <- data.table(
    id = c("A", "B"),
    index_date = as.Date(c("2010-06-15", "2010-06-15"))
  )
  using <- data.table(
    id = c("A", "A", "A", "B", "B"),
    date = as.Date(c("2009-01-01", "2010-03-01", "2011-01-01",
                      "2008-01-01", "2012-01-01")),
    income = c(100, 200, 300, 400, 500)
  )
  res <- covarclose(master, using, idvar = "id", indexdate = "index_date",
                    datevar = "date", vars = "income")
  # A: closest is 2010-03-01 (106 days before) vs 2011-01-01 (200 days after)
  # -> 200 at 2010-03-01
  expect_equal(res$data[id == "A", income], 200)
  # B: closest is 2008-01-01 (896 days before) vs 2012-01-01 (565 days after)
  # -> 500 at 2012-01-01
  expect_equal(res$data[id == "B", income], 500)
})

test_that("covarclose prefer=before matches Stata", {
  master <- data.table(
    id = "A",
    index_date = as.Date("2010-06-15")
  )
  using <- data.table(
    id = c("A", "A", "A"),
    date = as.Date(c("2009-01-01", "2010-03-01", "2011-01-01")),
    income = c(100, 200, 300)
  )
  res <- covarclose(master, using, idvar = "id", indexdate = "index_date",
                    datevar = "date", vars = "income", prefer = "before")
  # Only before observations: 2009-01-01 and 2010-03-01
  # Closest before: 2010-03-01 -> 200
  expect_equal(res$data$income, 200)
})

test_that("covarclose prefer=after matches Stata", {
  master <- data.table(
    id = "A",
    index_date = as.Date("2010-06-15")
  )
  using <- data.table(
    id = c("A", "A", "A"),
    date = as.Date(c("2009-01-01", "2010-03-01", "2011-01-01")),
    income = c(100, 200, 300)
  )
  res <- covarclose(master, using, idvar = "id", indexdate = "index_date",
                    datevar = "date", vars = "income", prefer = "after")
  # Only after observation: 2011-01-01
  # -> 300
  expect_equal(res$data$income, 300)
})

test_that("covarclose yearformat converts to July 1 like Stata mdy(7,1,year)", {
  master <- data.table(
    id = "A",
    index_date = as.Date("2010-06-15")
  )
  using <- data.table(
    id = c("A", "A"),
    year = c(2009L, 2010L),
    income = c(100, 200)
  )
  res <- covarclose(master, using, idvar = "id", indexdate = "index_date",
                    datevar = "year", vars = "income", yearformat = TRUE)
  # 2009 -> 2009-07-01, 2010 -> 2010-07-01
  # Distance: 2009-07-01 is 349 days before, 2010-07-01 is 16 days after
  # Closest: 2010-07-01 -> 200
  expect_equal(res$data$income, 200)
})

test_that("covarclose imputation with missing codes matches Stata", {
  master <- data.table(
    id = "A",
    index_date = as.Date("2010-06-15")
  )
  using <- data.table(
    id = c("A", "A", "A"),
    date = as.Date(c("2009-01-01", "2010-03-01", "2011-01-01")),
    income = c(100, 99, 300)  # 99 = missing code
  )
  res <- covarclose(master, using, idvar = "id", indexdate = "index_date",
                    datevar = "date", vars = "income",
                    impute = TRUE, missing_codes = 99)
  # After replacing 99 with NA:
  # Forward fill: 2009-01-01=100, 2010-03-01=100 (filled from 2009), 2011-01-01=300
  # Closest to 2010-06-15: 2010-03-01 (100) vs 2011-01-01 (300)
  # 2010-03-01 is 106 days before, 2011-01-01 is 200 days after
  # -> 100
  expect_equal(res$data$income, 100)
})

test_that("covarclose tie-breaking: earlier date wins like Stata", {
  master <- data.table(
    id = "A",
    index_date = as.Date("2010-06-15")
  )
  using <- data.table(
    id = c("A", "A"),
    date = as.Date(c("2010-03-15", "2010-09-15")),  # both 92 days from index
    income = c(100, 200)
  )
  res <- covarclose(master, using, idvar = "id", indexdate = "index_date",
                    datevar = "date", vars = "income")
  # Both equidistant (92 days). Stata sorts by dist then date, keeps first.
  # Earlier date (2010-03-15) should win -> 100
  expect_equal(res$data$income, 100)
})

test_that("covarclose no matching IDs returns NA like Stata", {
  master <- data.table(
    id = c("A", "B"),
    index_date = as.Date(c("2010-06-15", "2010-06-15"))
  )
  using <- data.table(
    id = "C",
    date = as.Date("2010-01-01"),
    income = 999
  )
  res <- covarclose(master, using, idvar = "id", indexdate = "index_date",
                    datevar = "date", vars = "income")
  expect_true(all(is.na(res$data$income)))
})

test_that("covarclose multiple variables extracted simultaneously", {
  master <- data.table(
    id = "A",
    index_date = as.Date("2010-06-15")
  )
  using <- data.table(
    id = c("A", "A"),
    date = as.Date(c("2010-01-01", "2010-12-01")),
    income = c(100, 200),
    education = c(3L, 4L)
  )
  res <- covarclose(master, using, idvar = "id", indexdate = "index_date",
                    datevar = "date", vars = c("income", "education"))
  # Closest: 2010-01-01 (166 days) vs 2010-12-01 (169 days)
  # -> 2010-01-01 wins -> income=100, education=3
  expect_equal(res$data$income, 100)
  expect_equal(res$data$education, 3L)
})

# =============================================================================
# DATEPARSE: Format handling
# =============================================================================

test_that("dateparse_parse handles all 4 formats like Stata dateparse", {
  # dateparse_parse returns list with $date
  # ISO
  expect_equal(dateparse_parse("2010-06-15")$date, as.Date("2010-06-15"))
  # Compact YYYYMMDD
  expect_equal(dateparse_parse("20100615")$date, as.Date("2010-06-15"))
  # European DD/MM/YYYY
  expect_equal(dateparse_parse("15/06/2010")$date, as.Date("2010-06-15"))
  # Stata text format
  expect_equal(dateparse_parse("15jun2010")$date, as.Date("2010-06-15"))
})

test_that("dateparse_window matches Stata dateparse window", {
  dt <- data.table(idx = as.Date("2010-06-15"))
  res <- dateparse_window(dt, indexvar = "idx", lookback = 365, followup = 180)
  expect_equal(res$data$window_start, as.Date("2010-06-15") - 365)
  expect_equal(res$data$window_end, as.Date("2010-06-15") + 180)
})

test_that("dateparse_validate checks start <= end like Stata", {
  res <- dateparse_validate(as.Date("2010-01-01"), as.Date("2010-12-31"))
  expect_equal(res$span_days, 365L)
  expect_true(res$span_years > 0)
})

# =============================================================================
# PROCMATCH: Code matching
# =============================================================================

test_that("procmatch_match exact and prefix modes", {
  dt <- data.table(
    id = c("A", "B", "C"),
    proc1 = c("ABC10", "DEF20", "ABC15"),
    proc2 = c("XYZ99", "ABC10", NA)
  )

  # Exact match
  res_exact <- procmatch_match(dt, codes = "ABC10",
                               procvars = c("proc1", "proc2"))
  expect_equal(res_exact$data$proc_match, c(1L, 1L, 0L))

  # Prefix match
  res_prefix <- procmatch_match(dt, codes = "ABC",
                                procvars = c("proc1", "proc2"),
                                prefix = TRUE)
  expect_equal(res_prefix$data$proc_match, c(1L, 1L, 1L))
})

test_that("procmatch_first finds earliest date per person", {
  dt <- data.table(
    id = c("A", "A", "B"),
    proc1 = c("ABC10", "ABC10", "DEF20"),
    date = as.Date(c("2010-01-01", "2011-01-01", "2010-06-01"))
  )
  res <- procmatch_first(dt, codes = "ABC10", idvar = "id",
                         procvars = "proc1", datevar = "date")
  # procmatch_first keeps all rows; check collapsed values
  a_rows <- res$data[id == "A"]
  b_rows <- res$data[id == "B"]
  expect_equal(a_rows$proc_first_dt[1], as.Date("2010-01-01"))
  expect_equal(a_rows$proc_ever[1], 1L)
  expect_equal(b_rows$proc_ever[1], 0L)
})
