# setools <img src="man/figures/logo.png" align="right" height="139" alt="" />

> Swedish Registry Epidemiological Research Tools for R

[![R](https://img.shields.io/badge/R-%3E%3D%204.0.0-blue)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![Version](https://img.shields.io/badge/version-0.1.0-orange.svg)]()

**setools** is an R implementation of the Stata `setools` package for Swedish registry-based epidemiological research. It provides a unified toolkit for the most common data operations in Swedish national register studies — all built on [data.table](https://rdatatable.gitlab.io/data.table/) for performance on large datasets.

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("tpcopeland/setools")
```

## Overview

```r
library(setools)
setools()
```

```
setools v0.1.0 - Swedish Registry Epidemiological Research Tools
13 commands available

[dates]
  dateparse_parse           Parse date strings to R Date objects
  dateparse_window          Calculate lookback/followup window dates
  dateparse_validate        Validate date range (start <= end)
  dateparse_inwindow        Check if dates fall within a window
  dateparse_filerange       Determine year files needed for date range

[codes]
  procmatch_match           Match procedure codes across variables
  procmatch_first           Find first occurrence of procedure codes
  cci_se                    Swedish Charlson Comorbidity Index (ICD-7 to ICD-10)
  covarclose                Extract closest covariate values to index date

[migration]
  migrations                Process Swedish migration registry data

[ms]
  sustainedss               Sustained EDSS progression date
  cdp                       Confirmed Disability Progression
  pira                      Progression Independent of Relapse Activity
```

## Functions

### Charlson Comorbidity Index — `cci_se()`

Swedish adaptation supporting ICD-7 through ICD-10 with proper hierarchy rules (Ludvigsson et al. 2021). Handles the full Swedish ICD version timeline (ICD-7 ≤1968, ICD-8 1969–1986, ICD-9 1987–1997, ICD-10 ≥1997).

```r
library(data.table)

diagnoses <- data.table(
  pid  = c("A", "A", "A", "B", "B"),
  icd  = c("I21", "E100", "J44", "C50", "C77"),
  date = as.Date(c("2005-01-01", "2005-06-01", "2008-03-15",
                    "2010-01-01", "2010-06-01"))
)

res <- cci_se(diagnoses, id = "pid", icd = "icd", date = "date",
              components = TRUE)
res$data
#>    pid cci_mi cci_chf cci_pvd ... cci_copd cci_diab ... charlson
#> 1:   A      1       0       0 ...        1        1 ...        3
#> 2:   B      0       0       0 ...        0        0 ...        6

res$info
#> $N_patients = 2, $N_any = 2, $mean_cci = 4.5, $max_cci = 6
```

**Hierarchy rules applied automatically:**
- Complicated diabetes clears uncomplicated diabetes
- Metastatic cancer clears non-metastatic cancer
- Mild liver + ascites promotes to severe liver disease

---

### Migration Registry Processing — `migrations()`

Filters cohorts based on Swedish migration registry data. Applies three exclusion types and computes emigration censoring dates.

```r
master <- data.table(
  id = c("A", "B", "C", "D"),
  study_start = as.Date("2010-01-01"),
  age = c(45L, 52L, 38L, 67L)
)

migfile <- data.table(
  id    = c("A", "B", "C"),
  out_1 = as.Date(c("2005-01-01", "2015-06-15", "2008-01-01")),
  in_1  = as.Date(c(NA, NA, "2012-06-01")),
  out_2 = as.Date(c(NA, NA, NA)),
  in_2  = as.Date(c(NA, NA, NA))
)

res <- migrations(master, migfile)
res$info
#> $N_excluded_emigrated   = 1  (A: left before 2010, never returned)
#> $N_excluded_abroad      = 1  (C: left 2008, returned 2012 — abroad at baseline)
#> $N_censored             = 1  (B: emigrated 2015-06-15 during follow-up)
#> $N_final                = 2  (B and D remain)

res$data
#>    id study_start age migration_out_dt
#> 1:  B  2010-01-01  52       2015-06-15
#> 2:  D  2010-01-01  67             <NA>
```

---

### Closest Covariate Extraction — `covarclose()`

Extracts the observation closest to each person's index date from longitudinal data (e.g., LISA income, education registers). Supports before/after preference, year-format dates, and missing value imputation.

```r
master <- data.table(
  id = c("A", "B"),
  index_date = as.Date(c("2015-06-01", "2016-01-15"))
)

lisa <- data.table(
  id   = c("A", "A", "A", "B", "B"),
  year = c(2013L, 2014L, 2015L, 2015L, 2016L),
  income    = c(280, 300, 320, 400, 420),
  education = c(3L, 3L, 3L, 4L, 4L)
)

res <- covarclose(master, lisa, idvar = "id", indexdate = "index_date",
                  datevar = "year", vars = c("income", "education"),
                  yearformat = TRUE)
res$data
#>    id index_date income education
#> 1:  A 2015-06-01    320         3
#> 2:  B 2016-01-15    420         4
```

---

### Procedure Code Matching — `procmatch_match()` / `procmatch_first()`

Match KVÅ/procedure codes across multiple procedure columns. Supports exact and prefix matching.

```r
operations <- data.table(
  pid  = c("A", "A", "B", "B", "C"),
  op1  = c("NFB40", "JAB30", "NFB49", "ABC10", "JAB30"),
  op2  = c(NA, "NFB49", NA, "NFB40", NA),
  date = as.Date(c("2015-01-01", "2016-03-01", "2015-06-01",
                    "2017-01-01", "2018-09-01"))
)

# Prefix match: all codes starting with "NFB"
res <- procmatch_match(operations, codes = "NFB",
                       procvars = c("op1", "op2"), prefix = TRUE)
res$data[, .(pid, op1, op2, proc_match)]
#>    pid   op1   op2 proc_match
#> 1:   A NFB40  <NA>          1
#> 2:   A JAB30 NFB49          1
#> 3:   B NFB49  <NA>          1
#> 4:   B ABC10 NFB40          1
#> 5:   C JAB30  <NA>          0

# First occurrence per person
res2 <- procmatch_first(operations, codes = "NFB",
                        procvars = c("op1", "op2"),
                        datevar = "date", idvar = "pid", prefix = TRUE)
unique(res2$data[, .(pid, proc_ever, proc_first_dt)])
#>    pid proc_ever proc_first_dt
#> 1:   A         1    2015-01-01
#> 2:   B         1    2015-06-01
#> 3:   C         0          <NA>
```

---

### Date Utilities — `dateparse_*()`

Five functions for common date operations in register studies:

```r
# Parse dates from various formats
dateparse_parse("2020-01-15")$date      # ISO
dateparse_parse("20200115")$date        # YYYYMMDD integer
dateparse_parse("15jan2020")$date       # Stata text format

# Create lookback/followup windows
dt <- data.table(pid = "A", index = as.Date("2020-01-01"))
dateparse_window(dt, "index", lookback = 365, followup = 180)$data
#>    pid      index window_start window_end
#> 1:   A 2020-01-01   2019-01-01 2020-06-29

# Check if dates fall in a window
visits <- data.table(
  pid = 1:3,
  date = as.Date(c("2019-06-01", "2020-03-15", "2021-01-01"))
)
dateparse_inwindow(visits, "date", "2020-01-01", "2020-12-31")$data
#>    pid       date in_window
#> 1:   1 2019-06-01     FALSE
#> 2:   2 2020-03-15      TRUE
#> 3:   3 2021-01-01     FALSE

# Determine which year-files to request
dateparse_filerange("2010-01-01", "2015-12-31",
                    lookback = 3650, followup = 365)
#> $file_start_year = 2000, $file_end_year = 2016
```

---

### MS Disability Progression — `sustainedss()`, `cdp()`, `pira()`

Three functions for analyzing EDSS disability progression in multiple sclerosis studies:

#### Sustained threshold crossing

```r
edss <- data.table(
  pid  = rep("A", 5),
  edss = c(2.0, 4.5, 4.0, 4.5, 4.0),
  date = as.Date(c("2010-01-01", "2010-06-01", "2010-09-01",
                    "2010-12-01", "2011-03-01"))
)

res <- sustainedss(edss, idvar = "pid", edssvar = "edss",
                   datevar = "date", threshold = 4.0)
res$data
#>    pid sustained_dt
#> 1:   A   2010-06-01
```

#### Confirmed Disability Progression (CDP)

```r
edss <- data.table(
  pid  = c(rep("A", 4), rep("B", 4)),
  edss = c(2.0, 3.5, 3.5, 3.5,     # A: confirmed progression
           2.0, 4.0, 2.5, 2.0),     # B: not confirmed (drops back)
  date = as.Date(c("2010-01-01", "2011-06-01", "2011-09-01", "2012-01-01",
                    "2010-01-01", "2010-07-01", "2011-02-01", "2011-07-01")),
  dx   = as.Date("2009-06-01")
)

res <- cdp(edss, idvar = "pid", edssvar = "edss",
           datevar = "date", dxdate = "dx")
res$data
#>    pid   cdp_date
#> 1:   A 2011-06-01
# Patient B excluded — progression not confirmed at 180 days

res$info
#> $N_persons = 1, $N_events = 1
```

#### PIRA vs RAW classification

```r
relapses <- data.table(
  pid = "A",
  relapse_date = as.Date("2015-01-01")  # far from A's CDP
)

res <- pira(edss[pid == "A"], idvar = "pid", edssvar = "edss",
            datevar = "date", dxdate = "dx",
            relapses = relapses, relapseidvar = "pid")
res$data
#>    pid  pira_date   raw_date
#> 1:   A 2011-06-01       <NA>

res$info
#> $N_cdp = 1, $N_pira = 1, $N_raw = 0
```

---

## Return value convention

All setools functions return a list with two elements:

| Element | Contents |
|---------|----------|
| `$data` | A `data.table` with results (one row per patient or per event) |
| `$info` | A named list with summary statistics and parameter values |

```r
res <- cci_se(diagnoses, id = "pid", icd = "icd", date = "date")
res$data     # data.table of Charlson scores
res$info     # list: N_input, N_patients, N_any, mean_cci, max_cci
```

## Dependencies

- **R** >= 4.0.0
- [**data.table**](https://rdatatable.gitlab.io/data.table/) — all data operations use data.table for performance

## References

- Ludvigsson JF, Appelros P, Askling J, et al. *Adaptation of the Charlson Comorbidity Index for register-based research in Sweden.* Clinical Epidemiology. 2021;13:21-41.
- Original Stata `setools` package for Swedish registry epidemiology

## License

MIT
