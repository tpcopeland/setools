# setools 0.1.0

* Initial release of the setools R package.
* Date parsing utilities: `dateparse_parse()`, `dateparse_window()`,
  `dateparse_validate()`, `dateparse_inwindow()`, `dateparse_filerange()`.
* Procedure code matching: `procmatch_match()`, `procmatch_first()`.
* Swedish Charlson Comorbidity Index: `cci_se()` with ICD-7 through ICD-10
  support and hierarchy rules.
* Migration registry processing: `migrations()` with three exclusion types
  and emigration censoring.
* Closest covariate extraction: `covarclose()` with before/after preference,
  year-format dates, and missing value imputation.
* MS disability progression: `sustainedss()`, `cdp()`, `pira()`.
