# Suppress R CMD check NOTEs for data.table NSE column references
utils::globalVariables(c(
  # data.table special symbols
  ".", ".I", ".N", ".SD",
  # cci_se internal columns
  ".code", ".yr", ".v7", ".v8", ".v9", ".v10",
  "ascites", "cancer", "charlson", "diab", "diabcomp", "livmild", "livsev", "mets",
  # sustainedss internal columns
  "ss_id_", "ss_edss_", "ss_date_", "ss_edss_work_", "ss_obs_id_",
  "ss_first_dt_", "ss_in_confirm_", "ss_lowest_", "ss_lastdt_",
  "ss_last_val_", "ss_not_sust_", "ss_sustained_dt_", "sustained_dt",
  # cdp internal columns
  "cd_id_", "cd_edss_", "cd_date_", "cd_dxdate_",
  "cd_bl_edss_", "cd_bl_date_", "cd_inwin_", "cd_fwdt_",
  "cd_pthresh_", "cd_echg_", "cd_isprog_", "cd_fpdt_",
  "cd_confedss_", "cd_minconf_", "cd_confirmed_",
  "cdp_date", "event_num", ".evt_dt_",
  # pira internal columns
  "pi_reldt_", "pi_lastrel_", "pi_hasrel_", "pi_postrel_",
  "pi_newbldt_", "pi_newbledss_", "pi_inrelwin_", "pi_anyrel_",
  "pira_date", "raw_date",
  # migrations internal columns
  "in_", "out_", "last_in", "last_out", "excl_inmig",
  "migration_out_dt", "total_mig",
  # procmatch internal columns
  ".row_match", "proc_first_dt", "proc_ever",
  # covarclose internal columns
  ".covar_date", ".dist", ".abs_dist", ".has_before", ".has_after"
))
