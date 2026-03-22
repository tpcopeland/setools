# Internal ICD code lookup tables for cci_se()
# Source: Ludvigsson et al. Clinical Epidemiology 2021;13:21-41
# Swedish adaptation of the Charlson Comorbidity Index
#
# Each comorbidity has patterns per ICD version (7, 8, 9, 10).
# Patterns are space-prefixed code prefixes matching regexm(" " + code, pattern).
# In R, matching is done via grepl(pattern, paste0(" ", code)).

.cci_patterns <- list(
  # 1. Myocardial infarction (weight 1)
  mi = list(
    icd7  = c(" 420,1"),
    icd8  = c(" 410", " 411", " 412,01", " 412,91"),
    icd9  = c(" 410", " 412"),
    icd10 = c(" I21", " I22", " I252")
  ),
  # 2. Congestive heart failure (weight 1)
  chf = list(
    icd7  = c(" 422,21", " 422,22", " 434,1", " 434,2"),
    icd8  = c(" 425,08", " 425,09", " 427,0", " 427,1", " 428"),
    icd9  = c(" 402A", " 402B", " 402X", " 404A", " 404B", " 404X", " 425E", " 425F", " 425H", " 425W", " 425X", " 428"),
    icd10 = c(" I110", " I130", " I132", " I255", " I420", " I426", " I427", " I428", " I429", " I43", " I50")
  ),
  # 3. Peripheral vascular disease (weight 1)
  pvd = list(
    icd7  = c(" 450,1", " 451", " 453"),
    icd8  = c(" 440", " 441", " 443,1", " 443,9"),
    icd9  = c(" 440", " 441", " 443B", " 443X", " 447B", " 557"),
    icd10 = c(" I70", " I71", " I731", " I738", " I739", " I771", " I790", " I792", " K55")
  ),
  # 4. Cerebrovascular disease (weight 1)
  cevd = list(
    icd7  = c(" 330", " 331", " 332", " 333", " 334"),
    icd8  = c(" 430", " 431", " 432", " 433", " 434", " 435", " 436", " 437", " 438"),
    icd9  = c(" 430", " 431", " 432", " 433", " 434", " 435", " 436", " 437", " 438"),
    icd10 = c(" G45", " I60", " I61", " I62", " I63", " I64", " I67", " I69")
  ),
  # 5. COPD (weight 1)
  copd = list(
    icd7  = c(" 502", " 527,1"),
    icd8  = c(" 491", " 492"),
    icd9  = c(" 491", " 492", " 496"),
    icd10 = c(" J43", " J44")
  ),
  # 6. Other chronic pulmonary disease (weight 1)
  pulm = list(
    icd7  = c(" 241", " 501", " 523", " 524", " 525", " 526"),
    icd8  = c(" 490", " 493", " 515", " 516", " 517", " 518"),
    icd9  = c(" 490", " 493", " 494", " 495", " 500", " 501", " 502", " 503", " 504", " 505", " 506", " 507", " 508", " 516", " 517"),
    icd10 = c(" J41", " J42", " J45", " J46", " J47", " J60", " J61", " J62", " J63", " J64", " J65", " J66", " J67", " J68", " J69", " J70")
  ),
  # 7. Rheumatic disease (weight 1)
  rheum = list(
    icd7  = c(" 722,00", " 722,01", " 722,10", " 722,20", " 722,23", " 456,0", " 456,1", " 456,2", " 456,3"),
    icd8  = c(" 446", " 696", " 712,0", " 712,1", " 712,2", " 712,3", " 712,5", " 716", " 734,0", " 734,1", " 734,9"),
    icd9  = c(" 446", " 696A", " 710A", " 710B", " 710C", " 710D", " 710E", " 714", " 719D", " 720", " 725"),
    icd10 = c(" M05", " M06", " M123", " M070", " M071", " M072", " M073", " M08", " M13", " M30", " M313", " M314", " M315", " M316", " M32", " M33", " M34", " M350", " M351", " M353", " M45", " M46")
  ),
  # 8. Dementia (weight 1)
  dem = list(
    icd7  = c(" 304", " 305"),
    icd8  = c(" 290"),
    icd9  = c(" 290", " 294B", " 331A", " 331B", " 331C", " 331X"),
    icd10 = c(" F00", " F01", " F02", " F03", " F051", " G30", " G311", " G319")
  ),
  # 9. Hemiplegia/paraplegia (weight 2)
  plegia = list(
    icd7  = c(" 351", " 352", " 357,00"),
    icd8  = c(" 343", " 344"),
    icd9  = c(" 342", " 343", " 344A", " 344B", " 344C", " 344D", " 344E", " 344F"),
    icd10 = c(" G114", " G80", " G81", " G82", " G830", " G831", " G832", " G833", " G838")
  ),
  # 10. Diabetes without complications (weight 1)
  diab = list(
    icd7  = c(" 260,09"),
    icd8  = c(" 250,00", " 250,07", " 250,08"),
    icd9  = c(" 250A", " 250B", " 250C"),
    icd10 = c(" E100", " E101", " E106", " E109", " E110", " E111", " E119", " E120", " E121", " E129", " E130", " E131", " E139", " E140", " E141", " E149")
  ),
  # 11. Diabetes with complications (weight 2)
  diabcomp = list(
    icd7  = c(" 260,2", " 260,21", " 260,29", " 260,3", " 260,4", " 260,49", " 260,99"),
    icd8  = c(" 250,01", " 250,02", " 250,03", " 250,04", " 250,05"),
    icd9  = c(" 250D", " 250E", " 250F", " 250G"),
    icd10 = c(" E102", " E103", " E104", " E105", " E107",
              " E112", " E113", " E114", " E115", " E116", " E117",
              " E122", " E123", " E124", " E125", " E126", " E127",
              " E132", " E133", " E134", " E135", " E136", " E137",
              " E142", " E143", " E144", " E145", " E146", " E147")
  ),
  # 12. Renal disease (weight 2)
  renal = list(
    icd7  = c(" 592", " 593", " 792"),
    icd8  = c(" 582", " 583", " 584", " 792", " 593,00", " 403,99", " 404,99", " 792,99", " Y29,01"),
    icd9  = c(" 403A", " 403B", " 403X", " 582", " 583", " 585", " 586", " 588A", " V42A", " V45B", " V56"),
    icd10 = c(" I120", " I131", " N032", " N033", " N034", " N035", " N036", " N037",
              " N052", " N053", " N054", " N055", " N056", " N057",
              " N11", " N18", " N19", " N250", " Q611", " Q612", " Q613", " Q614",
              " Z49", " Z940", " Z992")
  ),
  # 13. Mild liver disease (weight 1)
  livmild = list(
    icd7  = c(" 581"),
    icd8  = c(" 070", " 571", " 573"),
    icd9  = c(" 070", " 571C", " 571E", " 571F", " 573"),
    icd10 = c(" B15", " B16", " B17", " B18", " B19", " K703", " K709", " K73", " K746", " K754")
  ),
  # 14. Ascites (internal, used for liver hierarchy)
  ascites = list(
    icd7  = character(0),
    icd8  = c(" 785,3"),
    icd9  = c(" 789F"),
    icd10 = c(" R18")
  ),
  # 15. Moderate/severe liver disease (weight 3)
  livsev = list(
    icd7  = c(" 462,1"),
    icd8  = c(" 456,0", " 571,9", " 573,02"),
    icd9  = c(" 456A", " 456B", " 456C", " 572C", " 572D", " 572E"),
    icd10 = c(" I850", " I859", " I982", " I983")
  ),
  # 16. Peptic ulcer disease (weight 1)
  pud = list(
    icd7  = c(" 540", " 541", " 542"),
    icd8  = c(" 531", " 532", " 533", " 534"),
    icd9  = c(" 531", " 532", " 533", " 534"),
    icd10 = c(" K25", " K26", " K27", " K28")
  ),
  # 17. Cancer non-metastatic (weight 2)
  cancer = list(
    icd7  = c(" 140", " 141", " 142", " 143", " 144", " 145", " 146", " 147", " 148", " 149",
              " 150", " 151", " 152", " 153", " 154", " 155", " 156", " 157", " 158", " 159",
              " 160", " 161", " 162", " 163", " 164", " 165", " 166", " 167", " 168", " 169",
              " 170", " 171", " 172", " 173", " 174", " 175", " 176", " 177", " 178", " 179",
              " 180", " 181", " 182", " 183", " 184", " 185", " 186", " 187", " 188", " 189",
              " 190", " 191", " 192", " 193", " 194", " 195", " 196", " 197", " 200", " 201", " 202", " 203", " 204"),
    icd8  = c(" 140", " 141", " 142", " 143", " 144", " 145", " 146", " 147", " 148", " 149",
              " 150", " 151", " 152", " 153", " 154", " 155", " 156", " 157", " 158", " 159",
              " 160", " 161", " 162", " 163", " 164", " 165", " 166", " 167", " 168", " 169",
              " 170", " 171", " 172", " 174", " 180", " 181", " 182", " 183", " 184", " 185",
              " 186", " 187", " 188", " 189", " 190", " 191", " 192", " 193", " 194", " 195",
              " 196", " 197", " 198", " 199", " 200", " 201", " 202", " 203", " 204", " 205", " 206", " 207", " 209"),
    icd9  = c(" 140", " 141", " 142", " 143", " 144", " 145", " 146", " 147", " 148", " 149",
              " 150", " 151", " 152", " 153", " 154", " 155", " 156", " 157", " 158", " 159",
              " 160", " 161", " 162", " 163", " 164", " 165", " 166", " 167", " 168", " 169",
              " 170", " 171", " 172", " 174", " 175", " 176", " 177", " 178", " 179", " 180",
              " 181", " 182", " 183", " 184", " 185", " 186", " 187", " 188", " 189", " 190",
              " 191", " 192", " 193", " 194", " 195", " 196", " 197", " 198", " 199", " 200",
              " 201", " 202", " 203", " 204", " 205", " 206", " 207", " 208"),
    icd10 = c(" C00", " C01", " C02", " C03", " C04", " C05", " C06", " C07", " C08", " C09",
              " C10", " C11", " C12", " C13", " C14", " C15", " C16", " C17", " C18", " C19",
              " C20", " C21", " C22", " C23", " C24", " C25", " C26", " C27", " C28", " C29",
              " C30", " C31", " C32", " C33", " C34", " C35", " C36", " C37", " C38", " C39",
              " C40", " C41", " C43", " C45", " C46", " C47", " C48", " C49", " C50", " C51",
              " C52", " C53", " C54", " C55", " C56", " C57", " C58", " C59", " C60", " C61",
              " C62", " C63", " C64", " C65", " C66", " C67", " C68", " C69", " C70", " C71",
              " C72", " C73", " C74", " C75", " C76", " C81", " C82", " C83", " C84", " C85",
              " C86", " C88", " C89", " C90", " C91", " C92", " C93", " C94", " C95", " C96", " C97")
  ),
  # 18. Metastatic cancer (weight 6)
  mets = list(
    icd7  = c(" 156,91", " 198", " 199"),
    icd8  = c(" 196", " 197", " 198", " 199"),
    icd9  = c(" 196", " 197", " 198", " 199A", " 199B"),
    icd10 = c(" C77", " C78", " C79", " C80")
  ),
  # 19. AIDS/HIV (weight 6)
  aids = list(
    icd7  = character(0),
    icd8  = character(0),
    icd9  = c(" 079J", " 279K"),
    icd10 = c(" B20", " B21", " B22", " B23", " B24", " F024", " O987", " R75", " Z219", " Z717")
  )
)

# CCI weights per component (indexed by name)
.cci_weights <- c(
  mi = 1L, chf = 1L, pvd = 1L, cevd = 1L, copd = 1L, pulm = 1L,
  rheum = 1L, dem = 1L, plegia = 2L, diab = 1L, diabcomp = 2L,
  renal = 2L, livmild = 1L, livsev = 3L, pud = 1L, cancer = 2L,
  mets = 6L, aids = 6L
)

# Component labels
.cci_labels <- c(
  mi = "Myocardial infarction",
  chf = "Congestive heart failure",
  pvd = "Peripheral vascular disease",
  cevd = "Cerebrovascular disease",
  copd = "COPD",
  pulm = "Other chronic pulmonary disease",
  rheum = "Rheumatic disease",
  dem = "Dementia",
  plegia = "Hemiplegia/paraplegia",
  diab = "Diabetes without complications",
  diabcomp = "Diabetes with complications",
  renal = "Renal disease",
  livmild = "Mild liver disease",
  livsev = "Moderate/severe liver disease",
  pud = "Peptic ulcer disease",
  cancer = "Cancer (non-metastatic)",
  mets = "Metastatic cancer",
  aids = "AIDS/HIV"
)
