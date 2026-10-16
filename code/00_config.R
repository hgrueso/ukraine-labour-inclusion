# 00_config.R: paths, codebooks, age groups and variable selection ----------
# Run every script from the Ukraine "analysis" folder (the one holding data/).
suppressPackageStartupMessages({
  library(here); library(dplyr); library(tidyr); library(readr); library(stringr)
  library(ggplot2); library(scales)
})
source(here::here("R", "theme.R")); source(here::here("R", "utils.R")); source(here::here("R", "translations.R")); ensure_dirs()

# ---- data files ------------------------------------------------------------
find1 <- function(dir, pattern) {
  f <- list.files(here::here(dir), pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(f)) f[1] else NA_character_
}
PATHS <- list(
  # SSSU Labour Force Survey 2021, anonymised public-use file (pre-war baseline)
  lfs      = find1("data/mic_poc_rob_syly_21", "\\.sav$"),
  lfs_xlsx = find1("data/mic_poc_rob_syly_21", "^Ukr_2021\\.xlsx$"),
  # UNICEF Household Social and Economic Status Survey (HSESS), Dec 2023 to Feb 2024
  hsess_hh  = find1("data", "SESH .*Household.*\\.xlsx$"),
  hsess_ind = find1("data", "SESHS IND.*\\.xlsx$")
)

# Age groups: 15-18 education, training and apprenticeships (14-17 are legal minors);
# 19-24 labour market (participation, employment); 15-24 education-to-work transition;
# 15-29 ILO NEET frame, reported for reference. HSESS has no labour module below age 15.
AGE_MIN <- 15; AGE_MAX <- 24

# ---- LFS 2021 codebook (verified against SSSU "Opys zminnykh_2021") ----------
# sex: 1 = woman, 2 = man. Age only in bands (15-24 is one band).
LFS_LABELS <- list(
  age_gr = c("1"="15-24","2"="25-29","3"="30-34","4"="35-39","5"="40-49","6"="50-59","7"="60-70","8"="71+"),
  sex = c("1"="Women","2"="Men"),
  tp_ar = c("1"="Urban","2"="Rural"),
  marital_st = c("1"="Married","2"="Never married","3"="Divorced","4"="Widowed"),
  education = c("1"="Higher","2"="Vocational","3"="Secondary or less"),
  labour_force = c("1"="Employed","2"="Unemployed","3"="Outside labour force"),
  stat_empl = c("1"="Employee","2"="Self-employed / unpaid family"),
  occupation = c("1"="Managers","2"="Professionals","3"="Technicians","4"="Clerical",
                 "5"="Service and sales","6"="Skilled agricultural","7"="Craft trades",
                 "8"="Machine operators","9"="Elementary"),
  special_dipl = c("1"="Managers","2"="Professionals","3"="Technicians","4"="Clerical",
                   "5"="Service and sales","6"="Skilled agricultural","7"="Craft trades",
                   "8"="Machine operators","9"="No qualification"),
  informal_empl = c("1"="Informal","2"="Formal"),
  actual_duration = c("1"="<20h","2"="20-29h","3"="30-39h","4"="40h","5"=">40h"),
  reason_unempl = c("1"="Laid off","2"="Quit","3"="Contract ended","4"="Not placed after education",
                    "5"="Seasonal","6"="Other"),
  way_job_search = c("1"="Press","2"="Internet","3"="Personal contacts","4"="State Employment Service","5"="Other"),
  durat_job_search = c("1"="<3 months","2"="3-6 months","3"="6-12 months","4"="12+ months"),
  reason_inactiv = c("1"="Pensioner","2"="Student","3"="Home duties / dependent","4"="Discouraged",
                     "5"="Seasonal","6"="Other (health etc.)")
)
REGION_LABELS <- c("1"="Crimea","5"="Vinnytsia","7"="Volyn","12"="Dnipropetrovsk","14"="Donetsk",
  "18"="Zhytomyr","21"="Zakarpattia","23"="Zaporizhzhia","26"="Ivano-Frankivsk","32"="Kyiv oblast",
  "35"="Kirovohrad","44"="Luhansk","46"="Lviv","48"="Mykolaiv","51"="Odesa","53"="Poltava","56"="Rivne",
  "59"="Sumy","61"="Ternopil","63"="Kharkiv","65"="Kherson","68"="Khmelnytskyi","71"="Cherkasy",
  "73"="Chernivtsi","74"="Chernihiv","80"="Kyiv city","85"="Sevastopol")

# ---- LFS key variables by concept (verified against the SSSU codebook) --------
LFS_CONCEPT_MAP <- list(
  "Weight / design" = "weight_year", "Household / person ID" = c("kod_res", "kod_obs"),
  "Age" = "age_gr", "Sex" = "sex", "Region / oblast" = "rg", "Urban / rural" = "tp_ar",
  "Marital status" = "marital_st", "Education level" = c("education", "special_dipl"),
  "Caregiving / unpaid work" = "reason_inactiv", "Currently studying" = "reason_inactiv",
  "Employment status" = c("labour_force", "stat_empl"),
  "Job search / unemployment" = c("labour_force", "reason_unempl", "durat_job_search", "way_job_search"),
  "Reason not working" = c("reason_inactiv", "reason_unempl"), "Occupation / sector" = "occupation",
  "Formal / informal" = "informal_empl", "Hours worked" = "actual_duration")
LFS_KEY <- unique(unlist(LFS_CONCEPT_MAP))

# ---- HSESS variables (verified against the questionnaire codes) ---------------
# c(code, fallback regex on the question text). Codes come from the code row of
# each sheet (also listed in the "variables_ukr" sheet).
HSESS_IND <- list(
  key = c(NA, "унікальний ключ"), weight = c(NA, "Кінцева статистична вага"),
  oblast = c("OBLAST", "^Область$"), settlement = c("SETTL_TYPE", NA),
  relation = c("IND_A1", "^[AА]1\\."), age = c("IND_A2", "^[AА]2\\."), sex = c("IND_A3", "^[AА]3\\."), marital = c("IND_A6", "^[AА]6\\."),
  educ_level = c("IND_A7", "^[AА]7\\."), in_educ = c("IND_A9", "^[AА]9\\."), institution = c("IND_A10", "^[AА]10\\."),
  preschool_reason = c("IND_A14", "^[AА]14\\."), moved = c("IND_A15", "^[AА]15\\."), idp = c("IND_A19", "^[AА]19\\."),
  econ_status = c("IND_B1", "^Б1\\."), job_type = c("IND_B3_1", "^Б3\\.1\\. "), sector = c("IND_B5_2", "^Б5\\.2\\."),
  job_title = c("IND_B6_1", "^Б6\\.1\\."), occupation = c("IND_B6_2", "^Б6\\.2\\."), hours = c("IND_B9_1", "^Б9\\.1\\."), attitude = c("IND_B10_1", "^Б10\\.1\\."),
  why_not_working = c("IND_B17_1", "^Б17\\.1\\."), seeking = c("IND_B21", "^Б21"),
  reason_inactive = c("IND_B24", "^Б24\\."), care_children = c("IND_B28_1", "^Б28\\."),
  care_disabled = c("IND_B28_3", NA), care_elderly = c("IND_B28_4", NA),
  income_source = c("IND_V1_1", "^В1\\.1\\. "), income_amount = c("IND_V1_1_1", "^В1\\.1\\.1\\."),
  n_children = c("dity_n", "кількість дітей до 18"), hh_size = c("hh_size", "Фактичний розмір ДГ")
)
HSESS_HH <- list(
  key = c(NA, "унікальний ключ"), hh_income = c("V1", "загальний дохід"),
  disab_adult = c("D1_Mult3", NA), disab_child = c("D1_Mult4", NA), idp_hh = c("D1_Mult9", NA),
  veteran = c("D1_Mult10", NA), lost_home = c("G3_5_1", NA), damaged_home = c("G3_5_2", NA), wealth = c("V16", "^В16\\.")
)
for (k in names(HSESS_IND)) Encoding(HSESS_IND[[k]]) <- "UTF-8"
for (k in names(HSESS_HH))  Encoding(HSESS_HH[[k]])  <- "UTF-8"

# Frontline and conflict-affected oblasts (assumption: confirm with the UCO)
FRONTLINE <- c("Donetsk", "Kharkiv", "Zaporizhzhia", "Kherson", "Mykolaiv", "Sumy", "Chernihiv", "Dnipropetrovsk")

# ---- Key variables (shown on the slides and in the report) ---------------------
KEY_VARS <- tribble(
  ~concept, ~use, ~lfs, ~hsess,
  "Age", "Groups 15-18, 19-24; 15-29 for NEET", "age_gr (bands: 15-24, 25-29)", "IND_A2 age in years",
  "Sex", "Breakdown by sex", "sex", "IND_A3 sex",
  "Survey weight / design", "Weighted estimates", "weight_year; kod_res", "Final weight; household key",
  "Region", "Regional breakdown", "rg", "OBLAST",
  "Urban / rural", "Urban/rural breakdown", "tp_ar", "SETTL_TYPE",
  "Displacement", "Breakdown by displacement status", "not available", "IND_A19 IDP status",
  "Disability", "Breakdown by disability in household", "not available", "D1_Mult3/4 household member with disability",
  "Marital status", "Household formation", "marital_st", "IND_A6 marital status",
  "Motherhood", "Breakdown by motherhood", "not available", "Lives with own child under 18 (IND_A1 relationship, roster ages)",
  "Young children", "Childcare responsibilities", "not available", "Child under 6 in household (roster ages)",
  "Daily childcare", "Childcare responsibilities", "reason_inactiv = 3 (proxy)", "IND_B28_1 cares for own children daily",
  "Childcare access", "Preschool attendance", "not available", "IND_A14 why a child does not attend preschool",
  "Household income", "Income breakdown, costing", "not available", "V1 household income per member",
  "Education level", "Education-to-work transition", "education; special_dipl", "IND_A7 education level",
  "In education", "Enrolment, NEET", "reason_inactiv = 2", "IND_A9 currently in education; IND_A10 type of institution",
  "Employment", "Participation, employment, NEET", "labour_force", "IND_B1 status in reference week",
  "Job search", "Unemployment", "labour_force = 2; durat_job_search", "IND_B21 looking for work",
  "Reason not working", "Reasons for inactivity", "reason_inactiv; reason_unempl", "IND_B24 reason not working or seeking",
  "Occupation / sector", "Occupational distribution, earnings", "occupation", "IND_B6_2 occupation; IND_B5_2 sector",
  "Job quality", "Informality, hours", "informal_empl; actual_duration", "IND_B3_1 contract; IND_B9_1 hours; IND_B10_1",
  "Earnings", "Wage differences, costing", "not available", "IND_V1_1_1 monthly amount (wages)"
)
