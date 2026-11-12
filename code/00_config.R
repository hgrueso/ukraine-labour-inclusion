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
  # SSSU Labour Force Survey 2021, anonymised public-use file (reference year 2021)
  lfs      = find1("data/mic_poc_rob_syly_21", "\\.sav$"),
  lfs_xlsx = find1("data/mic_poc_rob_syly_21", "^Ukr_2021\\.xlsx$"),
  # UNICEF Household Social and Economic Status Survey (HSESS), Dec 2023 to Feb 2024
  mics7_dir = here::here("data", "mics7"),   # MICS 2025/26 SPSS files (hh, hl, wm, bh, ch, fs)
  hsess_hh  = find1("data", "SESH .*Household.*\\.xlsx$"),
  hsess_ind = find1("data", "SESHS IND.*\\.xlsx$")
)

# Age groups: 15-17 education, training and apprenticeships (14-17 are legal minors);
# 18-24 labour market (participation, employment); 15-24 education-to-work transition;
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
  ~concept, ~use, ~mics, ~hsess, ~lfs,
  "Age", "Groups 15-17, 18-24; 15-29 for NEET", "HL6; WB4 (women)", "IND_A2 age in years", "age_gr (bands)",
  "Sex", "Breakdown by sex", "HL4 (household members)", "IND_A3", "sex",
  "Survey design", "Weighted estimates", "hhweight, wmweight, fsweight; psu; stratum", "Final weight; household key", "weight_year; kod_res",
  "Region / area", "Regional and urban/rural breakdown", "HH7 oblast; macroregion; HH6", "OBLAST; SETTL_TYPE", "rg; tp_ar",
  "Displacement", "Breakdown by displacement", "WB10A IDP status; PE3 household moved", "IND_A19 IDP status", "not available",
  "Disability", "Breakdown by disability", "disability (functioning, 18-49)", "D1_Mult3/4 household member", "not available",
  "Marital status", "Household formation", "MSTATUS", "IND_A6", "marital_st",
  "Motherhood", "Breakdown by motherhood", "CEB children ever born; WDOBFC", "Own child under 18 (roster)", "not available",
  "Care work", "Care responsibilities", "CL11E/F caring for children or the sick (5-17)", "IND_B28_1 daily care of own children", "reason_inactiv = 3 (proxy)",
  "Childcare access", "Preschool attendance", "ED9, ED10A (ages 3-5)", "IND_A14 reason not attending", "not available",
  "Economic status", "Breakdown by income or wealth", "windex5 wealth quintile; PE20 income", "V1 household income per member", "not available",
  "Changes since 2022", "Household context", "PE3 moved; PE5 dwelling damaged; PE12 members separated", "not available", "not applicable",
  "Education level", "Education-to-work transition", "welevel; ED10A level attended", "IND_A7", "education; special_dipl",
  "In education", "School attendance, NEET", "ED9 (members 3-24); NE3 (women 15-24)", "IND_A9; IND_A10 type", "reason_inactiv = 2",
  "Training", "NEET", "NE6 courses or training (women 15-24)", "not available", "not available",
  "Employment", "Employment, NEET", "NE8-NE11, NE20-NE21 (women 15-24)", "IND_B1", "labour_force",
  "Job search", "Unemployment, participation", "not available", "IND_B21", "labour_force = 2",
  "Reason not working", "Reasons for inactivity", "not available", "IND_B24", "reason_inactiv",
  "Occupation / sector", "Occupational distribution", "not available", "IND_B6_2; IND_B5_2", "occupation",
  "Job quality", "Informality, hours", "not available", "IND_B3_1; IND_B9_1; IND_B10_1", "informal_empl; actual_duration",
  "Earnings", "Wage differences, costing", "not available", "IND_V1_1_1 (wages)", "not available"
)

# ---- MICS 2025/26 ----------------------------------------------------------------
MICS <- list(dir = here::here("data", "mics7"))
MICS$files <- c(hh = "hh.sav", hl = "hl.sav", wm = "wm.sav", fs = "fs.sav")
# Education level codes (ED10A / ED5A)
MICS_LEVEL <- c("0" = "Preschool", "11" = "General secondary", "21" = "General secondary", "31" = "General secondary",
                "41" = "Vocational", "51" = "Higher education", "61" = "Higher education", "71" = "Higher education", "81" = "Higher education")
