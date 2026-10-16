# 01_clean_lfs.R: SSSU Labour Force Survey 2021 -> analysis-ready file -------
# Codes verified against the SSSU codebook. Rotation panel: a person appears up
# to four times in the year; all interviews are kept (weight_year is built for
# annual averages) and standard errors are clustered by person (kod_res).
# Output: output/lfs_analysis_ready.rds
source(here::here("code", "00_config.R"))
suppressPackageStartupMessages(library(haven))

if (is.na(PATHS$lfs)) stop("LFS .sav not found in data/mic_poc_rob_syly_21")
raw <- haven::read_sav(PATHS$lfs) %>% haven::zap_labels()
names(raw) <- gsub("\\s+", "", names(raw))          # "durat_ job_search" -> "durat_job_search"
num <- function(x) suppressWarnings(as.numeric(x))
lab <- function(x, v) factor(unname(LFS_LABELS[[v]][as.character(x)]), levels = unname(LFS_LABELS[[v]]))

lfs <- raw %>% transmute(
  kod_res, w = num(weight_year),
  region = factor(unname(REGION_LABELS[as.character(rg)])),
  rural  = as.integer(num(tp_ar) == 2), area = lab(tp_ar, "tp_ar"),
  age_band = lab(age_gr, "age_gr"),
  female = as.integer(num(sex) == 1),
  sexo   = factor(ifelse(female == 1, "Young women", "Young men"), levels = c("Young women", "Young men")),
  marital = lab(marital_st, "marital_st"), married = as.integer(num(marital_st) == 1),
  education = lab(education, "education"),
  educ_higher = as.integer(num(education) == 1), educ_vet = as.integer(num(education) == 2),
  diploma = lab(special_dipl, "special_dipl"),
  lf = lab(labour_force, "labour_force"),
  in_lf = as.integer(num(labour_force) %in% 1:2),
  employed = as.integer(num(labour_force) == 1),
  unemployed = as.integer(num(labour_force) == 2),
  inactive = as.integer(num(labour_force) == 3),
  unemp_rate = ifelse(in_lf == 1, unemployed, NA_integer_),
  emp_status = lab(stat_empl, "stat_empl"),
  occupation = lab(occupation, "occupation"), occ_code = num(occupation),
  informal = ifelse(employed == 1, as.integer(num(informal_empl) == 1), NA_integer_),
  short_hours = ifelse(employed == 1, as.integer(num(actual_duration) %in% 1:2), NA_integer_),
  hours = lab(actual_duration, "actual_duration"),
  reason_unemp = lab(reason_unempl, "reason_unempl"),
  not_placed = ifelse(unemployed == 1, as.integer(num(reason_unempl) == 4), NA_integer_),
  search_dur = lab(durat_job_search, "durat_job_search"),
  long_unemp = ifelse(unemployed == 1, as.integer(num(durat_job_search) == 4), NA_integer_),
  job_search = lab(way_job_search, "way_job_search"),
  reason_inact = lab(reason_inactiv, "reason_inactiv"),
  student = as.integer(inactive == 1 & num(reason_inactiv) %in% 2),
  home_duties = ifelse(inactive == 1, as.integer(num(reason_inactiv) == 3), NA_integer_),
  discouraged = ifelse(inactive == 1, as.integer(num(reason_inactiv) == 4), NA_integer_),
  # NEET: not employed and not an inactive student. Unemployed students cannot be
  # identified in the public file, so they count as NEET (upper bound for the unemployed).
  neet = as.integer(employed == 0 & student == 0)
)
attr(lfs, "vintage") <- "SSSU Labour Force Survey 2021 (public-use file), weighted, SE clustered by person"
saveRDS(lfs, here::here("output", "lfs_analysis_ready.rds"))

y <- lfs %>% filter(age_band == "15-24")
cat(sprintf("LFS 2021: %s interviews, %s persons | 15-24: %s interviews (%s women)\n",
            format(nrow(lfs), big.mark = ","), format(dplyr::n_distinct(lfs$kod_res), big.mark = ","),
            format(nrow(y), big.mark = ","), format(sum(y$female == 1), big.mark = ",")))
cat(sprintf("Weighted NEET 15-24: women %.1f%%, men %.1f%%\n",
            100 * weighted.mean(y$neet[y$female == 1], y$w[y$female == 1]),
            100 * weighted.mean(y$neet[y$female == 0], y$w[y$female == 0])))
