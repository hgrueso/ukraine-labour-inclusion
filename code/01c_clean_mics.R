# 01c_clean_mics.R: MICS 2025/26 -> analysis-ready files ---------------------
# Files: hh (households), hl (household members), wm (women 15-49), fs (children 5-17).
# There is no men's questionnaire: NEET and employment are for women 15-24;
# school attendance (hl) is available for both sexes.
# Output: output/mics_members.rds, output/mics_women.rds, output/mics_fs.rds, output/mics_hh.rds
source(here::here("code", "00_config.R"))
suppressPackageStartupMessages(library(haven))

rd <- function(f) {
  p <- file.path(MICS$dir, f); if (!file.exists(p)) stop("MICS file not found: ", p)
  haven::read_sav(p) %>% haven::zap_labels() %>% haven::zap_formats()
}
lab_of <- function(f, v) { x <- haven::read_sav(file.path(MICS$dir, f), col_select = all_of(v))[[1]]
  l <- attr(x, "labels"); if (is.null(l)) NULL else setNames(names(l), l) }
yes <- function(x) ifelse(x %in% 1, 1L, ifelse(x %in% 2, 0L, NA_integer_))

# ---- households: changes since the beginning of the conflict ------------------------
hh <- rd(MICS$files["hh"]) %>% transmute(
  HH1, HH2, hhweight, psu, stratum, urban_rural = HH6,
  hh_moved = yes(PE3), hh_damaged = yes(PE5), hh_separated = yes(PE12),
  hh_income_down = ifelse(PE20 %in% c(1, 2, 3), as.integer(PE20 == 2), NA_integer_),
  hh_lost_job = ifelse(PE20 %in% 2, as.integer(!is.na(PE21A) & trimws(as.character(PE21A)) %in% c("A", "1")), NA_integer_),
  hh_complete = as.integer(!is.na(PE3)))
region_lab <- lab_of(MICS$files["hl"], "HH7"); macro_lab <- lab_of(MICS$files["hl"], "macroregion")

# ---- household members: school and preschool attendance ------------------------------
hl_raw <- rd(MICS$files["hl"])
if (!"HL3" %in% names(hl_raw)) hl_raw$HL3 <- NA_real_
hh_comp <- hl_raw %>% group_by(HH1, HH2) %>%
  summarise(n_adults = sum(HL6 >= 18, na.rm = TRUE), n_children = sum(HL6 < 18, na.rm = TRUE),
            spouse_present = as.integer(any(HL3 %in% 2)), .groups = "drop") %>%
  mutate(hh_children = as.integer(n_children > 0))
has_hl3 <- any(!is.na(hl_raw$HL3))        # HL3: relationship to the household head (2 = spouse)
hh_comp <- hh_comp %>% mutate(single_head = as.integer(n_children >= 1 & (if (has_hl3) spouse_present == 0 else n_adults == 1)))
cat("Single-headed households defined by", if (has_hl3) "head without spouse in the roster" else "one adult only (HL3 not available)", "\n")
hl <- hl_raw %>% transmute(
  HH1, HH2, HL1, hhweight, psu, stratum,
  sexo = factor(ifelse(HL4 == 2, "Young women", "Young men"), levels = c("Young women", "Young men")),
  age = HL6,
  region = unname(region_lab[as.character(HH7)]), macroregion = unname(macro_lab[as.character(macroregion)]),
  wealth_q = windex5, bottom40 = as.integer(windex5 %in% 1:2), bottom20 = as.integer(windex5 %in% 1),
  attend = case_when(age < 3 | age > 24 ~ NA_integer_, ED4 %in% 2 ~ 0L, ED9 %in% 1 ~ 1L, ED9 %in% 2 ~ 0L, TRUE ~ NA_integer_),
  level = ifelse(attend %in% 1, unname(MICS_LEVEL[as.character(ED10A)]), NA_character_),
  age_group = cut(age, c(-Inf, 2, 5, 14, 17, 24, Inf), labels = c("0-2", "3-5", "6-14", "15-17", "18-24", "25+"))) %>%
  mutate(level = ifelse(attend %in% 1 & is.na(level), "Other", level),
         in_gensec = ifelse(is.na(attend), NA_integer_, as.integer(level %in% "General secondary")),
         in_vocational = ifelse(is.na(attend), NA_integer_, as.integer(level %in% "Vocational")),
         in_higher = ifelse(is.na(attend), NA_integer_, as.integer(level %in% "Higher education")),
         in_other_educ = ifelse(is.na(attend), NA_integer_, as.integer(level %in% c("Other", "Preschool") & age >= 6)),
         preschool = ifelse(age >= 3 & age <= 5 & !is.na(attend), as.integer(attend == 1), NA_integer_)) %>%
  left_join(hh %>% select(HH1, HH2, urban_rural, starts_with("hh_")), by = c("HH1", "HH2")) %>%
  left_join(hh_comp %>% select(HH1, HH2, n_adults, n_children, hh_children, single_head), by = c("HH1", "HH2")) %>%
  mutate(rural = as.integer(urban_rural == 2), child_poor = ifelse(age < 18, bottom20, NA_integer_))
cat("Education level codes among those attending (15-24):\n")
print(table(rd(MICS$files["hl"]) %>% filter(HL6 >= 15, HL6 <= 24, ED9 == 1) %>% pull(ED10A), useNA = "ifany"))

# ---- women 15-49: NEET module (women 15-24), marriage, motherhood -------------------------
wm0 <- rd(MICS$files["wm"])
wm <- wm0 %>% filter(WM17 == 1) %>% transmute(
  HH1, HH2, LN, wmweight, psu, stratum, age = WB4,
  rural = as.integer(HH6 == 2), region = unname(region_lab[as.character(HH7)]),
  macroregion = unname(macro_lab[as.character(macroregion)]),
  wealth_q = windex5, bottom40 = as.integer(windex5 %in% 1:2), bottom20 = as.integer(windex5 %in% 1),
  educ = c("0" = "Primary or less", "1" = "Lower secondary", "2" = "Upper secondary", "3" = "Vocational", "4" = "Higher")[as.character(welevel)],
  married = ifelse(MSTATUS %in% 1:3, as.integer(MSTATUS == 1), NA_integer_),
  mother = ifelse(is.na(CEB), NA_integer_, as.integer(CEB > 0)),
  disability = ifelse(age >= 18, ifelse(disability %in% 1, 1L, ifelse(disability %in% 2, 0L, NA_integer_)), NA_integer_),
  idp = case_when(WB10A %in% 1 ~ 1L, WB10A %in% 2 | WB8 %in% 95 ~ 0L, !is.na(WB8) ~ 0L, TRUE ~ NA_integer_),
  age_union = WAGEM, age_first_birth = ifelse(!is.na(WDOBFC) & !is.na(WDOB), floor((WDOBFC - WDOB) / 12), NA_real_),
  ne_school = yes(NE3), ne_training = yes(NE6),
  ne_paid = NE8 %in% 1, ne_business = NE9 %in% 1 & (NE18X %in% 1 | NE21 %in% 1:2), ne_family = NE10 %in% 1,
  ne_absent = NE11 %in% 1 & !(NE13 %in% 1), ne_farm_sale = NE20 %in% 1 & NE21 %in% 1:2,
  ne_answered = !is.na(NE3) | !is.na(NE4) | !is.na(NE8)) %>%
  left_join(hl %>% select(HH1, HH2, HL1, attend_hl = attend), by = c("HH1", "HH2", "LN" = "HL1")) %>%
  left_join(hh %>% select(HH1, HH2, starts_with("hh_")), by = c("HH1", "HH2")) %>%
  left_join(hh_comp %>% select(HH1, HH2, n_adults, n_children, hh_children, single_head), by = c("HH1", "HH2")) %>%
  mutate(
    poor_with_child = as.integer(bottom40 == 1 & mother %in% 1),
    single_mother = as.integer(mother %in% 1 & single_head %in% 1),
    age_group = cut(age, c(-Inf, 14, 17, 24, Inf), labels = c("<15", "15-17", "18-24", "25-49")),
    in_educ = ifelse(age <= 24, as.integer(ne_school %in% 1 | attend_hl %in% 1), NA_integer_),
    in_training = ifelse(age <= 24 & ne_answered, as.integer(ne_training %in% 1), NA_integer_),
    employed = ifelse(age <= 24 & ne_answered, as.integer(ne_paid | ne_business | ne_family | ne_absent | ne_farm_sale), NA_integer_),
    neet = case_when(age > 24 ~ NA_integer_, in_educ %in% 1 ~ 0L, !ne_answered ~ NA_integer_,
                     TRUE ~ as.integer(employed == 0 & in_training == 0)),
    union_u18 = ifelse(age >= 20 & age <= 24, as.integer(age_union %in% 0:17), NA_integer_),
    birth_u18 = ifelse(age >= 20 & age <= 24, as.integer(!is.na(age_first_birth) & age_first_birth < 18), NA_integer_),
    birth_u20 = ifelse(age >= 20 & age <= 24, as.integer(!is.na(age_first_birth) & age_first_birth < 20), NA_integer_))

y <- wm %>% filter(age >= 15, age <= 24)
cat("\nWomen 15-24 with completed interview:", nrow(y), "\n")
cat("NEET module answered:\n"); print(table(age_group = y$age_group, answered = y$ne_answered))
cat("Attending school (household roster) by NEET module answered:\n"); print(table(attend_hl = y$attend_hl, answered = y$ne_answered, useNA = "ifany"))
cat("NEET status (unweighted):\n"); print(table(y$neet, useNA = "ifany"))
cat(sprintf("Weighted, women 15-24: in education %.1f%%, employed %.1f%%, NEET %.1f%%\n",
            100 * weighted.mean(y$in_educ, y$wmweight, na.rm = TRUE), 100 * weighted.mean(y$employed, y$wmweight, na.rm = TRUE),
            100 * weighted.mean(y$neet, y$wmweight, na.rm = TRUE)))

# ---- children 5-17: household chores and care work --------------------------------------
fs <- rd(MICS$files["fs"]) %>% filter(FS17 == 1) %>% transmute(
  HH1, HH2, LN, fsweight, psu, stratum, age = CB3,
  sexo = factor(ifelse(HL4 == 2, "Girls", "Boys"), levels = c("Girls", "Boys")),
  bottom40 = as.integer(windex5 %in% 1:2),
  care_children = yes(CL11E), care_sick = yes(CL11F), cooking = yes(CL11B), washing = yes(CL11D),
  chores_hours = ifelse(CL13 >= 0 & CL13 < 90, CL13, NA_real_),
  age_group = cut(age, c(4, 11, 14, 17), labels = c("5-11", "12-14", "15-17"))) %>%
  left_join(hh %>% transmute(HH1, HH2, rural = as.integer(urban_rural == 2)), by = c("HH1", "HH2"))

saveRDS(hh, here::here("output", "mics_hh.rds")); saveRDS(hl, here::here("output", "mics_members.rds"))
saveRDS(wm, here::here("output", "mics_women.rds")); saveRDS(fs, here::here("output", "mics_fs.rds"))
cat(sprintf("\nSaved: %s households, %s members, %s women, %s children 5-17\n",
            format(nrow(hh), big.mark = ","), format(nrow(hl), big.mark = ","), format(nrow(wm), big.mark = ","), format(nrow(fs), big.mark = ",")))
