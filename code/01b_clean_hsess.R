# 01b_clean_hsess.R: UNICEF HSESS 2023/24 -> English analysis-ready file -----
# Individual file (all household members, roster) merged with the household file.
# Variable codes verified against the questionnaire (see 00_config.R, HSESS_IND/HH).
# Output: output/hsess_analysis_ready.rds, output/tables/hsess_variables_used.csv
source(here::here("code", "00_config.R"))

ind <- read_hsess(PATHS$hsess_ind)
hh  <- if (!is.na(PATHS$hsess_hh)) read_hsess(PATHS$hsess_hh) else NULL
ci  <- vapply(HSESS_IND, function(s) hcol(ind, s), "")
chh <- if (!is.null(hh)) vapply(HSESS_HH, function(s) hcol(hh, s), "") else character(0)
need <- c("key", "weight", "age", "sex", "in_educ", "econ_status")
if (any(is.na(ci[need]))) stop("Core HSESS variables not found: ", paste(need[is.na(ci[need])], collapse = ", "))
g  <- function(k) if (!is.na(ci[k])) ind[[ci[k]]] else rep(NA, nrow(ind))
gh <- function(k) if (length(chh) && !is.na(chh[k])) hh[[chh[k]]] else rep(NA, nrow(hh))

d <- tibble(
  key = as.character(g("key")), w = num(g("weight")),
  oblast = tr(g("oblast"), TR$oblast), rural = as.integer(g("settlement") == "rural"),
  rel = tr(g("relation"), TR$relation), age = num(g("age")),
  sexo = factor(tr(g("sex"), TR$sex), levels = c("Young women", "Young men")),
  marital = tr(g("marital"), TR$marital), educ = tr(g("educ_level"), TR$education),
  in_educ = yes_no(g("in_educ")), institution = tr(g("institution"), TR$institution),
  econ = tr(g("econ_status"), TR$econ_status),
  moved = g("moved"), idp_raw = yes_no(g("idp")),
  job_type = tr(g("job_type"), TR$job_type), sector = tr(g("sector"), TR$sector),
  job_title = tolower(trimws(as.character(g("job_title")))),
  occupation = tr(g("occupation"), TR$occupation), hours = num(g("hours")),
  more_hours = tr(g("attitude"), TR$attitude_more_hours),
  why_not_working = tr(g("why_not_working"), TR$why_not_working),
  seeking_raw = yes_no(g("seeking")),
  reason_inactive = tr(g("reason_inactive"), TR$reason_inactive),
  care_children = tr(g("care_children"), TR$frequency), care_disabled = tr(g("care_disabled"), TR$frequency),
  care_elderly = tr(g("care_elderly"), TR$frequency),
  income_source = tr(g("income_source"), TR$income_source), income_amount = num(g("income_amount")),
  preschool_reason = tr(g("preschool_reason"), TR$preschool_reason),
  hh_size = num(g("hh_size"))
)

# household roster: young children, and own children (relationships are coded to the respondent)
#  - respondent and spouse: own children = the respondent's children under 18
#  - respondent's child or child-in-law aged 15-49: own children = grandchildren under 18, assigned when
#    one person or one couple of that generation lives in the household; otherwise unknown (NA)
#  - other members: 0
d <- d %>% mutate(sex_raw = sexo) %>% group_by(key) %>%
  mutate(child_u6 = as.integer(any(age < 6, na.rm = TRUE)), child_u3 = as.integer(any(age < 3, na.rm = TRUE)),
         hh_size = coalesce(hh_size, as.numeric(n())),
         .kids = sum(rel %in% "Child" & age < 18, na.rm = TRUE),
         .grandkids = sum(rel %in% "Grandchild" & age < 18, na.rm = TRUE),
         .g2 = rel %in% c("Child", "Child-in-law") & age >= 15 & age <= 49,
         .g2_f = sum(.g2 & sex_raw %in% "Young women"), .g2_m = sum(.g2 & sex_raw %in% "Young men")) %>% ungroup() %>%
  mutate(own_child = case_when(
    is.na(age) | age < 15 ~ NA_integer_,
    rel %in% c("Respondent", "Spouse") ~ as.integer(.kids > 0),
    .g2 & .grandkids == 0 ~ 0L,
    .g2 & .g2_f <= 1 & .g2_m <= 1 ~ 1L,
    .g2 ~ NA_integer_,
    TRUE ~ 0L)) %>%
  select(-starts_with("."), -sex_raw)

# household file: income, disability, displacement, war damage
if (!is.null(hh) && !is.na(chh["key"])) {
  h <- tibble(key = as.character(gh("key")), hh_income = num(gh("hh_income")),
              disab_hh = as.integer(yes_no(gh("disab_adult")) %in% 1 | yes_no(gh("disab_child")) %in% 1),
              idp_hh = yes_no(gh("idp_hh")), veteran_hh = yes_no(gh("veteran")),
              war_damage = as.integer(yes_no(gh("lost_home")) %in% 1 | yes_no(gh("damaged_home")) %in% 1),
              wealth = tr(gh("wealth"), TR$wealth)) %>% distinct(key, .keep_all = TRUE)
  d <- left_join(d, h, by = "key")
}

d <- d %>% mutate(
  age_group = cut(age, c(-Inf, 14, 18, 24, 29, 64, Inf), labels = c("0-14", "15-18", "19-24", "25-29", "30-64", "65+")),
  female = as.integer(sexo == "Young women"),
  partnered = as.integer(marital %in% c("Married", "Cohabiting")),
  frontline = as.integer(oblast %in% FRONTLINE),
  employed = ifelse(is.na(econ), NA_integer_, as.integer(econ %in% c("Working", "Temporarily absent"))),
  neet = ifelse(is.na(employed) | is.na(in_educ), NA_integer_, as.integer(employed == 0 & in_educ == 0)),
  seeking = ifelse(employed == 0, as.integer(seeking_raw %in% 1), NA_integer_),
  in_lf = ifelse(is.na(employed), NA_integer_, as.integer(employed == 1 | seeking %in% 1)),
  idp = ifelse(idp_raw %in% 1, 1L, ifelse(!is.na(idp_raw) | !is.na(moved), 0L, NA_integer_)),
  care_daily = as.integer(care_children == "Daily"),
  in_gensec = ifelse(is.na(in_educ), NA_integer_, as.integer(in_educ == 1 & institution %in% "General secondary")),
  in_vocational = ifelse(is.na(in_educ), NA_integer_, as.integer(in_educ == 1 & institution %in% "Vocational")),
  in_higher = ifelse(is.na(in_educ), NA_integer_, as.integer(in_educ == 1 & institution %in% "Higher education")),
  in_other_educ = ifelse(is.na(in_educ), NA_integer_, as.integer(in_educ == 1 & !(institution %in% c("General secondary", "Vocational", "Higher education")))),
  care_any_daily = as.integer(care_children == "Daily" | care_disabled == "Daily" | care_elderly == "Daily"),
  informal = ifelse(employed == 1 & !is.na(job_type) & job_type != "Other",
                    as.integer(job_type %in% c("Employee, verbal agreement", "Own-account, unregistered", "Family farm for sale")), NA_integer_),
  short_hours = ifelse(employed == 1 & !is.na(hours), as.integer(hours < 30), NA_integer_),
  wants_more_hours = ifelse(employed == 1, as.integer(more_hours %in% "Wants more hours"), NA_integer_),
  wage = ifelse(income_source %in% "Wages" & income_amount > 0, income_amount, NA_real_),
  pc_income = ifelse(hh_income > 0, hh_income / hh_size, NA_real_)
) %>% select(-idp_raw, -seeking_raw, -moved)

# income quintiles of household income per member (weighted, all individuals)
qs <- d %>% filter(!is.na(pc_income), w > 0) %>% arrange(pc_income) %>%
  mutate(cw = cumsum(w) / sum(w)) %>% select(key, pc_income, cw)
br <- sapply(c(.2, .4, .6, .8), function(p) qs$pc_income[which(qs$cw >= p)[1]])
d <- d %>% mutate(income_q = cut(pc_income, c(-Inf, br, Inf), labels = paste0("Q", 1:5)),
                  bottom40 = as.integer(income_q %in% c("Q1", "Q2")))

attr(d, "vintage") <- "UNICEF HSESS 2023/24, weighted, 95% CIs clustered by household"
saveRDS(d, here::here("output", "hsess_analysis_ready.rds"))

used <- tibble(role = c(names(ci), names(chh)), code = c(unname(ci), unname(chh)),
               file = c(rep("individual", length(ci)), rep("household", length(chh))))
write_table(used, "hsess_variables_used")

y <- d %>% filter(age >= 19, age <= 24)
wm <- function(v, s) round(100 * weighted.mean(y[[v]][y$sexo == s], y$w[y$sexo == s], na.rm = TRUE), 1)
cat(sprintf("HSESS: %s individuals in %s households | 19-24: %s (women %s, men %s)\n",
            format(nrow(d), big.mark = ","), format(dplyr::n_distinct(d$key), big.mark = ","),
            format(nrow(y), big.mark = ","), sum(y$sexo == "Young women", na.rm = TRUE), sum(y$sexo == "Young men", na.rm = TRUE)))
cat(sprintf("19-24, weighted: NEET women %s%%, men %s%% | in education women %s%%, men %s%% | employed women %s%%, men %s%%\n",
            wm("neet", "Young women"), wm("neet", "Young men"), wm("in_educ", "Young women"), wm("in_educ", "Young men"),
            wm("employed", "Young women"), wm("employed", "Young men")))
w1924 <- d %>% filter(age >= 19, age <= 24, sexo == "Young women")
cat(sprintf("Women 19-24 living with own child: %s of %s (relationship unknown for %s); of these, %s%% report caring for own children daily or occasionally\n",
            sum(w1924$own_child %in% 1), nrow(w1924), sum(is.na(w1924$own_child)),
            round(100 * mean(w1924$care_children[w1924$own_child %in% 1] %in% c("Daily", "Occasionally")), 1)))
if (any(is.na(ci))) cat("Individual variables not found: ", paste(names(ci)[is.na(ci)], collapse = ", "), "\n")
if (length(chh) && any(is.na(chh))) cat("Household variables not found: ", paste(names(chh)[is.na(chh)], collapse = ", "), "\n")
