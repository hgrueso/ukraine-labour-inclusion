# 03_regressions.R: basic hypothesis tests, LFS 2021 ------------------------
# Linear probability models, weight_year, oblast fixed effects, SE clustered by
# person. Controls: urban/rural, education, marital status (unless interacted).
# Output: output/models/lfs_tests.csv, output/figures/f8_coefficients_en.png
source(here::here("code", "00_config.R"))
suppressPackageStartupMessages({ library(fixest); library(purrr) })

lfs <- readRDS(here::here("output", "lfs_analysis_ready.rds"))
y1524 <- lfs %>% filter(age_band == "15-24")
y1529 <- lfs %>% filter(age_band %in% c("15-24", "25-29")) %>% mutate(age2529 = as.integer(age_band == "25-29"))
CTRL <- "rural + educ_higher + educ_vet + married"

test <- function(hyp, sample, data, y, term, rhs = paste("female +", CTRL)) {
  m <- feols(as.formula(paste(y, "~", rhs, "| region")), data = data, weights = ~w, cluster = ~kod_res)
  ct <- coeftable(m)
  tibble(Estimate = hyp, Sample = sample, `Effect (pp)` = round(100 * ct[term, 1], 1),
         SE = round(100 * ct[term, 2], 1), p = signif(ct[term, 4], 2), N = nobs(m))
}
tests <- bind_rows(
  test("NEET: women vs men", "15-24", y1524, "neet", "female"),
  test("NEET: women vs men", "15-29 (ILO frame)", y1529, "neet", "female", paste("female + age2529 +", CTRL)),
  test("Labour-force participation: women vs men", "15-24", y1524, "in_lf", "female"),
  test("Employment: women vs men", "15-24", y1524, "employed", "female"),
  test("NEET: woman x married", "15-24", y1524, "neet", "female:married",
       "female * married + rural + educ_higher + educ_vet"),
  test("NEET: woman x rural", "15-24", y1524, "neet", "female:rural",
       "female * rural + educ_higher + educ_vet + married"),
  test("NEET: woman x higher education", "15-24", y1524, "neet", "female:educ_higher",
       "female * educ_higher + rural + educ_vet + married"),
  test("Informal employment: women vs men", "15-24 employed", filter(y1524, employed == 1), "informal", "female"),
  test("Home duties as reason for inactivity: women vs men", "15-24 inactive", filter(y1524, inactive == 1), "home_duties", "female")
)
write_table(tests, "lfs_tests", sub = "models"); print(tests, n = Inf, width = Inf)

# coefficient plot: full model for NEET and labour-force participation, 15-24
full <- function(y) feols(as.formula(paste(y, "~ female * married + female * rural + female * educ_higher + educ_vet | region")),
                          data = y1524, weights = ~w, cluster = ~kod_res)
lab <- c(female = "Woman", married = "Married", rural = "Rural", educ_higher = "Higher education", educ_vet = "Vocational",
         `female:married` = "Woman x Married", `female:rural` = "Woman x Rural", `female:educ_higher` = "Woman x Higher education")
cf <- bind_rows(lapply(c("neet", "in_lf"), function(y) {
  m <- full(y); ct <- coeftable(m); ci <- confint(m)
  tibble(model = ifelse(y == "neet", "NEET", "Labour-force participation"), term = rownames(ct),
         estimate = 100 * ct[, 1], lo = 100 * ci[, 1], hi = 100 * ci[, 2])
})) %>% filter(term %in% names(lab)) %>% mutate(term = factor(lab[term], levels = rev(unname(lab))))
write_table(cf, "lfs_coefficients", sub = "models")
save_fig(ggplot(cf, aes(estimate, term, colour = model)) +
           geom_vline(xintercept = 0, linetype = 2, colour = GREY_MID) +
           geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(.5)) +
           scale_colour_manual(values = c(NEET = ACCENT_GIRL, `Labour-force participation` = UNICEF_DARK)) +
           labs(subtitle = "Linear probability models, 15-24, percentage points (95% CI)", x = NULL, y = NULL,
                caption = "SSSU Labour Force Survey 2021. Oblast fixed effects, weighted, SE clustered by person.") +
           theme_ukr() + theme(panel.grid.major.x = element_line(colour = "grey92"), panel.grid.major.y = element_blank()),
         "f8_coefficients_en", width = 9, height = 5)

# ---- HSESS 2023/24: who is NEET in wartime? ------------------------------------------
hp <- here::here("output", "hsess_analysis_ready.rds")
if (file.exists(hp)) {
  h <- readRDS(hp) %>% filter(age >= 15, age <= 24, !is.na(sexo), !is.na(neet), !is.na(own_child), w > 0) %>%
    mutate(age1924 = as.integer(age >= 19), higher = as.integer(educ %in% c("Higher (bachelor+)", "Short-cycle tertiary")))
  HC <- "age1924 + rural + idp + disab_hh + bottom40 + own_child + partnered + higher"
  htest <- function(hyp, y, term, rhs) {
    m <- feols(as.formula(paste(y, "~", rhs, "| oblast")), data = h, weights = ~w, cluster = ~key)
    ct <- coeftable(m)
    if (!term %in% rownames(ct)) return(NULL)
    tibble(Estimate = hyp, Sample = "15-24", `Effect (pp)` = round(100 * ct[term, 1], 1),
           SE = round(100 * ct[term, 2], 1), p = signif(ct[term, 4], 2), N = nobs(m))
  }
  ht <- bind_rows(
    htest("NEET: women vs men", "neet", "female", paste("female +", HC)),
    htest("NEET: woman x lives with own child", "neet", "female:own_child",
          paste("female * own_child +", sub("\\+ own_child", "", HC))),
    htest("NEET: woman x married or cohabiting", "neet", "female:partnered",
          paste("female * partnered +", sub("\\+ partnered", "", HC))),
    htest("NEET: internally displaced", "neet", "idp", paste("female +", HC)),
    htest("NEET: person with disability in household", "neet", "disab_hh", paste("female +", HC)),
    htest("NEET: poorest 40% of households", "neet", "bottom40", paste("female +", HC)),
    htest("NEET: woman x poorest 40%", "neet", "female:bottom40",
          paste("female * bottom40 +", sub("\\+ bottom40", "", HC))),
    htest("NEET: rural", "neet", "rural", paste("female +", HC))
  )
  write_table(ht, "hsess_tests", sub = "models"); print(ht, n = Inf, width = Inf)

  hm <- feols(as.formula(paste("neet ~ female * own_child + female * partnered +", sub("\\+ own_child", "", sub("\\+ partnered", "", HC)), "| oblast")),
              data = h, weights = ~w, cluster = ~key)
  hl <- c(female = "Woman", own_child = "Lives with own child", partnered = "Married or cohabiting",
          `female:own_child` = "Woman x lives with own child", `female:partnered` = "Woman x partnered", idp = "Internally displaced",
          disab_hh = "Disability in household", bottom40 = "Poorest 40%", rural = "Rural", higher = "Tertiary education", age1924 = "Aged 19-24")
  ct <- coeftable(hm); ci <- confint(hm)
  hc <- tibble(term = rownames(ct), estimate = 100 * ct[, 1], lo = 100 * ci[, 1], hi = 100 * ci[, 2]) %>%
    filter(term %in% names(hl)) %>% mutate(term = factor(hl[term], levels = rev(unname(hl))))
  write_table(hc, "hsess_coefficients", sub = "models")
  save_fig(ggplot(hc, aes(estimate, term)) + geom_vline(xintercept = 0, linetype = 2, colour = GREY_MID) +
             geom_pointrange(aes(xmin = lo, xmax = hi), colour = ACCENT_GIRL) +
             labs(subtitle = "Linear probability model of NEET, 15-24, percentage points (95% CI)", x = NULL, y = NULL,
                  caption = "UNICEF HSESS 2023/24. Oblast fixed effects, weighted, SE clustered by household.") +
             theme_ukr() + theme(panel.grid.major.x = element_line(colour = "grey92"), panel.grid.major.y = element_blank()),
           "h11_coefficients_en", width = 9, height = 5)
}
