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

# ---- HSESS 2023/24: NEET model ------------------------------------------------------
hp <- here::here("output", "hsess_analysis_ready.rds")
if (file.exists(hp)) {
  h <- readRDS(hp) %>% filter(age >= 15, age <= 24, !is.na(sexo), !is.na(neet), !is.na(own_child), w > 0) %>%
    mutate(age1824 = as.integer(age >= 18), higher = as.integer(educ %in% c("Higher (bachelor+)", "Short-cycle tertiary")))
  HC <- "age1824 + rural + idp + disab_hh + bottom40 + own_child + partnered + higher"
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
          disab_hh = "Disability in household", bottom40 = "Poorest 40%", rural = "Rural", higher = "Tertiary education", age1824 = "Aged 18-24")
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

# ---- MICS 2025/26: NEET among women 15-24 ----------------------------------------------
mp <- here::here("output", "mics_women.rds")
if (file.exists(mp)) {
  mw <- readRDS(mp) %>% filter(age >= 15, age <= 24, !is.na(neet)) %>%
    mutate(age1824 = as.integer(age >= 18), educ_higher = as.integer(educ %in% "Higher"), educ_voc = as.integer(educ %in% "Vocational"))
  MC <- "age1824 + married + mother + idp + bottom40 + rural + educ_higher + educ_voc + hh_moved + hh_income_down"
  mtest <- function(lab, term, rhs = MC, data = mw, sample = "Women 15-24") {
    m <- feols(as.formula(paste("neet ~", rhs, "| region")), data = data, weights = ~wmweight, cluster = ~psu)
    ct <- coeftable(m); if (!term %in% rownames(ct)) return(NULL)
    tibble(Estimate = lab, Sample = sample, `Effect (pp)` = round(100 * ct[term, 1], 1), SE = round(100 * ct[term, 2], 1),
           p = signif(ct[term, 4], 2), N = nobs(m))
  }
  mt <- bind_rows(
    mtest("NEET: married or in union", "married"), mtest("NEET: mother", "mother"),
    mtest("NEET: married x mother", "married:mother", paste("married * mother +", sub("married \\+ mother \\+ ", "", MC))),
    mtest("NEET: internally displaced", "idp"), mtest("NEET: poorest 40% (wealth)", "bottom40"), mtest("NEET: rural", "rural"),
    mtest("NEET: household moved since 2022", "hh_moved"), mtest("NEET: household income decreased since 2022", "hh_income_down"),
    mtest("NEET: functional difficulty", "disability", paste("disability +", MC), filter(mw, age >= 18, !is.na(disability)), "Women 18-24"))
  write_table(mt, "mics_tests", sub = "models"); print(mt, n = Inf, width = Inf)
  mm <- feols(as.formula(paste("neet ~", MC, "| region")), data = mw, weights = ~wmweight, cluster = ~psu)
  ml <- c(age1824 = "Aged 18-24", married = "Married or in union", mother = "Mother", idp = "Internally displaced",
          bottom40 = "Poorest 40% (wealth)", rural = "Rural", educ_higher = "Higher education", educ_voc = "Vocational education",
          hh_moved = "Household moved since 2022", hh_income_down = "Household income decreased since 2022")
  ct <- coeftable(mm); ci <- confint(mm)
  mc <- tibble(term = rownames(ct), estimate = 100 * ct[, 1], lo = 100 * ci[, 1], hi = 100 * ci[, 2]) %>%
    filter(term %in% names(ml)) %>% mutate(term = factor(ml[term], levels = rev(unname(ml))))
  write_table(mc, "mics_coefficients", sub = "models")
  save_fig(ggplot(mc, aes(estimate, term)) + geom_vline(xintercept = 0, linetype = 2, colour = GREY_MID) +
             geom_pointrange(aes(xmin = lo, xmax = hi), colour = ACCENT_GIRL) +
             labs(subtitle = "Linear probability model of NEET, women 15-24, percentage points (95% CI)", x = NULL, y = NULL,
                  caption = "MICS 2025/26. Oblast fixed effects, weighted, SE clustered by primary sampling unit.") +
             theme_ukr() + theme(panel.grid.major.x = element_line(colour = "grey92"), panel.grid.major.y = element_blank()),
           "m11_coefficients_en", width = 9, height = 5)
}

# ---- Adjusted comparisons: observed rate for the "No" group, plus the model coefficient for "Yes" ----
adj_rates <- function(data, y, groups, rhs, w, cl, fe = "oblast") {
  m <- feols(as.formula(paste(y, "~", rhs, "|", fe)), data = data, weights = as.formula(paste0("~", w)), cluster = as.formula(paste0("~", cl)))
  ct <- coeftable(m); fv <- fitted(m); ww <- data[[w]][obs(m)]
  map_dfr(names(groups), function(g) {
    if (!g %in% rownames(ct) || !g %in% names(data)) return(NULL)
    gg <- data[[g]][obs(m)]
    # model-based average predictions: everyone set to g = 0, then everyone set to g = 1
    p0 <- weighted.mean(fv - ct[g, 1] * gg, ww); p1 <- p0 + ct[g, 1]
    tibble(dimension = groups[[g]], group = c("No", "Yes"), estimate = c(p0, p1),
           lo = c(NA, p1 - 1.96 * ct[g, 2]), hi = c(NA, p1 + 1.96 * ct[g, 2]),
           n_yes = sum(gg %in% 1), p = ct[g, 4])
  })
}
# Difference chart: adjusted difference (Yes minus No) with 95% CI, predicted rates in the labels
plot_diff <- function(df, sub, cap, fills = FILL_SEX, by_sex = TRUE) {
  dd <- df %>% group_by(dimension, sexo) %>%
    summarise(p0 = estimate[group == "No"], p1 = estimate[group == "Yes"], diff = p1 - p0, lo = lo[group == "Yes"] - p0, hi = hi[group == "Yes"] - p0,
              n_yes = first(n_yes), .groups = "drop") %>%
    mutate(dimension = factor(dimension, levels = unique(df$dimension)), lab = sprintf("%s to %s", percent(p0, accuracy = 1), percent(p1, accuracy = 1)))
  g <- if (by_sex) ggplot(dd, aes(dimension, diff, colour = sexo)) else ggplot(dd, aes(dimension, diff))
  g <- g + geom_hline(yintercept = 0, linetype = 2, colour = GREY_MID) +
    (if (by_sex) geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(.5), size = .5) else
       geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(.5), size = .5, colour = ACCENT_GIRL)) +
    geom_text(aes(y = hi, label = lab), position = position_dodge(.5), vjust = -0.7, size = 2.4, colour = GREY_DARK) +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), " pp"), expand = expansion(mult = c(.15, .25))) +
    scale_x_discrete(labels = label_wrap(14)) +
    labs(subtitle = sub, x = NULL, y = NULL, caption = cap) + theme_ukr() + theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = "grey92"))
  if (by_sex) g <- g + scale_colour_manual(values = fills)
  g
}

# ---- HSESS: adjusted NEET and employment by motherhood, partnership, displacement, disability, income ----
if (file.exists(hp)) {
  hall <- readRDS(hp) %>% filter(!is.na(sexo), !is.na(own_child), w > 0) %>%
    mutate(age1824 = as.integer(age >= 18), higher = as.integer(educ %in% c("Higher (bachelor+)", "Short-cycle tertiary")))
  HG <- c(own_child = "Lives with own child", partnered = "Married or cohabiting", idp = "Internally displaced",
          disab_hh = "Disability in household", bottom40 = "Poorest 40%")
  HR <- "own_child + partnered + idp + disab_hh + bottom40 + rural + higher"
  h17 <- map_dfr(c("Young women", "Young men"), function(s) adj_rates(hall %>% filter(sexo == s, age >= 15, age <= 24, !is.na(neet)),
                                                                 "neet", HG, paste("age1824 +", HR), "w", "key") %>% mutate(sexo = s)) %>%
    mutate(sexo = factor(sexo, levels = c("Young women", "Young men")))
  h18 <- map_dfr(c("Young women", "Young men"), function(s) adj_rates(hall %>% filter(sexo == s, age >= 18, age <= 24, !is.na(employed)),
                                                                 "employed", HG, paste("age +", HR), "w", "key") %>% mutate(sexo = s)) %>%
    mutate(sexo = factor(sexo, levels = c("Young women", "Young men")))
  write_table(h17, "h17_adjusted_neet"); write_table(h18, "h18_adjusted_employment")
  hcap <- "UNICEF HSESS 2023/24. Linear probability models with oblast fixed effects, estimated separately for women and men; predicted rates with everyone set to 'No' and then to 'Yes'; 95% CI on the difference."
  save_fig(plot_diff(h17, "Adjusted difference in the NEET rate at 15-24 (Yes minus No); labels show the predicted rates", hcap), "h17_adjusted_neet_en", width = 11, height = 5)
  save_fig(plot_diff(h18, "Adjusted difference in the employment rate at 18-24 (Yes minus No); labels show the predicted rates", hcap), "h18_adjusted_employment_en", width = 11, height = 5)

  # Mincer wage equation: returns to a year of education, wage earners 19-64
  wg <- readRDS(hp) %>% filter(!is.na(wage), wage > 0, !is.na(educ_years), !is.na(sexo), age >= 18, age <= 64, w > 0) %>%
    mutate(lnw = log(wage), age2 = age^2)
  mw_all <- feols(lnw ~ educ_years * female + age + age2 | oblast, data = wg, weights = ~w, cluster = ~key)
  mw_w <- feols(lnw ~ educ_years + age + age2 | oblast, data = filter(wg, female == 1), weights = ~w, cluster = ~key)
  mw_m <- feols(lnw ~ educ_years + age + age2 | oblast, data = filter(wg, female == 0), weights = ~w, cluster = ~key)
  ret <- function(m, lab) { ct <- coeftable(m); tibble(Sample = lab, `Return per year (%)` = round(100 * (exp(ct["educ_years", 1]) - 1), 1),
    lo = round(100 * (exp(ct["educ_years", 1] - 1.96 * ct["educ_years", 2]) - 1), 1), hi = round(100 * (exp(ct["educ_years", 1] + 1.96 * ct["educ_years", 2]) - 1), 1),
    p = signif(ct["educ_years", 4], 2), N = nobs(m)) }
  mincer <- bind_rows(ret(mw_w, "Women 19-64"), ret(mw_m, "Men 19-64"),
                      tibble(Sample = "Difference women vs men (interaction)", `Return per year (%)` = round(100 * coeftable(mw_all)["educ_years:female", 1], 1),
                             lo = NA, hi = NA, p = signif(coeftable(mw_all)["educ_years:female", 4], 2), N = nobs(mw_all)))
  gap_coef <- coeftable(mw_all)["female", 1]
  write_table(mincer, "hsess_mincer", sub = "models"); print(mincer, n = Inf, width = Inf)
  # predicted wage relative to 11 years of education, by sex
  pr <- map_dfr(list(list(mw_w, "Women"), list(mw_m, "Men")), function(x) { ct <- coeftable(x[[1]]); b <- ct["educ_years", 1]; se <- ct["educ_years", 2]
    tibble(sex = x[[2]], years = c(9, 11, 12, 13, 15, 17), rel = exp(b * (years - 11)), lo = exp((b - 1.96 * se) * (years - 11)), hi = exp((b + 1.96 * se) * (years - 11))) })
  pr <- pr %>% mutate(level = c(`9` = "Basic secondary", `11` = "Complete secondary", `12` = "Vocational", `13` = "Short-cycle tertiary", `15` = "Bachelor", `17` = "Master")[as.character(years)])
  write_table(pr, "h16_predicted_wage_by_education")
  save_fig(ggplot(pr, aes(years, rel, colour = sex, fill = sex)) + geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .12, colour = NA) + geom_line(linewidth = 1) + geom_point(size = 2.2) +
             geom_text(data = pr %>% filter(sex == "Women"), aes(label = level), vjust = -1.4, size = 2.8, colour = GREY_DARK, show.legend = FALSE) +
             scale_colour_manual(values = c(Women = ACCENT_GIRL, Men = ACCENT_BOY)) + scale_fill_manual(values = c(Women = ACCENT_GIRL, Men = ACCENT_BOY)) +
             scale_y_continuous(labels = percent_format(1)) + scale_x_continuous(breaks = c(9, 11, 12, 13, 15, 17)) +
             labs(subtitle = "Predicted monthly wage by years of education, relative to complete secondary (11 years), by sex", x = "Years of education", y = NULL,
                  caption = "UNICEF HSESS 2023/24, wage earners 19-64. Log wage on years of education, age and age squared, oblast fixed effects, weighted, SE clustered by household.") +
             theme_ukr() + theme(panel.grid.major.x = element_line(colour = "grey92")), "h16_predicted_wage_by_education_en", width = 10, height = 5.2)
  # predicted wage by education level and sex, controlling for age, age squared, area and oblast
  wg2 <- wg %>% filter(!is.na(educ), !is.na(rural)) %>% mutate(educ = droplevels(educ))
  me <- feols(lnw ~ educ * female + age + age2 + rural + i(oblast), data = wg2, weights = ~w, cluster = ~key)
  mode_obl <- names(sort(table(wg2$oblast), decreasing = TRUE))[1]
  nd <- tidyr::expand_grid(educ = factor(levels(wg2$educ), levels = levels(wg2$educ)), female = 0:1) %>%
    mutate(age = 30, age2 = 900, rural = 0, oblast = mode_obl)
  pe <- predict(me, newdata = nd, se.fit = TRUE)
  h20 <- nd %>% mutate(fit = exp(pe$fit), lo = exp(pe$fit - 1.96 * pe$se.fit), hi = exp(pe$fit + 1.96 * pe$se.fit),
                       sex = factor(ifelse(female == 1, "Women", "Men"), levels = c("Women", "Men")))
  n20 <- wg2 %>% count(educ, sex = ifelse(female == 1, "Women", "Men"))
  h20 <- h20 %>% left_join(n20, by = c("educ", "sex"))
  write_table(h20, "h20_mincer_by_education")
  gap20 <- h20 %>% select(educ, sex, fit) %>% tidyr::pivot_wider(names_from = sex, values_from = fit) %>% mutate(gap = 100 * (1 - Women / Men))
  save_fig(ggplot(h20, aes(educ, fit, colour = sex, group = sex)) +
             geom_ribbon(aes(ymin = lo, ymax = hi, fill = sex), alpha = .12, colour = NA) + geom_line(linewidth = 1) + geom_point(size = 2.4) +
             geom_text(aes(label = format(round(fit), big.mark = ",")), vjust = -1.1, size = 2.8, show.legend = FALSE) +
             scale_colour_manual(values = c(Women = ACCENT_GIRL, Men = ACCENT_BOY)) + scale_fill_manual(values = c(Women = ACCENT_GIRL, Men = ACCENT_BOY)) +
             scale_x_discrete(labels = label_wrap(12)) + scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(.1, .2))) +
             labs(subtitle = "Predicted monthly net wage (UAH) by education level and sex, at age 30, urban; 95% CI", x = NULL, y = NULL,
                  caption = "UNICEF HSESS 2023/24, wage earners 19-64. Log wage on education level interacted with sex, age, age squared, area and oblast; weighted, SE clustered by household.") +
             theme_ukr() + theme(panel.grid.major.x = element_line(colour = "grey92")), "h20_mincer_by_education_en", width = 10, height = 5.2)
  write_table(gap20, "h20_adjusted_gap_by_education")
  write_table(tibble(key = c("ret_w", "ret_m", "wage_gap_adj", "gap_sec_adj", "gap_higher_adj"),
                     value = c(mincer$`Return per year (%)`[1], mincer$`Return per year (%)`[2], round(100 * (1 - exp(gap_coef)), 1),
                               round(gap20$gap[gap20$educ == "Complete secondary"][1], 1), round(gap20$gap[gap20$educ == "Higher (bachelor+)"][1], 1))),
              "hsess_model_headlines")
}

# ---- MICS: adjusted NEET for women 15-24 ----------------------------------------------------
if (file.exists(mp)) {
  MG <- c(mother = "Mother", married = "Married or in union", idp = "Internally displaced", bottom40 = "Poorest 40% (wealth)", hh_moved = "Household moved since 2022")
  m13 <- adj_rates(mw, "neet", MG, MC, "wmweight", "psu", fe = "region") %>% mutate(sexo = "Young women")
  write_table(m13, "m13_adjusted_neet")
  save_fig(plot_diff(m13, "Adjusted difference in the NEET rate among women 15-24 (Yes minus No); labels show the predicted rates",
                     "MICS 2025/26. Linear probability model with oblast fixed effects; predicted rates with everyone set to 'No' and then to 'Yes'; 95% CI on the difference.", by_sex = FALSE),
           "m13_adjusted_neet_en", width = 11, height = 5)
}

# ---- Slide 5: adjusted NEET by motherhood and partnership, MICS and HSESS side by side (women) ----
if (exists("m13") && exists("h17")) {
  s5 <- bind_rows(m13 %>% filter(dimension %in% c("Mother", "Married or in union")) %>% mutate(source = "MICS 2025/26, women 15-24"),
                  h17 %>% filter(sexo == "Young women", dimension %in% c("Lives with own child", "Married or cohabiting")) %>%
                    mutate(source = "HSESS 2023/24, women 15-24", dimension = ifelse(dimension == "Lives with own child", "Mother", "Married or cohabiting"))) %>%
    mutate(group = factor(group, levels = c("No", "Yes")), dimension = factor(dimension, levels = c("Mother", "Married or in union", "Married or cohabiting")),
           source = factor(source, levels = c("MICS 2025/26, women 15-24", "HSESS 2023/24, women 15-24")))
  write_table(s5, "s5_adjusted_motherhood_neet")
  s5d <- s5 %>% group_by(source, dimension) %>%
    summarise(p0 = estimate[group == "No"], p1 = estimate[group == "Yes"], diff = p1 - p0, lo = lo[group == "Yes"] - p0, hi = hi[group == "Yes"] - p0, .groups = "drop")
  write_table(s5d, "s5_adjusted_motherhood_neet_differences")
  s5r <- s5 %>% mutate(panel = "Adjusted NEET rate")
  s5d <- s5d %>% mutate(panel = "Difference, Yes minus No (95% CI)")
  save_fig(ggplot() +
             geom_col(data = s5r, aes(dimension, estimate, fill = group), position = position_dodge(.75), width = .7) +
             geom_text(data = s5r, aes(dimension, estimate, label = percent(estimate, accuracy = 1), group = group), position = position_dodge(.75), vjust = -0.6, size = 3, colour = GREY_DARK) +
             geom_hline(data = s5d, aes(yintercept = 0), linetype = 2, colour = GREY_MID) +
             geom_pointrange(data = s5d, aes(dimension, diff, ymin = lo, ymax = hi), colour = ACCENT_GIRL, size = .5) +
             geom_text(data = s5d, aes(dimension, hi, label = sprintf("%+d pp", round(100 * diff))), vjust = -0.7, size = 3, colour = GREY_DARK) +
             facet_grid(source ~ panel, scales = "free", labeller = labeller(source = label_wrap_gen(16))) +
             scale_fill_manual(values = c(No = "#F7B6D6", Yes = ACCENT_GIRL)) + scale_x_discrete(labels = label_wrap(12)) +
             scale_y_continuous(labels = function(v) percent(v, accuracy = 1), expand = expansion(mult = c(.05, .3))) +
             labs(subtitle = "Young women: predicted NEET rates (left) and the adjusted difference with its confidence interval (right)", x = NULL, y = NULL,
                  caption = "Linear probability models with oblast fixed effects; predicted rates with everyone set to 'No' and then to 'Yes', other characteristics at their observed values.") +
             theme_ukr(11) + theme(strip.text.y = element_text(angle = 0, size = 9), axis.text.x = element_text(size = 8)), "s5_adjusted_motherhood_neet_en", width = 10, height = 6)
}
