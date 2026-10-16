# 02_descriptive.R: LFS 2021 descriptive figures (East Java style) ----------
# Input:  output/lfs_analysis_ready.rds
# Output: output/figures/f*_en.png, output/tables/*.csv,
#         output/tables/lfs_headlines.csv (numbers quoted on the slides)
source(here::here("code", "00_config.R"))
suppressPackageStartupMessages({ library(survey); library(purrr); library(forcats) })
options(survey.lonely.psu = "adjust")

lfs <- readRDS(here::here("output", "lfs_analysis_ready.rds"))
cap <- "SSSU Labour Force Survey 2021, weighted, 95% CIs clustered by person."
des_all <- svydesign(ids = ~kod_res, weights = ~w, data = lfs)
des_y   <- subset(des_all, age_band == "15-24")
des_1529 <- subset(des_all, age_band %in% c("15-24", "25-29"))   # ILO NEET frame (15-29), reference only

# Weighted share of a 0/1 variable with 95% CI, by groups
wt_pct <- function(design, var, by) {
  f <- as.formula(paste0("~", var)); b <- as.formula(paste0("~", paste(by, collapse = "+")))
  r <- svyby(f, b, design, svymean, na.rm = TRUE, vartype = c("se", "ci"))
  tibble::as_tibble(r) %>% rename(estimate = !!var, lo = ci_l, hi = ci_u) %>% select(-se)
}
OUT <- c(in_lf = "Labour-force participation", employed = "Employed", unemp_rate = "Unemployment rate", neet = "NEET")
outcomes_by <- function(design, by = NULL) {
  map_dfr(names(OUT), ~ wt_pct(design, .x, c("sexo", by)) %>% mutate(indicator = OUT[[.x]])) %>%
    mutate(indicator = factor(indicator, levels = unname(OUT)))
}
pct_y <- scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .2)))
bars <- function(d, x) {
  ggplot(d, aes({{ x }}, estimate, fill = sexo)) +
    geom_col(position = position_dodge(.75), width = .7) +
    geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(.75), width = .16,
                  colour = GREY_DARK, linewidth = .4) +
    geom_text(aes(y = hi, label = percent(estimate, accuracy = 1)), position = position_dodge(.75),
              vjust = -0.8, size = 3.1, colour = GREY_DARK) +
    scale_fill_manual(values = FILL_SEX) + scale_x_discrete(labels = label_wrap(11)) + pct_y +
    labs(x = NULL, y = NULL, caption = cap) + theme_ukr()
}

# f1a: headline outcomes by sex, 15-24 ------------------------------------------
f1a <- outcomes_by(des_y); write_table(f1a, "f1a_outcomes_by_sex")
save_fig(bars(f1a, indicator) + labs(subtitle = "Young people 15-24, by sex"), "f1a_outcomes_by_sex_en")

# f1b: NEET for 15-24 and for 15-29 (ILO frame) ----------------------------------------
f1b <- bind_rows(wt_pct(des_y, "neet", "sexo") %>% mutate(group = "15-24"),
                 wt_pct(des_1529, "neet", "sexo") %>% mutate(group = "15-29 (ILO NEET frame)")) %>%
  mutate(group = factor(group, levels = c("15-24", "15-29 (ILO NEET frame)")), indicator = "NEET")
write_table(f1b, "f1b_neet_1524_1529")
save_fig(bars(f1b, group) + labs(subtitle = "NEET rate by age group and sex"), "f1b_neet_age_groups_en", width = 7.5, height = 4.8)

# f1c / f1d / f1e: by area, education, marital status (15-24) ------------------------
for (spec in list(list("area", "f1c_outcomes_by_area", "By urban/rural area and sex, 15-24", 9),
                  list("education", "f1d_outcomes_by_education", "By education level and sex, 15-24", 11),
                  list("married", "f1e_outcomes_by_marital", "By marital status and sex, 15-24", 9))) {
  d <- outcomes_by(des_y, spec[[1]]) %>% filter(!is.na(.data[[spec[[1]]]]))
  if (spec[[1]] == "married") d <- d %>% mutate(married = factor(ifelse(married == 1, "Married", "Not married"),
                                                               levels = c("Not married", "Married")))
  write_table(d, spec[[2]])
  save_fig(bars(d, .data[[spec[[1]]]]) + facet_wrap(~indicator, nrow = 1, scales = "free_y", labeller = label_wrap_gen(16)) +
             labs(subtitle = spec[[3]]), paste0(spec[[2]], "_en"), width = spec[[4]] + 2, height = 4.8)
}

# f2: reasons for being outside the labour force -------------------------------------
inact <- subset(des_y, inactive == 1 & !is.na(reason_inact))
f2 <- svyby(~reason_inact, ~sexo, inact, svymean, na.rm = TRUE) %>% tibble::as_tibble() %>%
  select(sexo, starts_with("reason_inact")) %>% select(-starts_with("se.")) %>%
  pivot_longer(starts_with("reason_inact"), names_to = "reason", values_to = "estimate") %>%
  mutate(reason = str_remove(reason, "^reason_inact"))
write_table(f2, "f2_inactivity_reasons")
save_fig(ggplot(f2, aes(reorder(reason, estimate), estimate, fill = sexo)) +
           geom_col(position = position_dodge(.75), width = .7) +
           geom_text(aes(label = percent(estimate, accuracy = 1)), position = position_dodge(.75),
                     hjust = -0.2, size = 3, colour = GREY_DARK) +
           coord_flip() + scale_fill_manual(values = FILL_SEX) +
           scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .25))) +
           labs(subtitle = "Main reason for being outside the labour force, 15-24, by sex", x = NULL, y = NULL, caption = cap) +
           theme_ukr() + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey92")),
         "f2_inactivity_reasons_en", width = 9, height = 5)

# f3: job quality among the employed ----------------------------------------------
emp <- subset(des_y, employed == 1)
f3 <- bind_rows(wt_pct(emp, "informal", "sexo") %>% mutate(indicator = "Informal employment"),
                wt_pct(emp, "short_hours", "sexo") %>% mutate(indicator = "Under 30 hours a week"))
write_table(f3, "f3_job_quality")
save_fig(bars(f3, indicator) + labs(subtitle = "Employed young people 15-24, by sex"),
         "f3_job_quality_en", width = 7.5, height = 4.8)

# f4: occupational segregation ----------------------------------------------------------
duncan <- function(d) {
  t <- xtabs(w ~ occ_code + female, d); t <- sweep(t, 2, colSums(t), "/"); round(100 * .5 * sum(abs(t[, 1] - t[, 2])), 1)
}
e <- lfs %>% filter(employed == 1, !is.na(occ_code))
D <- c(`15-24` = duncan(filter(e, age_band == "15-24")), `All ages` = duncan(e))
write_table(tibble(group = names(D), duncan_index = D), "f4_duncan_index")
f4 <- bind_rows(
  wt_pct(subset(des_all, employed == 1 & age_band == "15-24" & !is.na(occupation)), "female", "occupation") %>% mutate(group = "15-24"),
  wt_pct(subset(des_all, employed == 1 & !is.na(occupation)), "female", "occupation") %>% mutate(group = "All ages"))
write_table(f4, "f4_female_share_by_occupation")
save_fig(ggplot(f4, aes(reorder(occupation, estimate), estimate, fill = group)) +
           geom_col(position = position_dodge(.75), width = .7) +
           geom_hline(yintercept = .5, linetype = 2, colour = GREY_MID) +
           geom_text(aes(label = percent(estimate, accuracy = 1)), position = position_dodge(.75),
                     hjust = -0.2, size = 3, colour = GREY_DARK) +
           coord_flip() + scale_fill_manual(values = c("15-24" = ACCENT_GIRL, "All ages" = GREY_MID)) +
           scale_y_continuous(labels = percent_format(1), limits = c(0, 1.05)) +
           labs(subtitle = sprintf("Women's share of employment by occupation (dissimilarity: %s at 15-24)", D["15-24"]), x = NULL, y = NULL, caption = cap) +
           theme_ukr() + theme(panel.grid.major.y = element_blank()),
         "f4_occupation_share_en", width = 10, height = 5.2)

# f5: NEET by oblast, 15-24 ----------------------------------------------------------
f5 <- wt_pct(subset(des_y, !is.na(region)), "neet", c("sexo", "region"))
write_table(f5, "f5_neet_by_oblast")
ord <- f5 %>% filter(sexo == "Young women") %>% arrange(estimate) %>% pull(region)
save_fig(ggplot(f5 %>% mutate(region = factor(region, levels = ord)), aes(estimate, region, colour = sexo)) +
           geom_line(aes(group = region), colour = "grey80") + geom_point(size = 2.6) +
           scale_colour_manual(values = FILL_SEX) + scale_x_continuous(labels = percent_format(1)) +
           labs(subtitle = "NEET among 15-24 year olds by oblast and sex", x = NULL, y = NULL, caption = cap) +
           theme_ukr(11) + theme(panel.grid.major.x = element_line(colour = "grey92")),
         "f5_neet_by_oblast_en", width = 8, height = 7)

# f6: unemployment: duration and 'not placed after education' --------------------------
un <- subset(des_y, unemployed == 1)
f6 <- bind_rows(wt_pct(un, "long_unemp", "sexo") %>% mutate(indicator = "Unemployed and looking for work for 12 months or more"),
                wt_pct(un, "not_placed", "sexo") %>% mutate(indicator = "Unemployed because not placed in a job after completing education"))
write_table(f6, "f6_unemployment")
save_fig(bars(f6, indicator) + scale_x_discrete(labels = label_wrap(26)) + labs(subtitle = "Share of unemployed young people 15-24, by sex"),
         "f6_unemployment_en", width = 7.5, height = 4.8)

# f7: how strongly do the key variables relate to each other? (15-24) ------------------
y <- lfs %>% filter(age_band == "15-24") %>%
  transmute(Sex = sexo, Area = area, Region = region, Education = education, Married = married,
            Diploma = diploma, `Labour status` = lf, NEET = neet, `Reason inactive` = reason_inact,
            Occupation = occupation, Informal = informal, Hours = hours, w)
vars <- setdiff(names(y), "w")
f7 <- expand_grid(a = vars, b = vars) %>%
  mutate(v = map2_dbl(a, b, ~ if (.x == .y) NA_real_ else cramers_v(y[[.x]], y[[.y]], y$w)),
         a = factor(a, levels = vars), b = factor(b, levels = rev(vars)))
write_table(f7, "f7_relationships_lfs")
save_fig(ggplot(f7, aes(a, b, fill = v)) + geom_tile(colour = "white") +
           geom_text(aes(label = ifelse(is.na(v), "", sprintf("%.2f", v))), size = 2.8, colour = GREY_DARK) +
           scale_fill_gradient(low = "#f6f8fa", high = UNICEF_DARK, na.value = "white", limits = c(0, 1), name = "Cramer's V") +
           labs(subtitle = "Strength of association between key variables, 15-24 (0 = none, 1 = perfect)",
                x = NULL, y = NULL, caption = "SSSU Labour Force Survey 2021, weighted.") +
           theme_ukr(11) + theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "right",
                                 legend.title = element_text(size = 9), panel.grid = element_blank()),
         "f7_relationships_lfs_en", width = 9, height = 7)

# headline numbers quoted on the slides -------------------------------------------
g <- function(d, ind, s, extra = NULL) {
  r <- d %>% filter(indicator == ind, sexo == s); if (!is.null(extra)) r <- r %>% filter(!!rlang::parse_expr(extra))
  round(100 * r$estimate[1], 1)
}
heads <- tibble(
  key = c("neet_w", "neet_m", "lfp_w", "lfp_m", "emp_w", "emp_m", "unemp_w", "unemp_m",
          "neet_w_1529", "neet_m_1529", "duncan_1524", "duncan_all",
          "home_w_1524", "home_m_1524", "inf_w_1524", "inf_m_1524"),
  value = c(g(f1a, "NEET", "Young women"), g(f1a, "NEET", "Young men"),
            g(f1a, "Labour-force participation", "Young women"), g(f1a, "Labour-force participation", "Young men"),
            g(f1a, "Employed", "Young women"), g(f1a, "Employed", "Young men"),
            g(f1a, "Unemployment rate", "Young women"), g(f1a, "Unemployment rate", "Young men"),
            g(f1b, "NEET", "Young women", "group != '15-24'"), g(f1b, "NEET", "Young men", "group != '15-24'"),
            D["15-24"], D["All ages"],
            round(100 * f2$estimate[f2$sexo == "Young women" & f2$reason == "Home duties / dependent"], 1),
            round(100 * f2$estimate[f2$sexo == "Young men" & f2$reason == "Home duties / dependent"], 1),
            round(100 * f3$estimate[f3$sexo == "Young women" & f3$indicator == "Informal employment"], 1),
            round(100 * f3$estimate[f3$sexo == "Young men" & f3$indicator == "Informal employment"], 1)))
write_table(heads, "lfs_headlines")
print(heads, n = Inf)
