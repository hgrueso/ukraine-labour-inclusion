# 02c_mics_descriptive.R: MICS 2025/26 figures (East Java style) -----------
# Input:  output/mics_*.rds (01c_clean_mics.R)
# Output: output/figures/m*_en.png, output/tables/m*.csv, output/tables/mics_headlines.csv
source(here::here("code", "00_config.R")); source(here::here("R", "plots.R"))
suppressPackageStartupMessages(library(purrr))

hl <- readRDS(here::here("output", "mics_members.rds"))
wm <- readRDS(here::here("output", "mics_women.rds"))
fs <- readRDS(here::here("output", "mics_fs.rds"))
hh <- readRDS(here::here("output", "mics_hh.rds"))
cap <- "MICS 2025/26 (SSSU and UNICEF), weighted, 95% CIs accounting for the survey design."
dhl <- svydesign(ids = ~psu, strata = ~stratum, weights = ~hhweight, data = hl, nest = TRUE)
dwm <- svydesign(ids = ~psu, strata = ~stratum, weights = ~wmweight, data = wm, nest = TRUE)
dfs <- svydesign(ids = ~psu, strata = ~stratum, weights = ~fsweight, data = fs, nest = TRUE)
dhh <- svydesign(ids = ~psu, strata = ~stratum, weights = ~hhweight, data = hh %>% filter(hh_complete == 1), nest = TRUE)
w1524 <- subset(dwm, age >= 15 & age <= 24)
w1824 <- subset(dwm, age >= 18 & age <= 24)
PINK <- c("15-17" = "#F7B6D6", "18-24" = ACCENT_GIRL, "15-24" = "#8A0049")

bars_w <- function(d, x, fill = ACCENT_GIRL) {
  ggplot(d, aes({{ x }}, estimate)) + geom_col(fill = fill, width = .65) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = .15, colour = GREY_DARK, linewidth = .4) +
    geom_text(aes(y = hi, label = percent(estimate, accuracy = 1)), vjust = -0.8, size = 3.1, colour = GREY_DARK) +
    scale_x_discrete(labels = label_wrap(14)) +
    scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .2))) +
    labs(x = NULL, y = NULL, caption = cap) + theme_ukr()
}
overall <- function(design, var) { r <- svymean(as.formula(paste0("~", var)), design, na.rm = TRUE); ci <- confint(r)
  tibble(estimate = as.numeric(r), lo = pmax(ci[1], 0), hi = pmin(ci[2], 1)) }

# m1: school attendance by level, age group and sex (household roster) ------------------
LV <- c(in_gensec = "General secondary", in_vocational = "Vocational", in_higher = "Higher education", in_other_educ = "Other")
AG <- list(`14-17` = subset(dhl, age >= 14 & age <= 17), `18-24` = subset(dhl, age >= 18 & age <= 24))
m1 <- map_dfr(names(AG), function(g) map_dfr(names(LV), ~ wt_pct(AG[[g]], .x, "sexo") %>% mutate(level = LV[[.x]])) %>% mutate(group = g)) %>%
  mutate(group = factor(group, levels = names(AG)), level = factor(level, levels = rev(unname(LV))),
         sex = factor(ifelse(sexo == "Young women", "Women", "Men"), levels = c("Women", "Men")))
tot1 <- m1 %>% group_by(group, sex) %>% summarise(total = sum(estimate), .groups = "drop")
write_table(m1, "m1_attendance_by_level")
save_fig(ggplot(m1, aes(sex, estimate, fill = level)) + geom_col(width = .7) +
           geom_text(aes(label = ifelse(estimate >= .03, percent(estimate, accuracy = 1), "")), position = position_stack(vjust = .5), size = 3, colour = "white") +
           geom_text(data = tot1, aes(sex, total, label = percent(total, accuracy = 1)), inherit.aes = FALSE, vjust = -0.5, size = 3.3, colour = GREY_DARK) +
           facet_wrap(~group, nrow = 1) +
           scale_fill_manual(values = c("General secondary" = UNICEF_BLUE, "Vocational" = GREY_MID, "Higher education" = UNICEF_DARK, "Other" = "#C9D3DB"), breaks = unname(LV)) +
           scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .12))) +
           labs(subtitle = "Share attending school or university in the current school year, by level and sex", x = NULL, y = NULL, caption = cap) +
           theme_ukr(), "m1_attendance_by_level_en", width = 10, height = 5)

# m2: young women: in education, training, employment, NEET, by age group ------------------
OUTW <- c(in_educ = "In education", in_training = "In courses or training", employed = "Employed", neet = "NEET")
WG <- list(`15-17` = subset(dwm, age >= 15 & age <= 17), `18-24` = w1824, `15-24` = w1524)
m2 <- map_dfr(names(WG), function(g) map_dfr(names(OUTW), ~ overall(WG[[g]], .x) %>% mutate(indicator = OUTW[[.x]])) %>% mutate(group = g)) %>%
  mutate(group = factor(group, levels = names(WG)), indicator = factor(indicator, levels = unname(OUTW)))
write_table(m2, "m2_women_outcomes_by_age")
save_fig(ggplot(m2, aes(indicator, estimate, fill = group)) + geom_col(position = position_dodge(.8), width = .75) +
           geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(.8), width = .15, colour = GREY_DARK, linewidth = .4) +
           geom_text(aes(y = hi, label = percent(estimate, accuracy = 1)), position = position_dodge(.8), vjust = -0.8, size = 3, colour = GREY_DARK) +
           scale_fill_manual(values = PINK) + scale_x_discrete(labels = label_wrap(14)) +
           scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .2))) +
           labs(subtitle = "Young women, by age group", x = NULL, y = NULL, caption = cap) + theme_ukr(),
         "m2_women_outcomes_by_age_en", width = 9.5, height = 5)

# m2b: NEET among young women by age group ------------------------------------------------------
m2b <- m2 %>% filter(indicator == "NEET", group != "15-24") %>% mutate(n = c(`15-17` = sum(wm$age >= 15 & wm$age <= 17 & !is.na(wm$neet)),
                                                           `18-24` = sum(wm$age >= 18 & wm$age <= 24 & !is.na(wm$neet)),
                                                           `15-24` = sum(wm$age >= 15 & wm$age <= 24 & !is.na(wm$neet)))[as.character(group)],
                                                     xlab = factor(sprintf("%s (n = %d)", group, n), levels = sprintf("%s (n = %d)", c("15-17", "18-24"), n[match(c("15-17", "18-24"), group)])))
write_table(m2b, "m2b_neet_women_by_age")
save_fig(bars_w(m2b, xlab) + labs(subtitle = "Young women not in education, training or employment"), "m2b_neet_women_by_age_en", width = 7.5, height = 4.8)

# m3: NEET and employment by marriage and motherhood, women 18-24 --------------------------
mp_lev <- c("Not married, no child", "Married or in union, no child", "Not married, with child", "Married or in union, with child")
wmp <- update(subset(w1824, !is.na(married) & !is.na(mother)),
              mp = factor(paste0(ifelse(married == 1, "Married or in union", "Not married"), ", ", ifelse(mother == 1, "with child", "no child")), levels = mp_lev))
m3 <- map_dfr(c("neet", "employed", "in_educ"), ~ wt_pct(wmp, .x, "mp") %>% mutate(indicator = OUTW[[.x]]))
n3 <- wm %>% filter(age >= 18, age <= 24, !is.na(married), !is.na(mother)) %>%
  mutate(mp = paste0(ifelse(married == 1, "Married or in union", "Not married"), ", ", ifelse(mother == 1, "with child", "no child"))) %>%
  group_by(mp) %>% summarise(n = n(), pop = sum(wmweight), .groups = "drop") %>% mutate(share = pop / sum(pop))
m3 <- m3 %>% left_join(n3, by = "mp") %>% mutate(mp_lab = factor(sprintf("%s (n = %d)", mp, n), levels = sprintf("%s (n = %d)", mp_lev, n3$n[match(mp_lev, n3$mp)])),
                                                indicator = factor(indicator, levels = c("NEET", "Employed", "In education")))
write_table(m3, "m3_neet_marriage_motherhood")
pal3 <- setNames(c("#F7B6D6", "#EE6FAE", "#B5005F", "#6E003A"), levels(m3$mp_lab))
save_fig(ggplot(m3, aes(mp_lab, estimate, fill = mp_lab)) + geom_col(width = .75) +
           geom_errorbar(aes(ymin = lo, ymax = hi), width = .16, colour = GREY_DARK, linewidth = .4) +
           geom_text(aes(y = hi, label = percent(estimate, accuracy = 1)), vjust = -0.8, size = 3, colour = GREY_DARK) +
           facet_wrap(~indicator, nrow = 1) + scale_fill_manual(values = pal3) + guides(fill = guide_legend(nrow = 2)) +
           scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .15))) +
           labs(subtitle = "Women aged 18-24, by marital status and motherhood", x = NULL, y = NULL, caption = cap) +
           theme_ukr(12) + theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank()),
         "m3_neet_marriage_motherhood_en", width = 11, height = 5.2)
mothers_unmarried <- with(n3, round(100 * sum(pop[mp == "Not married, with child"]) / sum(pop[grepl("with child", mp)]), 1))

# m4: NEET by vulnerability and household changes since the beginning of the conflict -----
groups <- c(mother = "Mother", rural = "Rural", bottom40 = "Poorest 40% (wealth)", idp = "Internally displaced",
            disability = "Functional difficulty (18-24)")
groups_hh <- c(hh_moved = "Household moved since 2022", hh_damaged = "Dwelling damaged since 2022", hh_income_down = "Household income decreased since 2022")
m4 <- map_dfr(names(groups), function(g) {
  dd <- w1524[!is.na(w1524$variables[[g]]), ]
  wt_pct(dd, "neet", g) %>% rename(group = !!g) %>% mutate(dimension = groups[[g]], group = ifelse(group == 1, "Yes", "No"),
                                                           n = as.integer(table(dd$variables[[g]])[ifelse(group == "Yes", "1", "0")]))
}) %>% mutate(group = factor(group, levels = c("No", "Yes")), dimension = factor(dimension, levels = unname(groups)))
write_table(m4, "m4_neet_by_group")
m4 <- m4 %>% mutate(xlab = paste0(group, " (n=", n, ")"))
save_fig(bars_w(m4, xlab) + facet_wrap(~dimension, nrow = 1, scales = "free_x", labeller = label_wrap_gen(18)) +
           theme(axis.text.x = element_text(size = 8)) + labs(subtitle = "NEET among women aged 15-24, by group (n = unweighted sample in each group)"),
         "m4_neet_by_group_en", width = 12, height = 4.8)
m4b <- map_dfr(names(groups_hh), function(g) {
  dd <- w1524[!is.na(w1524$variables[[g]]), ]
  wt_pct(dd, "neet", g) %>% rename(group = !!g) %>% mutate(dimension = groups_hh[[g]], group = ifelse(group == 1, "Yes", "No"),
                                                           n = as.integer(table(dd$variables[[g]])[ifelse(group == "Yes", "1", "0")]))
}) %>% mutate(group = factor(group, levels = c("No", "Yes")), dimension = factor(dimension, levels = unname(groups_hh)), xlab = paste0(group, " (n=", n, ")"))
write_table(m4b, "m4b_neet_by_household_change")
save_fig(bars_w(m4b, xlab) + facet_wrap(~dimension, nrow = 1, scales = "free_x", labeller = label_wrap_gen(22)) +
           theme(axis.text.x = element_text(size = 8)) + labs(subtitle = "NEET among women aged 15-24, by household changes since the beginning of the conflict"),
         "m4b_neet_by_household_change_en", width = 10, height = 4.8)

# m4c: same groups, women 18-24 (sensitivity) --------------------------------------------------
m4c <- map_dfr(names(groups), function(g) {
  dd <- w1824[!is.na(w1824$variables[[g]]), ]
  wt_pct(dd, "neet", g) %>% rename(group = !!g) %>% mutate(dimension = groups[[g]], group = ifelse(group == 1, "Yes", "No"),
                                                           n = as.integer(table(dd$variables[[g]])[ifelse(group == "Yes", "1", "0")]))
}) %>% mutate(group = factor(group, levels = c("No", "Yes")), dimension = factor(dimension, levels = unname(groups)), xlab = paste0(group, " (n=", n, ")"))
write_table(m4c, "m4c_neet_by_group_1824")
save_fig(bars_w(m4c, xlab) + facet_wrap(~dimension, nrow = 1, scales = "free_x", labeller = label_wrap_gen(18)) +
           theme(axis.text.x = element_text(size = 8)) + labs(subtitle = "NEET among women aged 18-24, by group (n = unweighted sample in each group)"),
         "m4c_neet_by_group_1824_en", width = 12, height = 4.8)

# m5: NEET by macroregion and wealth quintile, women 18-24 ----------------------------------
m5 <- bind_rows(wt_pct(subset(w1824, !is.na(macroregion)), "neet", "macroregion") %>% rename(group = macroregion) %>% mutate(dimension = "Macroregion"),
                wt_pct(subset(w1824, !is.na(wealth_q)), "neet", "wealth_q") %>% mutate(group = paste0("Q", wealth_q), dimension = "Wealth quintile (Q1 = poorest)") %>% select(-wealth_q))
write_table(m5, "m5_neet_region_wealth")
save_fig(bars_w(m5, group) + facet_wrap(~dimension, scales = "free_x") + labs(subtitle = "NEET among women aged 18-24"),
         "m5_neet_region_wealth_en", width = 10, height = 4.8)

# m6: early marriage and early motherhood, women 20-24 ---------------------------------------
w2024 <- subset(dwm, age >= 20 & age <= 24)
m6 <- bind_rows(overall(w2024, "union_u18") %>% mutate(indicator = "Married or in union before 18"),
                overall(w2024, "birth_u18") %>% mutate(indicator = "First birth before 18"),
                overall(w2024, "birth_u20") %>% mutate(indicator = "First birth before 20"),
                overall(w1824, "mother") %>% mutate(indicator = "Mothers among women 18-24"),
                overall(w1824, "married") %>% mutate(indicator = "Married or in union, 18-24")) %>%
  mutate(indicator = factor(indicator, levels = indicator))
write_table(m6, "m6_early_marriage_motherhood")
save_fig(bars_w(m6, indicator) + labs(subtitle = "Young women; early marriage and early births measured among women aged 20-24"),
         "m6_early_marriage_motherhood_en", width = 9.5, height = 4.8)

# m7: household chores and care work, adolescents 12-17, by sex ------------------------------
CW <- c(care_children = "Caring for children", care_sick = "Caring for older or sick people", cooking = "Cooking", washing = "Washing clothes")
a1217 <- subset(dfs, age >= 12 & age <= 17)
m7 <- map_dfr(names(CW), ~ wt_pct(a1217, .x, "sexo") %>% mutate(indicator = CW[[.x]])) %>% mutate(indicator = factor(indicator, levels = unname(CW)))
write_table(m7, "m7_adolescent_care_work")
save_fig(bars_sex(m7, indicator, cap, fills = c(Girls = ACCENT_GIRL, Boys = ACCENT_BOY)) +
           labs(subtitle = "Household tasks done in the past week, adolescents aged 12-17, by sex"),
         "m7_adolescent_care_work_en", width = 9.5, height = 4.8)

# m8: preschool attendance, children 3-5 ------------------------------------------------------
p35 <- subset(dhl, age >= 3 & age <= 5 & !is.na(preschool))
m8 <- bind_rows(overall(p35, "preschool") %>% mutate(group = "All", dimension = "All"),
                wt_pct(p35, "preschool", "rural") %>% mutate(group = ifelse(rural == 1, "Rural", "Urban"), dimension = "Area") %>% select(-rural),
                wt_pct(p35, "preschool", "wealth_q") %>% mutate(group = paste0("Q", wealth_q), dimension = "Wealth quintile") %>% select(-wealth_q)) %>%
  mutate(dimension = factor(dimension, levels = c("All", "Area", "Wealth quintile")))
write_table(m8, "m8_preschool_attendance")
save_fig(bars_w(m8, group, fill = UNICEF_BLUE) + facet_grid(~dimension, scales = "free_x", space = "free_x") +
           labs(subtitle = "Children aged 3-5 attending preschool or school in the current school year"),
         "m8_preschool_attendance_en", width = 10, height = 4.8)

# m8b: preschool attendance by single year of age ----------------------------------------------
p35 <- update(p35, age_y = factor(paste("Age", age)))
m8b <- bind_rows(wt_pct(p35, "preschool", "age_y") %>% mutate(group = as.character(age_y), dimension = "By age") %>% select(-age_y),
                 wt_pct(p35, "preschool", c("age_y", "rural")) %>% mutate(group = paste(as.character(age_y), ifelse(rural == 1, "rural", "urban")), dimension = "By age and area") %>% select(-age_y, -rural))
write_table(m8b, "m8b_preschool_by_age")
save_fig(bars_w(m8b %>% filter(dimension == "By age"), group, fill = UNICEF_BLUE) +
           labs(subtitle = "Children attending preschool or school, by age"), "m8b_preschool_by_age_en", width = 7, height = 4.5)

# m9: households: changes since the beginning of the conflict -------------------------------
HC <- c(hh_moved = "All members moved elsewhere", hh_damaged = "Dwelling damaged or destroyed",
        hh_separated = "Members separated from the household", hh_income_down = "Household income decreased")
m9 <- map_dfr(names(HC), ~ overall(dhh[!is.na(dhh$variables[[.x]]), ], .x) %>% mutate(indicator = HC[[.x]])) %>%
  mutate(indicator = factor(indicator, levels = unname(HC)))
write_table(m9, "m9_household_changes")
save_fig(bars_w(m9, indicator, fill = UNICEF_DARK) + labs(subtitle = "Share of households reporting each change since the beginning of the conflict"),
         "m9_household_changes_en", width = 9, height = 4.8)

# m12: NEET and employment by education level, women 18-24 ------------------------------------
EL <- c("Primary or less", "Lower secondary", "Upper secondary", "Vocational", "Higher")
w1824e <- update(subset(w1824, !is.na(educ)), educ_f = factor(educ, levels = EL))
m12 <- map_dfr(c("neet", "employed"), ~ wt_pct(w1824e, .x, "educ_f") %>% mutate(indicator = OUTW[[.x]])) %>%
  left_join(wm %>% filter(age >= 18, age <= 24, !is.na(educ)) %>% count(educ_f = factor(educ, levels = EL)), by = "educ_f") %>%
  mutate(xlab = factor(sprintf("%s (n=%d)", educ_f, n), levels = sprintf("%s (n=%d)", EL, n[match(EL, educ_f)])), indicator = factor(indicator, levels = c("NEET", "Employed")))
write_table(m12, "m12_outcomes_by_education")
save_fig(bars_w(m12, xlab) + facet_wrap(~indicator) + theme(axis.text.x = element_text(size = 8)) +
           labs(subtitle = "Women aged 18-24, by highest level of education"), "m12_outcomes_by_education_en", width = 11, height = 5)

# m10: association between key variables, women 18-24 -----------------------------------------
yy <- wm %>% filter(age >= 18, age <= 24) %>%
  transmute(Age = age, Macroregion = macroregion, Rural = rural, `Wealth quintile` = wealth_q,
            Education = educ, Married = married, Mother = mother, IDP = idp, `Household moved` = hh_moved,
            `Income decreased` = hh_income_down, `In education` = in_educ, Employed = employed, NEET = neet, w = wmweight)
hv <- heatmap_v(yy, setdiff(names(yy), "w"), yy$w, "Strength of association between key variables, women 18-24 (0 = none, 1 = perfect)", cap)
write_table(hv$data, "m10_relationships_mics"); save_fig(hv$plot, "m10_relationships_mics_en", width = 9, height = 7)

# headline numbers -----------------------------------------------------------------------------
gm <- function(df, ind, g) round(100 * df$estimate[df$indicator == ind & df$group == g][1], 1)
heads <- tibble(
  key = c("neet_w_1518",
          "att_w_1417", "att_m_1417", "att_w_1824", "att_m_1824", "higher_w_1824", "higher_m_1824", "neet_w_1524", "neet_w_1824", "emp_w_1824", "educ_w_1518",
          "mothers_unmarried", "union_u18", "birth_u20", "mother_1824", "care_g", "care_b", "preschool", "hh_moved", "hh_income_down",
          "n_w1524", "n_w1824"),
  value = c(gm(m2, "NEET", "15-17"),
            round(100 * tot1$total[tot1$group == "14-17" & tot1$sex == "Women"], 1), round(100 * tot1$total[tot1$group == "14-17" & tot1$sex == "Men"], 1),
            round(100 * tot1$total[tot1$group == "18-24" & tot1$sex == "Women"], 1), round(100 * tot1$total[tot1$group == "18-24" & tot1$sex == "Men"], 1),
            round(100 * m1$estimate[m1$group == "18-24" & m1$sex == "Women" & m1$level == "Higher education"], 1),
            round(100 * m1$estimate[m1$group == "18-24" & m1$sex == "Men" & m1$level == "Higher education"], 1),
            gm(m2, "NEET", "15-24"), gm(m2, "NEET", "18-24"), gm(m2, "Employed", "18-24"), gm(m2, "In education", "15-17"),
            mothers_unmarried, round(100 * m6$estimate[1], 1), round(100 * m6$estimate[3], 1), round(100 * m6$estimate[4], 1),
            round(100 * m7$estimate[m7$sexo == "Girls" & m7$indicator == "Caring for children"], 1),
            round(100 * m7$estimate[m7$sexo == "Boys" & m7$indicator == "Caring for children"], 1),
            round(100 * m8$estimate[m8$group == "All"], 1), round(100 * m9$estimate[1], 1), round(100 * m9$estimate[4], 1),
            sum(wm$age >= 15 & wm$age <= 24, na.rm = TRUE), sum(wm$age >= 18 & wm$age <= 24, na.rm = TRUE)))
write_table(heads, "mics_headlines"); print(heads, n = Inf)
