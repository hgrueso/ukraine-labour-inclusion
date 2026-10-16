# 02b_hsess_descriptive.R: HSESS 2023/24 figures (wartime picture) ----------
# Input:  output/hsess_analysis_ready.rds
# Output: output/figures/h*_en.png, output/tables/h*.csv, output/tables/hsess_headlines.csv
source(here::here("code", "00_config.R")); source(here::here("R", "plots.R"))
suppressPackageStartupMessages(library(purrr))

d <- readRDS(here::here("output", "hsess_analysis_ready.rds")) %>% filter(w > 0)
cap <- "UNICEF HSESS 2023/24 (Dec 2023 to Feb 2024), weighted, 95% CIs clustered by household."
des <- svydesign(ids = ~key, weights = ~w, data = d)
y     <- subset(des, age >= 15 & age <= 24 & !is.na(sexo))   # 15-24: education-to-work transition
y1518 <- subset(des, age >= 15 & age <= 18 & !is.na(sexo))   # 15-18: education, training, apprenticeships
y1924 <- subset(des, age >= 19 & age <= 24 & !is.na(sexo))   # 19-24: labour market
y1529 <- subset(des, age >= 15 & age <= 29 & !is.na(sexo))   # 15-29: ILO NEET frame (reference)

OUT <- c(in_lf = "Labour-force participation", employed = "Employed", in_educ = "In education", neet = "NEET")
outcomes_by <- function(design, by = NULL)
  map_dfr(names(OUT), ~ wt_pct(design, .x, c("sexo", by)) %>% mutate(indicator = factor(OUT[[.x]], levels = unname(OUT))))

# h0: in education, by type of institution, age group and sex ---------------------------
LV <- c(in_gensec = "General secondary", in_vocational = "Vocational", in_higher = "Higher education", in_other_educ = "Other")
AG0 <- list(`15-18` = y1518, `19-24` = y1924, `15-24` = y)
h0 <- map_dfr(names(AG0), function(g) map_dfr(names(LV), ~ wt_pct(AG0[[g]], .x, "sexo") %>% mutate(level = LV[[.x]])) %>% mutate(group = g)) %>%
  mutate(group = factor(group, levels = names(AG0)), level = factor(level, levels = rev(unname(LV))),
         sex = factor(ifelse(sexo == "Young women", "Women", "Men"), levels = c("Women", "Men")))
tot0 <- h0 %>% group_by(group, sex) %>% summarise(total = sum(estimate), .groups = "drop")
write_table(h0, "h0_education_by_institution")
save_fig(ggplot(h0, aes(sex, estimate, fill = level)) + geom_col(width = .7) +
           geom_text(aes(label = ifelse(estimate >= .03, percent(estimate, accuracy = 1), "")), position = position_stack(vjust = .5),
                     size = 3, colour = "white") +
           geom_text(data = tot0, aes(sex, total, label = percent(total, accuracy = 1)), inherit.aes = FALSE, vjust = -0.5, size = 3.3, colour = GREY_DARK) +
           facet_wrap(~group, nrow = 1) +
           scale_fill_manual(values = c("General secondary" = UNICEF_BLUE, "Vocational" = GREY_MID, "Higher education" = UNICEF_DARK, "Other" = "#C9D3DB"),
                             breaks = unname(LV)) +
           scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .12))) +
           labs(subtitle = "Share of each age group currently in education, by type of institution and sex", x = NULL, y = NULL, caption = cap) +
           theme_ukr(), "h0_education_by_institution_en", width = 10, height = 5)

# h1: outcomes by sex, 19-24 -----------------------------------------------------------
h1 <- outcomes_by(y1924); write_table(h1, "h1_outcomes_by_sex_1924")
save_fig(bars_sex(h1, indicator, cap) + labs(subtitle = "Young people 19-24, by sex"), "h1_outcomes_by_sex_en")

# h2: in education and NEET by age group --------------------------------------------------
AG <- list(`15-18` = y1518, `19-24` = y1924, `15-24` = y, `15-29 (ILO NEET frame)` = y1529)
h2 <- map_dfr(names(AG), function(g) bind_rows(wt_pct(AG[[g]], "in_educ", "sexo") %>% mutate(indicator = "In education"),
                                               wt_pct(AG[[g]], "neet", "sexo") %>% mutate(indicator = "NEET")) %>% mutate(group = g)) %>%
  mutate(group = factor(group, levels = names(AG)))
write_table(h2, "h2_education_neet_by_age")
save_fig(bars_sex(h2 %>% filter(indicator == "NEET"), group, cap) + labs(subtitle = "NEET rate by age group and sex"),
         "h2_neet_by_age_en", width = 9, height = 4.8)

# h3: NEET by vulnerability group, 15-24 ------------------------------------------------
groups <- c(idp = "Internally displaced", disab_hh = "Person with disability in household", rural = "Rural",
            frontline = "Frontline oblast", own_child = "Lives with own child", bottom40 = "Poorest 40% of households",
            partnered = "Married or cohabiting")
h3 <- map_dfr(names(groups), function(g) {
  if (!g %in% names(d) || all(is.na(d[[g]]))) return(NULL)
  wt_pct(y[!is.na(y$variables[[g]]), ], "neet", c("sexo", g)) %>% rename(group = !!g) %>%
    mutate(dimension = groups[[g]], group = ifelse(group == 1, "Yes", "No"))
}) %>% mutate(group = factor(group, levels = c("No", "Yes")), dimension = factor(dimension, levels = unname(groups)))
nn <- map_dfr(names(groups), function(g) if (g %in% names(d)) d %>% filter(age >= 15, age <= 24, .data[[g]] %in% 1) %>%
  summarise(dimension = groups[[g]], n_yes = n()) else NULL)
write_table(left_join(h3, nn, by = "dimension"), "h3_neet_by_vulnerability")
save_fig(bars_sex(h3, group, paste(cap, "Some groups are small; see the n in the table.")) +
           facet_wrap(~dimension, nrow = 2, labeller = label_wrap_gen(22)) +
           labs(subtitle = "NEET among 15-24 year olds, by group and sex"), "h3_neet_by_vulnerability_en", width = 11, height = 6.4)

# h12: motherhood and partnership, women 19-24 ----------------------------------------------
wm <- subset(y1924, sexo == "Young women" & !is.na(own_child) & !is.na(partnered))
wm <- update(wm, mp = factor(paste0(ifelse(partnered == 1, "Married or cohabiting", "Not married"), ", ",
                                    ifelse(own_child == 1, "with child", "no child")),
                             levels = c("Not married, no child", "Married or cohabiting, no child",
                                        "Not married, with child", "Married or cohabiting, with child")))
h12 <- map_dfr(names(OUT)[names(OUT) %in% c("neet", "employed", "in_educ")], function(v)
  wt_pct(wm, v, "mp") %>% mutate(indicator = OUT[[v]]))
n12 <- d %>% filter(age >= 19, age <= 24, sexo == "Young women", !is.na(own_child), !is.na(partnered)) %>%
  mutate(mp = paste0(ifelse(partnered == 1, "Married or cohabiting", "Not married"), ", ", ifelse(own_child == 1, "with child", "no child"))) %>%
  group_by(mp) %>% summarise(n = n(), pop = sum(w), .groups = "drop") %>% mutate(share = pop / sum(pop))
h12 <- h12 %>% left_join(n12 %>% select(mp, n, share), by = "mp") %>%
  mutate(mp_lab = factor(sprintf("%s\n(n = %d)", mp, n), levels = unique(sprintf("%s\n(n = %d)", levels(wm$variables$mp), n12$n[match(levels(wm$variables$mp), n12$mp)]))),
         indicator = factor(indicator, levels = c("NEET", "Employed", "In education")))
write_table(h12, "h12_motherhood_partnership_women")
pal12 <- setNames(c("#F7B6D6", "#EE6FAE", "#B5005F", "#6E003A"), levels(h12$mp_lab))
save_fig(ggplot(h12, aes(mp_lab, estimate, fill = mp_lab)) + geom_col(width = .75) +
           geom_errorbar(aes(ymin = lo, ymax = hi), width = .16, colour = GREY_DARK, linewidth = .4) +
           geom_text(aes(y = hi, label = percent(estimate, accuracy = 1)), vjust = -0.8, size = 3, colour = GREY_DARK) +
           facet_wrap(~indicator, nrow = 1) +
           scale_fill_manual(values = pal12, labels = gsub("\n", " ", names(pal12))) +
           guides(fill = guide_legend(nrow = 2)) +
           scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .15))) +
           labs(subtitle = "Women aged 19-24, by motherhood and partnership", x = NULL, y = NULL, caption = cap) +
           theme_ukr(12) + theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank()),
         "h12_motherhood_partnership_en", width = 11, height = 5.2)
mothers_unpartnered <- with(n12, round(100 * sum(pop[mp == "Not married, with child"]) / sum(pop[grepl("with child", mp)]), 1))

# h4: care: daily care of own children, and NEET by daily care ---------------------------
h4a <- wt_pct(subset(y, !is.na(care_daily)), "care_daily", c("sexo", "age_group")) %>% mutate(age_group = droplevels(age_group))
h4b <- wt_pct(subset(y1924, !is.na(care_daily)), "neet", c("sexo", "care_daily")) %>%
  mutate(care = factor(ifelse(care_daily == 1, "Cares daily", "Does not"), levels = c("Does not", "Cares daily")))
write_table(h4a, "h4a_daily_childcare"); write_table(h4b, "h4b_neet_by_daily_childcare")
save_fig(bars_sex(h4a, age_group, cap) + labs(subtitle = "Cares for or teaches own children every day, by age group and sex"),
         "h4a_daily_childcare_en", width = 8.5, height = 4.8)
save_fig(bars_sex(h4b, care, cap) + labs(subtitle = "NEET among 19-24 year olds, by daily childcare and sex"),
         "h4b_neet_by_daily_childcare_en", width = 7.5, height = 4.8)

# h5: why not working and not looking, 19-24 ------------------------------------------------
r <- subset(y1924, !is.na(reason_inactive))
h5 <- svyby(~reason_inactive, ~sexo, r, svymean, na.rm = TRUE) %>% tibble::as_tibble() %>%
  select(sexo, starts_with("reason_inactive")) %>%
  tidyr::pivot_longer(starts_with("reason_inactive"), names_to = "reason", values_to = "estimate") %>%
  mutate(reason = sub("^reason_inactive", "", reason))
n5 <- d %>% filter(age >= 19, age <= 24, !is.na(reason_inactive)) %>% count(sexo)
write_table(h5, "h5_reasons_not_working")
save_fig(hbars_sex(h5, reason, sprintf("%s n = %s women, %s men.", cap, n5$n[n5$sexo == "Young women"], n5$n[n5$sexo == "Young men"])) +
           labs(subtitle = "Main reason for not working and not looking for work, 19-24"),
         "h5_reasons_not_working_en", width = 10, height = 5.2)

# h6: job quality among the employed, 19-24 ------------------------------------------------
e <- subset(y1924, employed == 1)
h6 <- bind_rows(wt_pct(e, "informal", "sexo") %>% mutate(indicator = "Informal (no written contract or unregistered)"),
                wt_pct(e, "short_hours", "sexo") %>% mutate(indicator = "Usually under 30 hours a week"),
                wt_pct(e, "wants_more_hours", "sexo") %>% mutate(indicator = "Would like more paid hours"))
write_table(h6, "h6_job_quality")
save_fig(bars_sex(h6, indicator, cap) + labs(subtitle = "Employed young people 19-24, by sex"), "h6_job_quality_en", width = 9, height = 4.8)

# h7: occupational segregation ---------------------------------------------------------------
h7 <- bind_rows(wt_pct(subset(des, employed == 1 & age >= 19 & age <= 24 & !is.na(occupation) & occupation != "Other"), "female", "occupation") %>% mutate(group = "19-24"),
                wt_pct(subset(des, employed == 1 & age >= 15 & age <= 64 & !is.na(occupation) & occupation != "Other"), "female", "occupation") %>% mutate(group = "15-64"))
n7 <- d %>% filter(employed == 1, age >= 19, age <= 24, !is.na(occupation), occupation != "Other") %>% count(occupation, name = "n_1924")
ref7 <- d %>% filter(employed == 1, age >= 19, age <= 24, !is.na(occupation), occupation != "Other", !is.na(female)) %>%
  summarise(s = weighted.mean(female, w)) %>% pull(s)
h7 <- h7 %>% left_join(n7, by = "occupation") %>% mutate(occ_lab = sprintf("%s (n = %d)", occupation, n_1924))
write_table(h7, "h7_female_share_by_occupation")
save_fig(ggplot(h7, aes(reorder(occ_lab, estimate), estimate, fill = group)) +
           geom_col(position = position_dodge(.75), width = .7) +
           geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(.75), width = .25, colour = GREY_DARK, linewidth = .35) +
           geom_hline(yintercept = ref7, linetype = 2, colour = ACCENT_GIRL) +
           geom_text(aes(y = hi, label = percent(estimate, accuracy = 1)), position = position_dodge(.75), hjust = -0.25, size = 3, colour = GREY_DARK) +
           coord_flip() + scale_fill_manual(values = c("19-24" = ACCENT_GIRL, "15-64" = GREY_MID)) +
           scale_y_continuous(labels = percent_format(1), limits = c(0, 1.08)) +
           labs(subtitle = sprintf("Women's share of employment by occupation; dashed line: all occupations at 19-24 (%s)",
                                   percent(ref7, accuracy = 1)), x = NULL, y = NULL, caption = cap) +
           theme_ukr() + theme(panel.grid.major.y = element_blank()), "h7_occupation_share_en", width = 11.5, height = 5.4)

# job titles reported in each occupation group, for review (free text, Ukrainian; not published)
jt <- d %>% filter(employed == 1, age >= 19, age <= 29, !is.na(occupation), !is.na(job_title), job_title != "") %>%
  mutate(sex = ifelse(sexo == "Young women", "Women", "Men")) %>% count(occupation, sex, job_title, sort = TRUE) %>%
  group_by(occupation, sex) %>% slice_head(n = 15) %>% ungroup() %>% arrange(occupation, sex, desc(n))
write_csv(jt, here::here("output", "inventory", "occupation_job_titles_review.csv"))

# h8: earnings: median monthly wage by sex -------------------------------------------------
h8 <- d %>% filter(!is.na(wage), !is.na(sexo), age >= 19, age <= 64) %>%
  mutate(group = ifelse(age <= 24, "19-24", "25-64"), sex = ifelse(sexo == "Young women", "Women", "Men")) %>%
  group_by(group, sex) %>% summarise(median_wage = wmedian(wage, w), n = n(), .groups = "drop") %>%
  group_by(group) %>% mutate(gap_pct = round(100 * (1 - median_wage[sex == "Women"] / median_wage[sex == "Men"]), 1)) %>% ungroup()
write_table(h8, "h8_median_wage")
save_fig(ggplot(h8, aes(group, median_wage, fill = factor(sex, levels = c("Women", "Men")))) +
           geom_col(position = position_dodge(.75), width = .7) +
           geom_text(aes(label = format(round(median_wage), big.mark = ",")), position = position_dodge(.75), vjust = -0.6, size = 3.3, colour = GREY_DARK) +
           scale_fill_manual(values = c(Women = ACCENT_GIRL, Men = ACCENT_BOY)) +
           scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, .15))) +
           labs(subtitle = "Median monthly wage (UAH, net), wage earners by age and sex", x = NULL, y = NULL, caption = cap) +
           theme_ukr(), "h8_median_wage_en", width = 7.5, height = 4.8)

# h9: childcare access: why young children do not attend preschool ------------------------
h9 <- d %>% filter(!is.na(preschool_reason)) %>% count(preschool_reason, wt = w) %>% mutate(estimate = n / sum(n))
n9 <- sum(!is.na(d$preschool_reason))
write_table(h9, "h9_preschool_reasons")
save_fig(ggplot(h9, aes(reorder(preschool_reason, estimate), estimate)) + geom_col(fill = UNICEF_BLUE, width = .7) +
           geom_text(aes(label = percent(estimate, accuracy = 1)), hjust = -0.2, size = 3.2, colour = GREY_DARK) + coord_flip() +
           scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .2))) +
           labs(subtitle = sprintf("Main reason a child of preschool age does not attend (n = %s children)", n9), x = NULL, y = NULL, caption = cap) +
           theme_ukr() + theme(panel.grid.major.y = element_blank()), "h9_preschool_reasons_en", width = 9.5, height = 5)

# h10: relationships between the key variables, 15-24 -------------------------------------
yy <- d %>% filter(age >= 15, age <= 24, !is.na(sexo)) %>%
  transmute(Sex = sexo, `Age group` = age_group, Oblast = oblast, Rural = rural, Frontline = frontline, IDP = idp,
            `Disability in household` = disab_hh, `Child under 6` = child_u6, `Lives with own child` = own_child, `Poorest 40%` = bottom40,
            Partnered = partnered, Education = educ, `In education` = in_educ, Employed = employed, NEET = neet,
            `Daily childcare` = care_daily, w)
hv <- heatmap_v(yy, setdiff(names(yy), "w"), yy$w, "Strength of association between key variables, 15-24 (0 = none, 1 = perfect)", cap)
write_table(hv$data, "h10_relationships_hsess")
save_fig(hv$plot, "h10_relationships_hsess_en", width = 9, height = 7)

# headline numbers for the slides ------------------------------------------------------------
pick <- function(df, ind, sx, grp = NULL) { r <- df[df$indicator == ind & df$sexo == sx, ]
  if (!is.null(grp)) r <- r[r$group == grp, ]; round(100 * r$estimate[1], 1) }
pk <- function(df, sx, col, val) round(100 * df$estimate[df$sexo == sx & df[[col]] == val][1], 1)
heads <- tibble(
  key = c("educ_w_1524", "educ_m_1524", "higher_w_1924", "higher_m_1924", "mothers_unpartnered", "n_mothers_1924",
          "neet_mother", "neet_nochild", "lfp_w", "lfp_m", "emp_w", "emp_m", "educ_w", "educ_m", "neet_w_1924", "neet_m_1924",
          "neet_w", "neet_m", "neet_w_1529", "neet_m_1529", "educ_w_1518", "educ_m_1518",
          "care_w", "care_m", "inf_w", "inf_m", "gap_1924", "gap_2564", "n_1524", "n_1924"),
  value = c(round(100 * tot0$total[tot0$group == "15-24" & tot0$sex == "Women"], 1), round(100 * tot0$total[tot0$group == "15-24" & tot0$sex == "Men"], 1),
            round(100 * h0$estimate[h0$group == "19-24" & h0$sex == "Women" & h0$level == "Higher education"], 1),
            round(100 * h0$estimate[h0$group == "19-24" & h0$sex == "Men" & h0$level == "Higher education"], 1),
            mothers_unpartnered, sum(n12$n[grepl("with child", n12$mp)]),
            round(100 * weighted.mean(h12$estimate[h12$indicator == "NEET" & grepl("with child", h12$mp)], h12$share[h12$indicator == "NEET" & grepl("with child", h12$mp)]), 1),
            round(100 * weighted.mean(h12$estimate[h12$indicator == "NEET" & grepl("no child", h12$mp)], h12$share[h12$indicator == "NEET" & grepl("no child", h12$mp)]), 1),
            pick(h1, "Labour-force participation", "Young women"), pick(h1, "Labour-force participation", "Young men"),
            pick(h1, "Employed", "Young women"), pick(h1, "Employed", "Young men"),
            pick(h1, "In education", "Young women"), pick(h1, "In education", "Young men"),
            pick(h1, "NEET", "Young women"), pick(h1, "NEET", "Young men"),
            pick(h2, "NEET", "Young women", "15-24"), pick(h2, "NEET", "Young men", "15-24"),
            pick(h2, "NEET", "Young women", "15-29 (ILO NEET frame)"), pick(h2, "NEET", "Young men", "15-29 (ILO NEET frame)"),
            pick(h2, "In education", "Young women", "15-18"), pick(h2, "In education", "Young men", "15-18"),
            pk(h4a, "Young women", "age_group", "19-24"), pk(h4a, "Young men", "age_group", "19-24"),
            round(100 * h6$estimate[h6$sexo == "Young women" & grepl("^Informal", h6$indicator)][1], 1),
            round(100 * h6$estimate[h6$sexo == "Young men" & grepl("^Informal", h6$indicator)][1], 1),
            h8$gap_pct[h8$group == "19-24"][1], h8$gap_pct[h8$group == "25-64"][1],
            sum(d$age >= 15 & d$age <= 24, na.rm = TRUE), sum(d$age >= 19 & d$age <= 24, na.rm = TRUE)))
write_table(heads, "hsess_headlines"); print(heads, n = Inf)
