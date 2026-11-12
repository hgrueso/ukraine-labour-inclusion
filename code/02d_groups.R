# 02d_groups.R: barriers by group, poverty and VET (women 18-24) -------------
# Input:  output/mics_*.rds, output/hsess_analysis_ready.rds
# Output: output/figures/g*_en.png, output/tables/g*.csv, output/tables/groups_headlines.csv
source(here::here("code", "00_config.R")); source(here::here("R", "plots.R"))
suppressPackageStartupMessages({ library(purrr); library(fixest) })

wm <- readRDS(here::here("output", "mics_women.rds")); hl <- readRDS(here::here("output", "mics_members.rds"))
hs <- readRDS(here::here("output", "hsess_analysis_ready.rds")) %>% filter(w > 0)
capm <- "MICS 2025/26 (SSSU and UNICEF), weighted, 95% CIs accounting for the survey design."
caph <- "UNICEF HSESS 2023/24, weighted, 95% CIs clustered by household."
dwm <- svydesign(ids = ~psu, strata = ~stratum, weights = ~wmweight, data = wm, nest = TRUE)
dhs <- svydesign(ids = ~key, weights = ~w, data = hs)
mw <- subset(dwm, age >= 18 & age <= 24)
hw <- subset(dhs, age >= 18 & age <= 24 & sexo == "Young women")
hm <- subset(dhs, age >= 18 & age <= 24 & sexo == "Young men")

by_group <- function(design, y, groups) map_dfr(names(groups), function(g) {
  if (!g %in% names(design$variables) || all(is.na(design$variables[[g]]))) return(NULL)
  dd <- design[!is.na(design$variables[[g]]), ]
  if (length(unique(dd$variables[[g]])) < 2) return(NULL)
  wt_pct(dd, y, g) %>% rename(group = !!g) %>%
    mutate(dimension = groups[[g]], group = ifelse(group == 1, "Yes", "No"),
           n = as.integer(table(dd$variables[[g]])[ifelse(group == "Yes", "1", "0")]))
}) %>% mutate(group = factor(group, levels = c("No", "Yes")), dimension = factor(dimension, levels = unname(groups)), xlab = paste0(group, "\n(n=", n, ")"))
bars_groups <- function(d, sub, cap, fill = ACCENT_GIRL, nrow = 1) {
  ggplot(d, aes(xlab, estimate)) + geom_col(fill = fill, width = .65) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = .15, colour = GREY_DARK, linewidth = .4) +
    geom_text(aes(y = hi, label = percent(estimate, accuracy = 1)), vjust = -0.8, size = 3, colour = GREY_DARK) +
    facet_wrap(~dimension, nrow = nrow, scales = "free_x", labeller = label_wrap_gen(16)) +
    scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .2))) +
    labs(subtitle = sub, x = NULL, y = NULL, caption = cap) + theme_ukr() + theme(axis.text.x = element_text(size = 8))
}

# g1: MICS, NEET among women 18-24 by group ------------------------------------------------
GM <- c(mother = "Mother", single_head = "Single-headed household", bottom20 = "Poorest 20% (wealth)", bottom40 = "Poorest 40% (wealth)",
        rural = "Rural", idp = "Internally displaced", disability = "Functional difficulty")
g1 <- by_group(mw, "neet", GM); write_table(g1, "g1_mics_neet_by_group_1824")
save_fig(bars_groups(g1, "NEET among women aged 18-24, by group (n = unweighted sample)", capm), "g1_mics_neet_by_group_en", width = 13, height = 4.8)

# g2: HSESS, NEET among women and men 18-24 by group ------------------------------------------
GH <- c(own_child = "Lives with own child", single_head = "Single-headed household", poor_rel = "Below 60% of median income",
        bottom40 = "Poorest 40% (income)", rural = "Rural", idp = "Internally displaced", disab_hh = "Disability in household")
g2 <- bind_rows(by_group(hw, "neet", GH) %>% mutate(sexo = "Young women"), by_group(hm, "neet", GH) %>% mutate(sexo = "Young men")) %>%
  mutate(sexo = factor(sexo, levels = c("Young women", "Young men")))
write_table(g2, "g2_hsess_neet_by_group_1824")
save_fig(ggplot(g2, aes(group, estimate, fill = sexo)) + geom_col(position = position_dodge(.75), width = .7) +
           geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(.75), width = .16, colour = GREY_DARK, linewidth = .4) +
           geom_text(aes(y = hi, label = percent(estimate, accuracy = 1)), position = position_dodge(.75), vjust = -0.8, size = 2.8, colour = GREY_DARK) +
           facet_wrap(~dimension, nrow = 1, labeller = label_wrap_gen(16)) + scale_fill_manual(values = FILL_SEX) +
           scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .2))) +
           labs(subtitle = "NEET at 18-24 by group and sex", x = NULL, y = NULL, caption = caph) + theme_ukr(), "g2_hsess_neet_by_group_en", width = 13, height = 4.8)

# g3: multiple disadvantage: NEET by number of disadvantages, women 18-24 --------------------------
count_dis <- function(df, vars) rowSums(sapply(vars, function(v) df[[v]] %in% 1))
wm_c <- wm %>% filter(age >= 18, age <= 24) %>% mutate(ndis = pmin(count_dis(., c("mother", "bottom40", "rural", "idp", "single_head")), 3),
                                                        ndis = factor(ndis, levels = 0:3, labels = c("0", "1", "2", "3 or more")))
hs_c <- hs %>% filter(age >= 18, age <= 24, sexo == "Young women") %>% mutate(ndis = pmin(count_dis(., c("own_child", "bottom40", "rural", "idp", "single_head")), 3),
                                                                             ndis = factor(ndis, levels = 0:3, labels = c("0", "1", "2", "3 or more")))
g3 <- bind_rows(
  wt_pct(svydesign(ids = ~psu, strata = ~stratum, weights = ~wmweight, data = wm_c, nest = TRUE), "neet", "ndis") %>%
    left_join(count(wm_c, ndis), by = "ndis") %>% mutate(source = "MICS 2025/26"),
  wt_pct(svydesign(ids = ~key, weights = ~w, data = hs_c), "neet", "ndis") %>% left_join(count(hs_c, ndis), by = "ndis") %>% mutate(source = "HSESS 2023/24")) %>%
  mutate(xlab = paste0(ndis, "\n(n=", n, ")"), source = factor(source, levels = c("MICS 2025/26", "HSESS 2023/24")))
write_table(g3, "g3_neet_by_number_of_disadvantages")
save_fig(ggplot(g3, aes(ndis, estimate)) + geom_col(fill = ACCENT_GIRL, width = .65) +
           geom_errorbar(aes(ymin = lo, ymax = hi), width = .15, colour = GREY_DARK, linewidth = .4) +
           geom_text(aes(y = hi, label = percent(estimate, accuracy = 1)), vjust = -0.8, size = 3, colour = GREY_DARK) +
           geom_text(aes(y = 0, label = paste0("n=", n)), vjust = 1.6, size = 2.6, colour = GREY_MID) +
           facet_wrap(~source) + scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(.08, .2))) +
           labs(subtitle = "NEET among women aged 18-24 by number of disadvantages (motherhood, poorest 40%, rural, displaced, single-headed household)",
                x = "Number of disadvantages", y = NULL, caption = paste(capm, caph)) + theme_ukr(), "g3_multiple_disadvantage_en", width = 10, height = 5)

# g4: poverty and motherhood: NEET among women 18-24 in four groups, both surveys -------------------
pm_lab <- function(poor, mother) factor(paste0(ifelse(poor == 1, "Poorest 40%", "Other households"), ", ", ifelse(mother == 1, "mother", "no child")),
                                        levels = c("Other households, no child", "Poorest 40%, no child", "Other households, mother", "Poorest 40%, mother"))
mw4 <- update(subset(mw, !is.na(bottom40) & !is.na(mother)), pm = pm_lab(bottom40, mother))
hw4 <- update(subset(hw, !is.na(bottom40) & !is.na(own_child)), pm = pm_lab(bottom40, own_child))
g4 <- bind_rows(wt_pct(mw4, "neet", "pm") %>% left_join(count(mw4$variables, pm), by = "pm") %>% mutate(source = "MICS 2025/26 (wealth)"),
                wt_pct(hw4, "neet", "pm") %>% left_join(count(hw4$variables, pm), by = "pm") %>% mutate(source = "HSESS 2023/24 (income)")) %>%
  mutate(source = factor(source, levels = c("MICS 2025/26 (wealth)", "HSESS 2023/24 (income)")))
comp <- bind_rows(wm_c %>% filter(neet == 1) %>% summarise(source = "MICS 2025/26", share = weighted.mean(bottom40 == 1 & mother == 1, wmweight, na.rm = TRUE)),
                  hs_c %>% filter(neet == 1) %>% summarise(source = "HSESS 2023/24", share = weighted.mean(bottom40 == 1 & own_child == 1, w, na.rm = TRUE)))
write_table(g4, "g4_neet_poverty_motherhood"); write_table(comp, "g4b_share_of_neet_poor_mothers")
save_fig(ggplot(g4, aes(pm, estimate, fill = pm)) + geom_col(width = .7) +
           geom_errorbar(aes(ymin = lo, ymax = hi), width = .16, colour = GREY_DARK, linewidth = .4) +
           geom_text(aes(y = hi, label = percent(estimate, accuracy = 1)), vjust = -0.8, size = 3, colour = GREY_DARK) +
           geom_text(aes(y = 0, label = paste0("n=", n)), vjust = 1.6, size = 2.6, colour = GREY_MID) +
           facet_wrap(~source) + scale_fill_manual(values = c("#F7B6D6", "#EE6FAE", "#B5005F", "#6E003A"), guide = "none") +
           scale_x_discrete(labels = label_wrap(14)) + scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(.08, .2))) +
           labs(subtitle = "NEET among women aged 18-24, by household economic status and motherhood", x = NULL, y = NULL, caption = paste(capm, caph)) +
           theme_ukr() + theme(axis.text.x = element_text(size = 8)), "g4_poverty_motherhood_en", width = 11, height = 5)

# g5: child poverty: children in households with a NEET young woman -------------------------------------
neet_hh <- wm %>% filter(age >= 18, age <= 24, !is.na(neet)) %>% group_by(HH1, HH2) %>% summarise(hh_neet_w = as.integer(any(neet == 1)), .groups = "drop")
ch <- hl %>% filter(age < 18) %>% left_join(neet_hh, by = c("HH1", "HH2")) %>% filter(!is.na(hh_neet_w)) %>%
  mutate(grp = ifelse(hh_neet_w == 1, "Household with a NEET woman 18-24", "Household with a non-NEET woman 18-24"))
dch <- svydesign(ids = ~psu, strata = ~stratum, weights = ~hhweight, data = ch, nest = TRUE)
g5 <- bind_rows(wt_pct(dch, "bottom20", "grp") %>% mutate(indicator = "Poorest 20%"), wt_pct(dch, "bottom40", "grp") %>% mutate(indicator = "Poorest 40%")) %>%
  left_join(count(ch, grp), by = "grp")
write_table(g5, "g5_child_poverty_neet_households")
save_fig(ggplot(g5, aes(indicator, estimate, fill = grp)) + geom_col(position = position_dodge(.75), width = .7) +
           geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(.75), width = .16, colour = GREY_DARK, linewidth = .4) +
           geom_text(aes(y = hi, label = percent(estimate, accuracy = 1)), position = position_dodge(.75), vjust = -0.8, size = 3, colour = GREY_DARK) +
           scale_fill_manual(values = c("Household with a NEET woman 18-24" = ACCENT_GIRL, "Household with a non-NEET woman 18-24" = GREY_MID)) +
           scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .2))) +
           labs(subtitle = sprintf("Children under 18 living in households with a young woman aged 18-24: share in the poorest wealth groups (n = %s and %s children)",
                                   g5$n[g5$grp == "Household with a NEET woman 18-24"][1], g5$n[g5$grp == "Household with a non-NEET woman 18-24"][1]),
                x = NULL, y = NULL, caption = capm) + theme_ukr(), "g5_child_poverty_en", width = 9.5, height = 4.8)

# g6: VET profile, HSESS women and men 18-29 ------------------------------------------------------------
v <- hs %>% filter(age >= 18, age <= 29, !is.na(educ), !is.na(sexo)) %>%
  mutate(ed3 = case_when(educ == "Vocational" ~ "Vocational", educ == "Complete secondary" ~ "Complete secondary",
                         educ %in% c("Higher (bachelor+)", "Short-cycle tertiary") ~ "Tertiary", TRUE ~ NA_character_)) %>% filter(!is.na(ed3)) %>%
  mutate(ed3 = factor(ed3, levels = c("Complete secondary", "Vocational", "Tertiary")))
dv <- svydesign(ids = ~key, weights = ~w, data = v)
IND6 <- c(neet = "NEET", employed = "Employed", informal = "Informal (employed)", rural = "Rural", own_child = "Lives with own child", in_educ = "In education")
g6 <- map_dfr(names(IND6), ~ wt_pct(dv, .x, c("sexo", "ed3")) %>% mutate(indicator = IND6[[.x]])) %>%
  mutate(indicator = factor(indicator, levels = unname(IND6)))
write_table(g6, "g6_vet_profile")
save_fig(ggplot(g6, aes(ed3, estimate, fill = sexo)) + geom_col(position = position_dodge(.75), width = .7) +
           geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(.75), width = .16, colour = GREY_DARK, linewidth = .4) +
           geom_text(aes(y = hi, label = percent(estimate, accuracy = 1)), position = position_dodge(.75), vjust = -0.8, size = 2.6, colour = GREY_DARK) +
           facet_wrap(~indicator, nrow = 2, scales = "free_y") + scale_fill_manual(values = FILL_SEX) + scale_x_discrete(labels = label_wrap(10)) +
           scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .25))) +
           labs(subtitle = "People aged 18-29 by highest level of education and sex", x = NULL, y = NULL, caption = caph) +
           theme_ukr() + theme(axis.text.x = element_text(size = 8)), "g6_vet_profile_en", width = 11, height = 6.2)
# sectors of employed VET graduates, women vs men
sec <- v %>% filter(employed == 1, ed3 == "Vocational", !is.na(sector)) %>% count(sexo, sector, wt = w) %>% group_by(sexo) %>%
  mutate(share = n / sum(n)) %>% slice_max(share, n = 6) %>% ungroup()
write_table(sec, "g6b_vet_sectors")
save_fig(ggplot(sec, aes(reorder(sector, share), share, fill = sexo)) + geom_col(width = .7) + coord_flip() + facet_wrap(~sexo, scales = "free_y") +
           geom_text(aes(label = percent(share, accuracy = 1)), hjust = -0.2, size = 3, colour = GREY_DARK) +
           scale_fill_manual(values = FILL_SEX, guide = "none") + scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .25))) +
           labs(subtitle = "Main sectors of employed vocational graduates aged 18-29, by sex", x = NULL, y = NULL, caption = caph) +
           theme_ukr() + theme(panel.grid.major.y = element_blank()), "g6b_vet_sectors_en", width = 10, height = 4.8)
# g7: VET effect on NEET among women 18-29, before and after controls
vw <- v %>% filter(sexo == "Young women", !is.na(neet)) %>% mutate(vet = as.integer(ed3 == "Vocational"), tert = as.integer(ed3 == "Tertiary"))
specs <- list(`No controls` = "neet ~ vet + tert", `+ age, area` = "neet ~ vet + tert + age + rural", `+ motherhood, partnership` = "neet ~ vet + tert + age + rural + own_child + partnered",
              `+ income, displacement, disability` = "neet ~ vet + tert + age + rural + own_child + partnered + bottom40 + idp + disab_hh")
g7 <- imap_dfr(specs, function(f, nm) { m <- feols(as.formula(paste(f, "| oblast")), data = vw, weights = ~w, cluster = ~key); ct <- coeftable(m)
  tibble(spec = nm, estimate = 100 * ct["vet", 1], lo = 100 * (ct["vet", 1] - 1.96 * ct["vet", 2]), hi = 100 * (ct["vet", 1] + 1.96 * ct["vet", 2]), N = nobs(m)) }) %>%
  mutate(spec = factor(spec, levels = names(specs)))
write_table(g7, "g7_vet_effect_controls")
save_fig(ggplot(g7, aes(spec, estimate)) + geom_hline(yintercept = 0, linetype = 2, colour = GREY_MID) +
           geom_pointrange(aes(ymin = lo, ymax = hi), colour = ACCENT_GIRL, size = .6) +
           geom_text(aes(label = sprintf("%+.0f pp", estimate)), vjust = -1.2, size = 3, colour = GREY_DARK) +
           scale_x_discrete(labels = label_wrap(16)) + scale_y_continuous(labels = function(x) paste0(round(x), " pp")) +
           labs(subtitle = "Difference in NEET between women with vocational and with complete secondary education, aged 18-29, as controls are added",
                x = NULL, y = NULL, caption = paste(caph, "Linear probability models with oblast fixed effects; 95% CI.")) +
           theme_ukr() + theme(panel.grid.major.y = element_line(colour = "grey92")), "g7_vet_effect_controls_en", width = 10, height = 4.8)

# headlines ---------------------------------------------------------------------------------------
gh <- function(df, dim, grp) round(100 * df$estimate[df$dimension == dim & df$group == grp][1], 1)
heads <- tibble(key = c("g1_mother_yes", "g1_mother_no", "g1_single_yes", "g1_single_no", "g1_poor20_yes", "g1_poor20_no", "g1_idp_yes", "g1_idp_no",
                        "g2_poor_yes_w", "g2_poor_no_w", "g2_single_yes_w", "g2_single_no_w", "g3_mics_3", "g3_mics_0", "g3_hsess_3", "g3_hsess_0",
                        "g4_mics_poor_mother", "g4_mics_other_nochild", "g4_hsess_poor_mother", "g4_hsess_other_nochild", "comp_mics", "comp_hsess",
                        "g5_poor40_neet", "g5_poor40_other", "g7_raw", "g7_full", "vet_neet_w", "sec_neet_w", "tert_neet_w"),
                value = c(gh(g1, "Mother", "Yes"), gh(g1, "Mother", "No"), gh(g1, "Single-headed household", "Yes"), gh(g1, "Single-headed household", "No"),
                          gh(g1, "Poorest 20% (wealth)", "Yes"), gh(g1, "Poorest 20% (wealth)", "No"), gh(g1, "Internally displaced", "Yes"), gh(g1, "Internally displaced", "No"),
                          gh(filter(g2, sexo == "Young women"), "Below 60% of median income", "Yes"), gh(filter(g2, sexo == "Young women"), "Below 60% of median income", "No"),
                          gh(filter(g2, sexo == "Young women"), "Single-headed household", "Yes"), gh(filter(g2, sexo == "Young women"), "Single-headed household", "No"),
                          round(100 * g3$estimate[g3$source == "MICS 2025/26" & g3$ndis == "3 or more"][1], 1), round(100 * g3$estimate[g3$source == "MICS 2025/26" & g3$ndis == "0"][1], 1),
                          round(100 * g3$estimate[g3$source == "HSESS 2023/24" & g3$ndis == "3 or more"][1], 1), round(100 * g3$estimate[g3$source == "HSESS 2023/24" & g3$ndis == "0"][1], 1),
                          round(100 * g4$estimate[grepl("MICS", g4$source) & g4$pm == "Poorest 40%, mother"][1], 1), round(100 * g4$estimate[grepl("MICS", g4$source) & g4$pm == "Other households, no child"][1], 1),
                          round(100 * g4$estimate[grepl("HSESS", g4$source) & g4$pm == "Poorest 40%, mother"][1], 1), round(100 * g4$estimate[grepl("HSESS", g4$source) & g4$pm == "Other households, no child"][1], 1),
                          round(100 * comp$share[comp$source == "MICS 2025/26"], 1), round(100 * comp$share[comp$source == "HSESS 2023/24"], 1),
                          round(100 * g5$estimate[g5$indicator == "Poorest 40%" & grepl("a NEET", g5$grp)][1], 1), round(100 * g5$estimate[g5$indicator == "Poorest 40%" & grepl("non-NEET", g5$grp)][1], 1),
                          round(g7$estimate[1], 1), round(g7$estimate[nrow(g7)], 1),
                          round(100 * g6$estimate[g6$indicator == "NEET" & g6$sexo == "Young women" & g6$ed3 == "Vocational"][1], 1),
                          round(100 * g6$estimate[g6$indicator == "NEET" & g6$sexo == "Young women" & g6$ed3 == "Complete secondary"][1], 1),
                          round(100 * g6$estimate[g6$indicator == "NEET" & g6$sexo == "Young women" & g6$ed3 == "Tertiary"][1], 1)))
write_table(heads, "groups_headlines"); print(heads, n = Inf)
