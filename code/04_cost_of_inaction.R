# 04_cost_of_inaction.R: what the gender gap in NEET costs Ukraine ------------------------
# Central question: how much more would the economy and public revenues receive each year if
# young women aged 18-24 were NEET at the same rate as young men, and the additional women in
# work earned what employed women with the same education, age and location earn?
#   Gap ................ NEET rate of women minus NEET rate of men, ages 18-24 (HSESS 2023/24)
#   Population ......... weighted count of women aged 18-24 (HSESS weights, population-scaled)
#   Earnings ........... wage predicted for each NEET young woman by a Mincer equation
#                        estimated on employed women aged 18-64 (HSESS 2023/24)
#   Formality .......... formal share among employed women aged 18-24 (HSESS 2023/24)
#   Economy ............ value of the additional work = what employers pay (gross wage plus
#                        employer social contribution for formal jobs; net wage otherwise)
#   Public revenues .... personal income tax, military levy, employer social contribution (ESV), VAT
#   Budget perspective . 2024 GDP, 2024 public revenues and 2024 preschool spending (same price
#                        year as the HSESS wages, collected Dec 2023 to Feb 2024)
# Output: output/tables/coi_*.csv, output/models/coi_mincer_*.csv, output/figures/c1, c4
source(here::here("code", "00_config.R"))
suppressPackageStartupMessages({ library(purrr); library(fixest) })

COI <- list(
  pop_w1824 = NA_real_,   # override with an official estimate of women aged 18-24; NA = HSESS weighted count
  pit = 0.18, levy = 0.05, usc = 0.22, vat = 0.20, consumption_share = 0.80,   # statutory rates; VAT base assumed
  discount = 0.05, working_life_end = 60,                                      # 5% as in A Brighter Future (UNICEF Ukraine, 2024)
  # Budget perspective, 2024, UAH billion (same year as the HSESS wages); sources in BUDGET below
  gdp_2024 = 7658.7, taxes_2024 = 3100, esv_2024 = 546.1, health_2024 = 238.7, pmg_2024 = 159, education_2024 = 348.4,
  preschool_2024 = 52.6, lowincome_2024 = 17.9
)
hs <- readRDS(here::here("output", "hsess_analysis_ready.rds")) %>% filter(w > 0)
wm <- readRDS(here::here("output", "mics_women.rds")) %>% filter(age >= 18, age <= 24, !is.na(neet))
wmean <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
wq <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]; x[which(cumsum(w) / sum(w) >= p)[1]] }

# ---- 1. The gap and the number of women it represents ----------------------------------------------------
yw <- hs %>% filter(female == 1, age >= 18, age <= 24, !is.na(neet)); ym <- hs %>% filter(female == 0, age >= 18, age <= 24, !is.na(neet))
neet_w <- wmean(yw$neet, yw$w); neet_m <- wmean(ym$neet, ym$w); gap <- neet_w - neet_m
pop_source <- if (is.na(COI$pop_w1824)) "HSESS 2023/24 weighted count" else "User-supplied estimate"
pop <- if (is.na(COI$pop_w1824)) sum(yw$w) else COI$pop_w1824
if (pop < 1e5) stop("HSESS weights are not population-scaled: set COI$pop_w1824")
n_neet_w <- pop * neet_w; n_gap <- pop * gap
neet_mics <- wmean(wm$neet, wm$wmweight)

# ---- 2. Earnings: Mincer equation on employed women, predicted for NEET young women -------------------------
emp <- hs %>% filter(female == 1, !is.na(wage), wage > 0, !is.na(educ), !is.na(rural), age >= 18, age <= 64) %>% mutate(lnw = log(wage), age2 = age^2)
mz <- feols(lnw ~ educ + age + age2 + rural + i(oblast), data = emp, weights = ~w)
smear <- wmean(exp(resid(mz)), emp$w[obs(mz)])
nw <- yw %>% filter(neet == 1, !is.na(educ), !is.na(rural), oblast %in% unique(emp$oblast)) %>% mutate(age2 = age^2)
nw$pot_wage <- exp(predict(mz, newdata = nw)) * smear
wage_c <- wmean(nw$pot_wage, nw$w)
ew <- hs %>% filter(female == 1, !is.na(wage), age >= 18, age <= 24)
wage_p25 <- wq(ew$wage, ew$w, .25); wage_med <- wq(ew$wage, ew$w, .5)
write_table(as.data.frame(coeftable(mz)) %>% tibble::rownames_to_column("term") %>% filter(!grepl("oblast", term)) %>%
              transmute(Term = recode(sub("^educ", "Education: ", term), `(Intercept)` = "Constant", age = "Age", age2 = "Age squared", rural = "Rural"),
                        Coefficient = round(Estimate, 3), SE = round(`Std. Error`, 3), p = ifelse(`Pr(>|t|)` < .001, "<0.001", formatC(`Pr(>|t|)`, format = "f", digits = 3))),
            "coi_mincer_women", sub = "models")

# ---- 3. Formality and other shares used in the sensitivity analysis ---------------------------------------------
eh <- yw %>% filter(employed %in% 1, !is.na(informal)); tert <- eh$educ %in% c("Higher (bachelor+)", "Short-cycle tertiary")
formal_c <- 1 - wmean(eh$informal, eh$w); formal_lo <- 1 - wmean(eh$informal[!tert], eh$w[!tert]); formal_hi <- 1 - wmean(eh$informal[tert], eh$w[tert])
nh <- yw %>% filter(neet == 1)
share_seeking <- wmean(nh$seeking %in% 1, nh$w)

# ---- 4. Value of one additional young woman in work, per year -------------------------------------------------------
per_woman <- function(wage_month, formal) {
  net <- 12 * wage_month; gross <- net / (1 - COI$pit - COI$levy)
  tibble(net = net, pit_levy = formal * gross * (COI$pit + COI$levy), esv = formal * gross * COI$usc,
         vat = net * COI$consumption_share * COI$vat / (1 + COI$vat)) %>%
    mutate(economy = net + pit_levy + esv, revenue = pit_levy + esv + vat)
}
pw <- per_woman(wage_c, formal_c)
central <- pw %>% mutate(women = n_gap, across(c(net, pit_levy, esv, vat, economy, revenue), ~ .x * n_gap / 1e9, .names = "nat_{.col}"))
write_table(central, "coi_central")

# ---- 5. Budget perspective -------------------------------------------------------------------------------------------
BUDGET <- tibble::tribble(
  ~key,         ~comparator,                                                        ~value_bn,          ~measure,          ~source,
  "gdp",        "GDP",                                                              COI$gdp_2024,       "Economy",         "State Statistics Service of Ukraine, GDP estimate for 2024 (31 March 2025)",
  "revenue",    "Public revenues: state budget taxes and fees plus social contributions", COI$taxes_2024 + COI$esv_2024, "Public revenues", "Ministry of Finance, 2024 budget execution: over UAH 3.1 trillion in taxes, fees and other payments (general and special funds) and UAH 546.1 billion in ESV",
  "health",     "Public spending on health",                                        COI$health_2024,    "Public revenues", "Ministry of Finance, 31 January 2025: health spending in 2024 (all budgets)",
  "pmg",        "Programme of Medical Guarantees",                                  COI$pmg_2024,       "Public revenues", "National Health Service of Ukraine, 1 January 2024: budget of the 2024 programme (over UAH 159 billion)",
  "education",  "Public spending on education",                                     COI$education_2024, "Public revenues", "Ministry of Finance: education spending in 2024 (all budgets)",
  "preschool",  "Public spending on preschool education",                           COI$preschool_2024, "Public revenues", "A Brighter Future (UNICEF Ukraine, 2024): amount budgeted for 2024",
  "lowincome",  "State support to low-income families",                             COI$lowincome_2024, "Public revenues", "Ministry of Finance, State Budget 2025 overview: UAH 19.7 billion in 2025, UAH 1.8 billion more than in 2024")
budget <- BUDGET %>% mutate(coi_bn = ifelse(measure == "Economy", central$nat_economy, central$nat_revenue), share = 100 * coi_bn / value_bn)
write_table(budget, "coi_budget")
write_table(budget %>% transmute(`Compared with (2024)` = comparator, `UAH bn` = format(value_bn, big.mark = ",", trim = TRUE),
                                 `Measured against` = ifelse(measure == "Economy", "Economy", "Revenues"),
                                 `Share` = sprintf("%.1f%%", share), Source = source), "coi_budget_table")

# ---- 6. Sensitivity: one assumption at a time ---------------------------------------------------------------------------
sens_row <- function(lab, value_lab, wage, formal, n) { p <- per_woman(wage, formal); tibble(Assumption = lab, Value = value_lab,
  `Women in work` = format(round(n, -2), big.mark = ","), `Economy (UAH bn a year)` = round(p$economy * n / 1e9, 1), `Public revenues (UAH bn a year)` = round(p$revenue * n / 1e9, 1)) }
sens <- bind_rows(
  sens_row("Central estimate", "Gender gap; Mincer wage; formality of employed young women", wage_c, formal_c, n_gap),
  sens_row("Wage", sprintf("25th percentile of employed women 18-24 (UAH %s)", format(round(wage_p25), big.mark = ",")), wage_p25, formal_c, n_gap),
  sens_row("Wage", sprintf("Median of employed women 18-24 (UAH %s)", format(round(wage_med), big.mark = ",")), wage_med, formal_c, n_gap),
  sens_row("Formality", sprintf("%s, women without tertiary education", percent(formal_lo, accuracy = 1)), wage_c, formal_lo, n_gap),
  sens_row("Formality", sprintf("%s, women with tertiary education", percent(formal_hi, accuracy = 1)), wage_c, formal_hi, n_gap),
  sens_row("Women in work", sprintf("Only NEET young women already looking for work (%s of them)", percent(share_seeking, accuracy = 1)), wage_c, formal_c, n_neet_w * share_seeking),
  sens_row("Women in work", sprintf("Gap measured with the MICS NEET rate for women (%s)", percent(neet_mics, accuracy = 0.1)), wage_c, formal_c, pop * (neet_mics - neet_m)))
write_table(sens, "coi_sensitivity")

# ---- 7. Break-even: share of participants who must move into work to recover a given cost -------------------------------
annuity <- function(T) sum((1 + COI$discount)^(-(0:(T - 1))))
be <- tidyr::expand_grid(cost = c(10000, 25000, 50000, 100000), years = c(1, 3)) %>%
  mutate(`Public revenues only` = cost / (pw$revenue * map_dbl(years, annuity)), `Value to the economy` = cost / (pw$economy * map_dbl(years, annuity)))
be_tab <- be %>% transmute(`Cost per participant (UAH)` = format(cost, big.mark = ",", trim = TRUE), `Years in work` = years,
                           `Public revenues only` = percent(pmin(`Public revenues only`, 9.99), accuracy = 1), `Value to the economy` = percent(`Value to the economy`, accuracy = 1))
write_table(be_tab, "coi_break_even")

# ---- 8. Break-even for two existing Ukrainian programmes (2026 terms) ------------------------------------------------------
# Both pay only while the young woman is in work, so the question is additionality: the share of supported women
# who would not have been working without the programme. Break-even share = cost / (annual value x discounted years in work).
#   Youth hiring compensation (State Employment Service): 50% of the minimum wage, UAH 4,324 a month in 2026, for 6 months
#   eYasla: UAH 8,000 a month for a parent who returns to full-time work after the child turns one, until age three (24 months)
# Benefits use 2023/24 wages while costs are 2026 amounts, so the shares are conservative (higher than with 2026 wages).
PROG <- list(yhc_month = 4324, yhc_months = 6, ey_month = 8000, ey_months = 24)
yhc_cost <- PROG$yhc_month * PROG$yhc_months
ey_cost <- sum(rep(PROG$ey_month * 12, PROG$ey_months / 12) * (1 + COI$discount)^(-(0:(PROG$ey_months / 12 - 1))))
progs <- tibble::tribble(
  ~key,    ~Programme,                                                                                  ~cost,     ~years,
  "yhc_1", "Youth hiring compensation: 50% of the minimum wage for 6 months (State Employment Service)",  yhc_cost,  1,
  "yhc_3", "Youth hiring compensation: 50% of the minimum wage for 6 months (State Employment Service)",  yhc_cost,  3,
  "ey_2",  "eYasla: UAH 8,000 a month from the child's first to third birthday (24 months)",             ey_cost,   2,
  "ey_5",  "eYasla: UAH 8,000 a month from the child's first to third birthday (24 months)",             ey_cost,   5) %>%
  mutate(need_rev = cost / (pw$revenue * map_dbl(years, annuity)), need_econ = cost / (pw$economy * map_dbl(years, annuity)))
fmt_need <- function(x) ifelse(x > 1, sprintf("%s: not reachable", percent(x, accuracy = 1)), percent(x, accuracy = 1))
write_table(progs %>% transmute(Programme, `Cost per woman (UAH)` = format(round(cost), big.mark = ",", trim = TRUE), `Years she stays in work` = years,
                                `Public revenues only` = fmt_need(need_rev), `Value to the economy` = fmt_need(need_econ)), "coi_programmes")

# ---- c1: headline chart ------------------------------------------------------------------------------------------------
c1 <- bind_rows(
  tibble(bar = "Economy: value of the additional work", part = c("Net earnings of young women", "Income tax and military levy", "Employer social contribution (ESV)"),
         uah = c(central$nat_net, central$nat_pit_levy, central$nat_esv)),
  tibble(bar = "Public revenues", part = c("Income tax and military levy", "Employer social contribution (ESV)", "VAT on what young women spend"),
         uah = c(central$nat_pit_levy, central$nat_esv, central$nat_vat))) %>%
  mutate(bar = factor(bar, levels = c("Public revenues", "Economy: value of the additional work")),
         part = factor(part, levels = rev(c("Net earnings of young women", "Income tax and military levy", "Employer social contribution (ESV)", "VAT on what young women spend"))))
tot <- c1 %>% group_by(bar) %>% summarise(uah = sum(uah), .groups = "drop")
save_fig(ggplot(c1, aes(uah, bar, fill = part)) + geom_col(width = .55) +
           geom_text(data = tot, aes(uah, bar, label = sprintf("UAH %.1f bn", uah)), inherit.aes = FALSE, hjust = -0.15, size = 4.2, fontface = "bold", colour = GREY_DARK) +
           scale_fill_manual(values = c("Net earnings of young women" = ACCENT_GIRL, "Income tax and military levy" = UNICEF_DARK, "Employer social contribution (ESV)" = UNICEF_BLUE, "VAT on what young women spend" = "#7FC8EE"),
                             breaks = c("Net earnings of young women", "Income tax and military levy", "Employer social contribution (ESV)", "VAT on what young women spend")) +
           scale_x_continuous(labels = label_comma(), expand = expansion(mult = c(0, .2))) + scale_y_discrete(labels = label_wrap(22)) +
           guides(fill = guide_legend(nrow = 2)) +
           labs(subtitle = sprintf("Each year, if about %s more young women aged 18-24 were in work (UAH billion, 2024 prices)", format(round(n_gap, -3), big.mark = ",")), x = NULL, y = NULL,
                caption = "HSESS 2023/24. Women in work: gap between the NEET rates of young women and young men. Earnings: Mincer equation. Taxes at statutory rates on formal jobs.") +
           theme_ukr() + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey92")), "c1_cost_of_gender_gap_en", width = 10.5, height = 4.6)

# ---- c4: who the NEET young women are (MICS 18-24) ------------------------------------------------------------------------
G <- c(mother = "Mothers", bottom40 = "Poorest 40%", rural = "Rural", idp = "Internally displaced", single_head = "Single-headed households", disability = "Functional difficulty")
nm <- wm %>% filter(neet == 1)
c4 <- map_dfr(names(G), function(g) { x <- nm %>% filter(!is.na(.data[[g]])); tibble(group = G[[g]], share_of_neet = wmean(x[[g]] == 1, x$wmweight), n = sum(x[[g]] == 1)) })
share_any <- wmean(rowSums(sapply(c("mother", "bottom40", "rural", "idp", "single_head"), function(v) nm[[v]] %in% 1)) > 0, nm$wmweight)
write_table(c4, "coi_by_group")
save_fig(ggplot(c4, aes(reorder(group, share_of_neet), share_of_neet)) + geom_col(fill = ACCENT_GIRL, width = .65) +
           geom_text(aes(label = sprintf("%s (n = %d)", percent(share_of_neet, accuracy = 1), n)), hjust = -0.1, size = 3.1, colour = GREY_DARK) +
           coord_flip() + scale_y_continuous(labels = percent_format(1), limits = c(0, 1), breaks = seq(0, 1, .25), expand = expansion(mult = c(0, .02))) +
           labs(subtitle = "Share of NEET young women aged 18-24 who belong to each group", x = NULL, y = NULL, caption = "MICS 2025/26 (SSSU and UNICEF), weighted.") +
           theme_ukr() + theme(panel.grid.major.y = element_blank(), plot.caption = element_text(hjust = 0), plot.caption.position = "plot",
                               plot.title.position = "plot", plot.margin = margin(6, 28, 6, 6)), "c4_cost_by_group_en", width = 10, height = 4.8)

# ---- headlines --------------------------------------------------------------------------------------------------------------
ages <- 21:COI$working_life_end; dfac <- sum((1 + COI$discount)^(-(ages - 21)))
heads <- tibble(
  key = c("neet_w_h", "neet_m_h", "gap_pp", "pop_w1824", "n_neet_w", "n_gap", "neet_mics", "wage_c", "wage_p25", "wage_med", "formal_c", "formal_lo", "formal_hi",
          "share_seeking", "n_mincer", "r2_mincer", "n_neet_pred", "n_wage", "pw_net", "pw_economy", "pw_revenue", "nat_economy", "nat_revenue", "nat_net",
          "nat_pit_levy", "nat_esv", "nat_vat", paste0("pct_", budget$key), "lifetime_economy", "lifetime_revenue", "share_mothers", "share_poor40", "share_any",
          "be_25k_1y_rev", "be_25k_3y_rev", "be_100k_3y_rev", "yhc_cost", "ey_cost", paste0("need_rev_", progs$key), paste0("need_econ_", progs$key)),
  value = c(100 * neet_w, 100 * neet_m, 100 * gap, pop, n_neet_w, n_gap, 100 * neet_mics, wage_c, wage_p25, wage_med, 100 * formal_c, 100 * formal_lo, 100 * formal_hi,
            100 * share_seeking, nobs(mz), r2(mz, "r2"), nrow(nw), nrow(ew), pw$net, pw$economy, pw$revenue, central$nat_economy, central$nat_revenue, central$nat_net,
            central$nat_pit_levy, central$nat_esv, central$nat_vat, budget$share, pw$economy * dfac, pw$revenue * dfac,
            100 * c4$share_of_neet[c4$group == "Mothers"], 100 * c4$share_of_neet[c4$group == "Poorest 40%"], 100 * share_any,
            100 * be$`Public revenues only`[be$cost == 25000 & be$years == 1], 100 * be$`Public revenues only`[be$cost == 25000 & be$years == 3],
            100 * be$`Public revenues only`[be$cost == 100000 & be$years == 3], yhc_cost, ey_cost, 100 * progs$need_rev, 100 * progs$need_econ))
write_table(heads, "coi_headlines"); print(heads, n = Inf); print(sens, width = Inf); print(budget)
