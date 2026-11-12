# 04_cost_of_inaction.R: economic and fiscal cost of young women remaining NEET -----
# Every scenario parameter comes from the surveys:
#   Who is counted ........... NEET women aged 18-24 (MICS 2025/26 rate)
#   What they could earn ..... wage predicted for each NEET young woman by a Mincer equation
#                              estimated on employed women (HSESS 2023/24), given her education,
#                              age and place of residence; bounds from the observed wages of
#                              employed women aged 18-24 (25th percentile and median)
#   How many could work ...... pessimistic: those already looking for work;
#                              central: closing the NEET gap with young men;
#                              optimistic: those looking for work or kept out by care duties
#   How many jobs are formal . formal share among employed women aged 18-24: without tertiary
#                              education, all, and with tertiary education
#   Horizon .................. annual cost, and present value over 5 years at 5% for all scenarios
# Results are expressed per 1,000 NEET young women (a reporting unit, not an estimate of their
# number) and per 100,000 women aged 18-24; national totals need COI$pop_w1824.
# Output: output/tables/coi_*.csv, output/figures/c1, c3, c4, output/tables/coi_headlines.csv
source(here::here("code", "00_config.R"))
suppressPackageStartupMessages({ library(purrr); library(fixest) })

COI <- list(
  pop_w1824 = NA_real_,     # women aged 18-24 in Ukraine, 2026 (national scaling); NA = not available
  gdp_uah_bn = NA_real_,    # nominal GDP, UAH billion; NA = not used
  pit = 0.18, levy = 0.05, usc = 0.22, vat = 0.20, consumption_share = 0.80,   # statutory rates; VAT base assumed
  years = 5, discount = 0.05, working_life_end = 60   # 5% social discount rate, as in A Brighter Future (UNICEF Ukraine, 2024)
)
wm <- readRDS(here::here("output", "mics_women.rds")) %>% filter(age >= 18, age <= 24, !is.na(neet))
hs <- readRDS(here::here("output", "hsess_analysis_ready.rds")) %>% filter(w > 0)
h19 <- readr::read_csv(here::here("output", "tables", "h19_transition_status.csv"), show_col_types = FALSE)
wmean <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
wq <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]; x[which(cumsum(w) / sum(w) >= p)[1]] }

# ---- 1. NEET rates ---------------------------------------------------------------------------------
neet_w <- wmean(wm$neet, wm$wmweight)                                           # MICS, women 18-24
neet_w_h <- h19$share[h19$age_group == "18-24" & h19$sexo == "Young women" & h19$status == "NEET"]
neet_m_h <- h19$share[h19$age_group == "18-24" & h19$sexo == "Young men" & h19$status == "NEET"]

# ---- 2. Potential earnings: Mincer equation on employed women, predicted for NEET young women --------
emp <- hs %>% filter(female == 1, !is.na(wage), wage > 0, !is.na(educ), !is.na(rural), age >= 18, age <= 64) %>% mutate(lnw = log(wage), age2 = age^2)
mz <- feols(lnw ~ educ + age + age2 + rural + i(oblast), data = emp, weights = ~w)
smear <- wmean(exp(resid(mz)), emp$w[obs(mz)])                                     # Duan smearing factor for the retransformation
nw <- hs %>% filter(female == 1, age >= 18, age <= 24, neet %in% 1, !is.na(educ), !is.na(rural), oblast %in% unique(emp$oblast)) %>% mutate(age2 = age^2)
nw$pot_wage <- exp(predict(mz, newdata = nw)) * smear
wage_central <- wmean(nw$pot_wage, nw$w)
ew <- hs %>% filter(female == 1, !is.na(wage), age >= 18, age <= 24)
wage_p25 <- wq(ew$wage, ew$w, .25); wage_med <- wq(ew$wage, ew$w, .5)
mincer_tab <- as.data.frame(coeftable(mz)) %>% tibble::rownames_to_column("term") %>% filter(!grepl("oblast", term)) %>%
  transmute(Term = recode(sub("^educ", "Education: ", term), `(Intercept)` = "Constant", age = "Age", age2 = "Age squared", rural = "Rural"),
            Coefficient = round(Estimate, 3), SE = round(`Std. Error`, 3), p = ifelse(`Pr(>|t|)` < .001, "<0.001", formatC(`Pr(>|t|)`, format = "f", digits = 3)))
write_table(mincer_tab, "coi_mincer_women", sub = "models")
write_table(tibble(n_employed_women = nobs(mz), r2 = round(r2(mz, "r2"), 3), smearing = round(smear, 3), n_neet_predicted = nrow(nw),
                   wage_pred_mean = round(wage_central), wage_obs_p25 = wage_p25, wage_obs_median = wage_med), "coi_mincer_summary", sub = "models")

# ---- 3. Share of NEET young women who could work -------------------------------------------------------
nh <- hs %>% filter(female == 1, age >= 18, age <= 24, neet %in% 1)
care_reasons <- c("Maternity or childcare leave", "Housework and caring for children")
share_seeking <- wmean(nh$seeking %in% 1, nh$w)
share_care <- wmean(!(nh$seeking %in% 1) & nh$reason_inactive %in% care_reasons, nh$w)
share_gap <- max(0, (neet_w_h - neet_m_h) / neet_w_h)

# ---- 4. Formal share among employed young women ----------------------------------------------------------
eh <- hs %>% filter(female == 1, age >= 18, age <= 24, employed %in% 1, !is.na(informal))
tert <- eh$educ %in% c("Higher (bachelor+)", "Short-cycle tertiary")
formal_all <- 1 - wmean(eh$informal, eh$w); formal_low <- 1 - wmean(eh$informal[!tert], eh$w[!tert]); formal_high <- 1 - wmean(eh$informal[tert], eh$w[tert])

# ---- scenarios -------------------------------------------------------------------------------------------
pv_factor <- sum((1 + COI$discount)^(-(0:(COI$years - 1))))
ord3 <- function(values, basis) { o <- order(values); list(v = values[o], b = basis[o]) }   # lowest value to the pessimistic scenario
wg <- ord3(c(wage_p25, wage_central, wage_med), c("25th percentile wage of employed women 18-24", "Wage predicted for NEET young women (Mincer equation)", "Median wage of employed women 18-24"))
av <- ord3(c(share_seeking, share_gap, share_seeking + share_care), c("NEET young women already looking for work", "Closing the NEET gap with young men", "Looking for work, or out of work because of care duties"))
fm <- ord3(c(formal_low, formal_all, formal_high), c("Employed women 18-24 without tertiary education", "All employed women 18-24", "Employed women 18-24 with tertiary education"))
SC <- tibble(
  scenario = factor(c("Pessimistic", "Central", "Optimistic"), levels = c("Pessimistic", "Central", "Optimistic")),
  wage_month = wg$v, wage_basis = wg$b, avoidable = av$v, avoidable_basis = av$b, formal = fm$v, formal_basis = fm$b) %>%
  mutate(annual_net = 12 * wage_month, gross = annual_net / (1 - COI$pit - COI$levy),
         tax_levy = formal * gross * (COI$pit + COI$levy), usc = formal * gross * COI$usc,
         vat = annual_net * COI$consumption_share * COI$vat / (1 + COI$vat), fiscal = tax_levy + usc + vat,
         discount = COI$discount, years = COI$years,
         per1000_women_year = 1000 * avoidable * annual_net, per1000_fiscal_year = 1000 * avoidable * fiscal,
         per1000_women_pv = per1000_women_year * pv_factor, per1000_fiscal_pv = per1000_fiscal_year * pv_factor,
         per1000_total_pv = per1000_women_pv + per1000_fiscal_pv,
         per100k_women_year = 100 * neet_w * (per1000_women_year + per1000_fiscal_year))   # per 100,000 women aged 18-24
# National anchor: HSESS weights are scaled to the population of the areas it covers (Dec 2023 to Feb 2024),
# so the weighted count of women aged 18-24 and of NEET women among them gives a national order of magnitude.
# Set COI$pop_w1824 to an official 2026 figure to override (the MICS NEET rate is then applied).
hwo <- hs %>% filter(female == 1, age >= 18, age <= 24, !is.na(neet))
pop_source <- if (is.na(COI$pop_w1824)) "HSESS 2023/24 weighted count" else "User-supplied population"
if (is.na(COI$pop_w1824)) COI$pop_w1824 <- sum(hwo$w)
n_neet_nat <- if (pop_source == "User-supplied population") COI$pop_w1824 * neet_w else sum(hwo$w[hwo$neet == 1])
if (COI$pop_w1824 < 1e5) { COI$pop_w1824 <- NA_real_; n_neet_nat <- NA_real_ }   # weights not population-scaled: no national total
SC <- SC %>% mutate(national_year = (per1000_women_year + per1000_fiscal_year) * n_neet_nat / 1000, national_fiscal_year = per1000_fiscal_year * n_neet_nat / 1000,
                    pct_gdp_year = if (!is.na(COI$gdp_uah_bn)) 100 * national_year / (COI$gdp_uah_bn * 1e9) else NA_real_)
write_table(SC, "coi_scenarios")
# ---- c2: national annual cost by scenario --------------------------------------------------------------
if (!is.na(n_neet_nat)) {
  c2 <- SC %>% transmute(scenario, `Net earnings of young women` = per1000_women_year * n_neet_nat / 1000 / 1e9,
                         `Government receipts (taxes, levy, contributions, VAT)` = per1000_fiscal_year * n_neet_nat / 1000 / 1e9) %>%
    tidyr::pivot_longer(-scenario, names_to = "who", values_to = "uah_bn") %>%
    mutate(who = factor(who, levels = c("Government receipts (taxes, levy, contributions, VAT)", "Net earnings of young women")))
  tot2 <- c2 %>% group_by(scenario) %>% summarise(fiscal = sum(uah_bn[grepl("Government", who)]), uah_bn = sum(uah_bn), .groups = "drop")
  save_fig(ggplot(c2, aes(scenario, uah_bn, fill = who)) + geom_col(width = .62) +
             geom_text(data = tot2, aes(scenario, uah_bn, label = sprintf("UAH %.1f bn a year\nof which budget %.1f bn", uah_bn, fiscal)), inherit.aes = FALSE, vjust = -0.3, size = 3.3, colour = GREY_DARK, lineheight = .9) +
             scale_fill_manual(values = c("Net earnings of young women" = ACCENT_GIRL, "Government receipts (taxes, levy, contributions, VAT)" = UNICEF_DARK)) +
             scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, .25))) +
             labs(subtitle = sprintf("Annual cost of about %s NEET young women aged 18-24 remaining NEET (UAH billion)", format(round(n_neet_nat, -3), big.mark = ",")), x = NULL, y = NULL,
                  caption = "Number of NEET young women: HSESS 2023/24 weighted count for the areas it covers. Scenario parameters from MICS 2025/26 and HSESS 2023/24.") +
             theme_ukr(), "c2_cost_of_inaction_national_en", width = 10, height = 5)
}
ages <- 21:COI$working_life_end
lifetime_pv <- sum(SC$annual_net[2] * (1 + COI$discount)^(-(ages - 21))); lifetime_fiscal_pv <- sum(SC$fiscal[2] * (1 + COI$discount)^(-(ages - 21)))

# ---- c1: annual cost per 1,000 NEET young women -------------------------------------------------------------
c1 <- SC %>% transmute(scenario, `Net earnings of young women` = 1000 * avoidable * annual_net, `Income tax and levy` = 1000 * avoidable * tax_levy,
                       `Employer social contributions` = 1000 * avoidable * usc, `VAT on consumption` = 1000 * avoidable * vat) %>%
  tidyr::pivot_longer(-scenario, names_to = "component", values_to = "uah") %>%
  mutate(component = factor(component, levels = rev(c("Net earnings of young women", "Income tax and levy", "Employer social contributions", "VAT on consumption"))))
tot1 <- c1 %>% group_by(scenario) %>% summarise(fiscal = sum(uah[component != "Net earnings of young women"]), uah = sum(uah), .groups = "drop")
lab1 <- SC %>% transmute(scenario, lab = sprintf("%s of them work\nUAH %s a month\n%s formal", percent(avoidable, accuracy = 1), format(round(wage_month, -2), big.mark = ","), percent(formal, accuracy = 1)))
save_fig(ggplot(c1, aes(scenario, uah / 1e6, fill = component)) + geom_col(width = .68) +
           geom_text(data = tot1, aes(scenario, uah / 1e6, label = sprintf("UAH %sm a year\nof which fiscal %sm", round(uah / 1e6), round(fiscal / 1e6))), inherit.aes = FALSE, vjust = -0.3, size = 3, colour = GREY_DARK, lineheight = .9) +
           scale_x_discrete(labels = setNames(paste0(lab1$scenario, "\n", lab1$lab), lab1$scenario)) +
           scale_fill_manual(values = c("Net earnings of young women" = ACCENT_GIRL, "Income tax and levy" = UNICEF_DARK, "Employer social contributions" = UNICEF_BLUE, "VAT on consumption" = "#7FC8EE"),
                             breaks = c("Net earnings of young women", "Income tax and levy", "Employer social contributions", "VAT on consumption")) +
           scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, .3))) +
           labs(subtitle = "Annual cost per 1,000 NEET young women aged 18-24 (UAH million)", x = NULL, y = NULL,
                caption = "All parameters from MICS 2025/26 and HSESS 2023/24. Fiscal receipts at statutory rates on formal jobs, plus VAT on consumption. Zero earnings assumed while NEET.") +
           theme_ukr() + theme(axis.text.x = element_text(size = 8.5, lineheight = .9)), "c1_cost_of_inaction_scenarios_en", width = 11, height = 5.6)

# ---- c3: present value over 5 years, by who receives it ---------------------------------------------------------
c3 <- SC %>% transmute(scenario, `Net earnings of young women` = per1000_women_pv, `Government receipts (taxes, levy, contributions, VAT)` = per1000_fiscal_pv) %>%
  tidyr::pivot_longer(-scenario, names_to = "perspective", values_to = "uah") %>%
  mutate(perspective = factor(perspective, levels = c("Government receipts (taxes, levy, contributions, VAT)", "Net earnings of young women")))
save_fig(ggplot(c3, aes(scenario, uah / 1e6, fill = perspective)) + geom_col(width = .68) +
           geom_text(data = SC, aes(scenario, per1000_total_pv / 1e6, label = sprintf("UAH %sm", round(per1000_total_pv / 1e6))), inherit.aes = FALSE, vjust = -0.5, size = 3.2, colour = GREY_DARK) +
           scale_fill_manual(values = c("Net earnings of young women" = ACCENT_GIRL, "Government receipts (taxes, levy, contributions, VAT)" = UNICEF_DARK)) +
           scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, .2))) +
           labs(subtitle = sprintf("Present value over %d years per 1,000 NEET young women (UAH million), %s%% discount rate", COI$years, 100 * COI$discount), x = NULL, y = NULL,
                caption = "Same wage, employment and formality assumptions as the annual chart; the three scenarios differ only in those survey-based parameters.") +
           theme_ukr(), "c3_cost_of_inaction_pv_en", width = 10, height = 5.2)

# ---- c4: who the NEET young women are (MICS 18-24) ------------------------------------------------------------------
G <- c(mother = "Mothers", bottom40 = "Poorest 40%", rural = "Rural", idp = "Internally displaced", single_head = "Single-headed households", disability = "Functional difficulty")
nm <- wm %>% filter(neet == 1)
c4 <- map_dfr(names(G), function(g) { x <- nm %>% filter(!is.na(.data[[g]])); tibble(group = G[[g]], share_of_neet = wmean(x[[g]] == 1, x$wmweight), n = sum(x[[g]] == 1)) }) %>%
  mutate(annual_total_central = share_of_neet * (SC$per1000_women_year[2] + SC$per1000_fiscal_year[2]))
share_any <- wmean(rowSums(sapply(c("mother", "bottom40", "rural", "idp", "single_head"), function(v) nm[[v]] %in% 1)) > 0, nm$wmweight)
write_table(c4, "coi_by_group")
save_fig(ggplot(c4, aes(reorder(group, share_of_neet), share_of_neet)) + geom_col(fill = ACCENT_GIRL, width = .65) +
           geom_text(aes(label = sprintf("%s (n = %d)", percent(share_of_neet, accuracy = 1), n)), hjust = -0.1, size = 3.1, colour = GREY_DARK) +
           coord_flip() + scale_y_continuous(labels = percent_format(1), limits = c(0, 1), expand = expansion(mult = c(0, 0))) +
           labs(subtitle = "Share of NEET young women aged 18-24 who belong to each group", x = NULL, y = NULL,
                caption = sprintf("MICS 2025/26. Groups overlap, so shares do not add up to 100%%. %s of NEET young women belong to at least one of the first five groups.", percent(share_any, accuracy = 1))) +
           theme_ukr() + theme(panel.grid.major.y = element_blank()), "c4_cost_by_group_en", width = 10, height = 4.8)

# ---- headlines ------------------------------------------------------------------------------------------------------
heads <- tibble(
  key = c("neet_w", "neet_w_h", "neet_m_h", "share_seeking", "share_gap", "share_care_plus", "wage_pess", "wage_cent", "wage_opt", "n_mincer", "n_neet_pred", "n_wage",
          "formal_low", "formal_all", "formal_high", "annual_net_c", "fiscal_year_c", "year_per1000_pess", "year_per1000_c", "year_per1000_opt", "fiscal_per1000_c",
          "pv_per1000_pess", "pv_per1000_cent", "pv_per1000_opt", "per100k_c", "lifetime_pv", "lifetime_fiscal_pv", "pop_w1824", "n_neet_nat", "nat_year_c", "pct_gdp_c",
          "share_mothers", "share_poor40", "share_any", "avoid_pess", "avoid_cent", "avoid_opt", "formal_pess", "formal_cent", "formal_opt", "r2_mincer",
          "young_per1000", "nat_year_pess", "nat_year_opt", "nat_fiscal_c", "nat_fiscal_pess", "nat_fiscal_opt"),
  value = c(100 * neet_w, 100 * neet_w_h, 100 * neet_m_h, 100 * share_seeking, 100 * share_gap, 100 * (share_seeking + share_care), SC$wage_month, nobs(mz), nrow(nw), nrow(ew),
            100 * formal_low, 100 * formal_all, 100 * formal_high, SC$annual_net[2], SC$fiscal[2],
            (SC$per1000_women_year + SC$per1000_fiscal_year) / 1e6, SC$per1000_fiscal_year[2] / 1e6, SC$per1000_total_pv / 1e6, SC$per100k_women_year[2] / 1e9,
            lifetime_pv, lifetime_fiscal_pv, COI$pop_w1824, n_neet_nat, SC$national_year[2] / 1e9, SC$pct_gdp_year[2],
            100 * c4$share_of_neet[c4$group == "Mothers"], 100 * c4$share_of_neet[c4$group == "Poorest 40%"], 100 * share_any,
            100 * SC$avoidable, 100 * SC$formal, r2(mz, "r2"),
            1000 / neet_w, SC$national_year[1] / 1e9, SC$national_year[3] / 1e9, SC$national_fiscal_year[2] / 1e9, SC$national_fiscal_year[1] / 1e9, SC$national_fiscal_year[3] / 1e9))
write_table(heads, "coi_headlines"); print(heads, n = Inf); print(SC %>% select(scenario, wage_month, avoidable, formal), width = Inf)
if (is.na(COI$pop_w1824)) cat("\nNational totals not computed: set COI$pop_w1824 (women aged 18-24, 2026) in code/04_cost_of_inaction.R\n")

write_table(SC %>% transmute(Scenario = scenario, `Monthly wage (UAH)` = format(round(wage_month), big.mark = ","), `Wage basis` = wage_basis,
                             `Share who work` = percent(avoidable, accuracy = 1), `Basis` = avoidable_basis,
                             `Formal jobs` = percent(formal, accuracy = 1), `Formality basis` = formal_basis), "coi_scenario_table")
