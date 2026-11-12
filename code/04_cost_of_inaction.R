# 04_cost_of_inaction.R: economic and fiscal cost of young women remaining NEET -----
# Follows the Nepal cost-of-inaction build: survey-based rates and wages, explicit
# scenarios, present values, and national totals anchored on an external population
# figure (COI$pop_w1824). Three scenarios reflect the uncertainty of the current
# context: pessimistic, central and optimistic.
# Inputs: output/mics_women.rds, output/hsess_analysis_ready.rds, output/tables/h19_transition_status.csv
# Output: output/tables/coi_*.csv, output/figures/c*_en.png, output/tables/coi_headlines.csv
source(here::here("code", "00_config.R"))
suppressPackageStartupMessages(library(purrr))

COI <- list(
  pop_w1824 = NA_real_,     # women aged 18-24 in Ukraine, 2026 (national scaling); NA = not yet available
  gdp_uah_bn = NA_real_,    # nominal GDP, UAH billion (to express totals as a share of GDP); NA = not used
  pit = 0.18, levy = 0.05, usc = 0.22, vat = 0.20, consumption_share = 0.80,   # tax parameters, to confirm
  working_life_end = 60     # for the lifetime calculation
)
wm <- readRDS(here::here("output", "mics_women.rds")) %>% filter(age >= 18, age <= 24, !is.na(neet))
hs <- readRDS(here::here("output", "hsess_analysis_ready.rds")) %>% filter(w > 0)
h19 <- readr::read_csv(here::here("output", "tables", "h19_transition_status.csv"), show_col_types = FALSE)

# ---- survey inputs -------------------------------------------------------------------------------
neet_w <- weighted.mean(wm$neet, wm$wmweight)                                   # MICS, women 18-24
neet_w_h <- h19$share[h19$age_group == "18-24" & h19$sexo == "Young women" & h19$status == "NEET"]
neet_m_h <- h19$share[h19$age_group == "18-24" & h19$sexo == "Young men" & h19$status == "NEET"]
gap_share <- max(0, (neet_w_h - neet_m_h) / neet_w_h)                            # share of NEET women that closes the gap with men
wq <- hs %>% filter(!is.na(wage), !is.na(sexo), sexo == "Young women", age >= 18, age <= 24) %>% arrange(wage) %>% mutate(cw = cumsum(w) / sum(w))
wage_at <- function(p) wq$wage[which(wq$cw >= p)[1]]
wages <- c(pessimistic = wage_at(.25), central = wage_at(.5), optimistic = wage_at(.75))
n_wage <- nrow(wq)
if (is.na(COI$pop_w1824) && sum(wm$wmweight) > 1e5) COI$pop_w1824 <- sum(wm$wmweight)   # only if MICS weights are population-scaled

# ---- scenarios ---------------------------------------------------------------------------------------
SC <- tibble(
  scenario = factor(c("Pessimistic", "Central", "Optimistic"), levels = c("Pessimistic", "Central", "Optimistic")),
  wage_month = unname(wages),                         # net monthly wage a NEET woman could earn
  avoidable = c(0.10, gap_share, 0.50),               # share of NEET women who could be in employment
  formal = c(0.60, 0.80, 1.00),                       # share of that employment that is formal (pays taxes and contributions)
  years = c(3, 5, 10),                                # years the employment is sustained
  discount = c(0.07, 0.05, 0.03)) %>%
  mutate(annual_net = 12 * wage_month, gross = annual_net / (1 - COI$pit - COI$levy),
         tax_levy = formal * gross * (COI$pit + COI$levy), usc = formal * gross * COI$usc,
         vat = annual_net * COI$consumption_share * COI$vat / (1 + COI$vat),
         labour_cost = formal * gross * (1 + COI$usc) + (1 - formal) * annual_net,   # output proxy per employed woman
         fiscal = tax_levy + usc + vat,
         pv_factor = map2_dbl(years, discount, ~ sum((1 + .y)^(-(0:(.x - 1))))),
         per1000_women_year = 1000 * avoidable * annual_net, per1000_fiscal_year = 1000 * avoidable * fiscal,
         per1000_output_year = 1000 * avoidable * labour_cost,
         per1000_women_pv = per1000_women_year * pv_factor, per1000_fiscal_pv = per1000_fiscal_year * pv_factor,
         per1000_total_pv = per1000_women_pv + per1000_fiscal_pv)
n_neet_nat <- if (!is.na(COI$pop_w1824)) COI$pop_w1824 * neet_w else NA_real_
SC <- SC %>% mutate(national_women_year = per1000_women_year * n_neet_nat / 1000, national_fiscal_year = per1000_fiscal_year * n_neet_nat / 1000,
                    national_total_pv = per1000_total_pv * n_neet_nat / 1000,
                    pct_gdp_year = if (!is.na(COI$gdp_uah_bn)) 100 * (national_women_year + national_fiscal_year) / (COI$gdp_uah_bn * 1e9) else NA_real_)
write_table(SC, "coi_scenarios")

# lifetime cost per woman who stays NEET to age 60 (central wage, central discount)
ages <- 21:COI$working_life_end; lifetime_pv <- sum(SC$annual_net[2] * (1 + SC$discount[2])^(-(ages - 21)))
lifetime_fiscal_pv <- sum(SC$fiscal[2] * (1 + SC$discount[2])^(-(ages - 21)))

# ---- c1: annual cost per 1,000 NEET young women, by component and scenario -------------------------------
c1 <- SC %>% transmute(scenario, `Net earnings of young women` = 1000 * avoidable * annual_net,
                       `Income tax and levy` = 1000 * avoidable * tax_levy, `Employer social contributions` = 1000 * avoidable * usc,
                       `VAT on consumption` = 1000 * avoidable * vat) %>%
  tidyr::pivot_longer(-scenario, names_to = "component", values_to = "uah") %>%
  mutate(component = factor(component, levels = rev(c("Net earnings of young women", "Income tax and levy", "Employer social contributions", "VAT on consumption"))))
tot1 <- c1 %>% group_by(scenario) %>% summarise(fiscal = sum(uah[component != "Net earnings of young women"]), uah = sum(uah), .groups = "drop")
lab1 <- SC %>% transmute(scenario, lab = sprintf("%s could work\nUAH %s a month\n%s%% formal", percent(avoidable, accuracy = 1), format(wage_month, big.mark = ","), 100 * formal))
save_fig(ggplot(c1, aes(scenario, uah / 1e6, fill = component)) + geom_col(width = .68) +
           geom_text(data = tot1, aes(scenario, uah / 1e6, label = sprintf("UAH %sm a year\nof which fiscal %sm", round(uah / 1e6), round(fiscal / 1e6))), inherit.aes = FALSE, vjust = -0.3, size = 3, colour = GREY_DARK, lineheight = .9) +
           scale_x_discrete(labels = setNames(paste0(lab1$scenario, "\n", lab1$lab), lab1$scenario)) +
           scale_fill_manual(values = c("Net earnings of young women" = ACCENT_GIRL, "Income tax and levy" = UNICEF_DARK, "Employer social contributions" = UNICEF_BLUE, "VAT on consumption" = "#7FC8EE"),
                             breaks = c("Net earnings of young women", "Income tax and levy", "Employer social contributions", "VAT on consumption")) +
           scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, .3))) +
           labs(subtitle = "Annual cost of inaction per 1,000 NEET young women aged 18-24 (UAH million), by scenario", x = NULL, y = NULL,
                caption = "Wages: HSESS 2023/24 wage earners, women 18-24 (25th percentile, median, 75th percentile). Fiscal receipts at statutory rates on the formal share; VAT on 80% of net earnings. Zero earnings assumed while NEET.") +
           theme_ukr() + theme(axis.text.x = element_text(size = 8.5, lineheight = .9)), "c1_cost_of_inaction_scenarios_en", width = 11, height = 5.6)

# ---- c3: present value over the scenario horizon, women's earnings vs government receipts -------------------
c3 <- SC %>% transmute(scenario, years, `Net earnings of young women` = per1000_women_pv, `Government receipts (taxes, levy, contributions, VAT)` = per1000_fiscal_pv) %>%
  tidyr::pivot_longer(-c(scenario, years), names_to = "perspective", values_to = "uah") %>%
  mutate(perspective = factor(perspective, levels = c("Government receipts (taxes, levy, contributions, VAT)", "Net earnings of young women")))
tot3 <- SC %>% transmute(scenario, years, uah = per1000_total_pv, discount)
save_fig(ggplot(c3, aes(scenario, uah / 1e6, fill = perspective)) + geom_col(width = .68) +
           geom_text(data = tot3, aes(scenario, uah / 1e6, label = sprintf("UAH %sm\n%d years, %s%% discount", round(uah / 1e6), years, 100 * discount)), inherit.aes = FALSE, vjust = -0.3, size = 3, colour = GREY_DARK, lineheight = .9) +
           scale_fill_manual(values = c("Net earnings of young women" = ACCENT_GIRL, "Government receipts (taxes, levy, contributions, VAT)" = UNICEF_DARK)) +
           scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, .3))) +
           labs(subtitle = "Present value per 1,000 NEET young women (UAH million) over each scenario's horizon", x = NULL, y = NULL,
                caption = "Pessimistic: 3 years at 7%; central: 5 years at 5%; optimistic: 10 years at 3%. Same wage, employment and formality assumptions as the annual chart.") +
           theme_ukr(), "c3_cost_of_inaction_pv_en", width = 10, height = 5.2)

# ---- c4: where the cost sits: composition of NEET young women, MICS 18-24 ---------------------------------------
G <- c(mother = "Mothers", bottom40 = "Poorest 40%", rural = "Rural", idp = "Internally displaced", single_head = "Single-headed households", disability = "Functional difficulty")
c4 <- map_dfr(names(G), function(g) {
  x <- wm %>% filter(neet == 1, !is.na(.data[[g]]))
  tibble(group = G[[g]], share_of_neet = weighted.mean(x[[g]] == 1, x$wmweight), n = sum(x[[g]] == 1))
}) %>% mutate(annual_fiscal_central = share_of_neet * SC$per1000_fiscal_year[2], annual_total_central = share_of_neet * (SC$per1000_women_year[2] + SC$per1000_fiscal_year[2]))
write_table(c4, "coi_by_group")
save_fig(ggplot(c4, aes(reorder(group, share_of_neet), share_of_neet)) + geom_col(fill = ACCENT_GIRL, width = .65) +
           geom_text(aes(label = sprintf("%s of NEET young women\nUAH %sm a year (central)", percent(share_of_neet, accuracy = 1), round(annual_total_central / 1e6))), hjust = -0.05, size = 2.9, colour = GREY_DARK, lineheight = .9) +
           coord_flip() + scale_y_continuous(labels = percent_format(1), limits = c(0, 1.25), expand = expansion(mult = c(0, 0))) +
           labs(subtitle = "Share of NEET young women aged 18-24 in each group, and their part of the annual cost per 1,000", x = NULL, y = NULL,
                caption = "MICS 2025/26. Groups overlap, so shares do not sum to 100%. Annual cost per 1,000 NEET young women under the central scenario, allocated by the group's share.") +
           theme_ukr() + theme(panel.grid.major.y = element_blank()), "c4_cost_by_group_en", width = 10.5, height = 5)

# ---- headlines ----------------------------------------------------------------------------------------------
heads <- tibble(
  key = c("neet_w", "neet_w_h", "neet_m_h", "gap_share", "wage_pess", "wage_cent", "wage_opt", "n_wage",
          "annual_net_c", "fiscal_year_c", "year_per1000_c", "fiscal_per1000_c", "pv_per1000_pess", "pv_per1000_cent", "pv_per1000_opt",
          "lifetime_pv", "lifetime_fiscal_pv", "pop_w1824", "n_neet_nat", "nat_year_c", "nat_fiscal_year_c", "pct_gdp_c", "share_mothers", "share_poor40"),
  value = c(100 * neet_w, 100 * neet_w_h, 100 * neet_m_h, 100 * gap_share, wages, n_wage,
            SC$annual_net[2], SC$fiscal[2], (SC$per1000_women_year[2] + SC$per1000_fiscal_year[2]) / 1e6, SC$per1000_fiscal_year[2] / 1e6,
            SC$per1000_total_pv[1] / 1e6, SC$per1000_total_pv[2] / 1e6, SC$per1000_total_pv[3] / 1e6,
            lifetime_pv, lifetime_fiscal_pv, COI$pop_w1824, n_neet_nat, (SC$national_women_year[2] + SC$national_fiscal_year[2]) / 1e9,
            SC$national_fiscal_year[2] / 1e9, SC$pct_gdp_year[2], 100 * c4$share_of_neet[c4$group == "Mothers"], 100 * c4$share_of_neet[c4$group == "Poorest 40%"]))
write_table(heads, "coi_headlines"); print(heads, n = Inf)
if (is.na(COI$pop_w1824)) cat("\nNational totals not computed: set COI$pop_w1824 (women aged 18-24, 2026) in code/04_cost_of_inaction.R\n")
