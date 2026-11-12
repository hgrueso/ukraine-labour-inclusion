# 05_interventions.R: illustrative comparison of intervention types -----------
# Not a programme model: each intervention is described by a cost per participant
# (present value of total cost over the support period) and a range of additional,
# sustained employment it could produce, taken from code/intervention_parameters.csv.
# Benefits per participant use the same earnings and fiscal parameters as the cost of
# inaction (central scenario). Returns are shown for three perspectives:
# young women's net earnings, government receipts, and both together.
# Output: output/tables/interventions_bcr.csv, output/figures/c5_interventions_en.png, c6_break_even_en.png
source(here::here("code", "00_config.R"))
suppressPackageStartupMessages(library(purrr))

P <- readr::read_csv(here::here("code", "intervention_parameters.csv"), show_col_types = FALSE)
SC <- readr::read_csv(here::here("output", "tables", "coi_scenarios.csv"), show_col_types = FALSE) %>% filter(scenario == "Central")
annual_net <- SC$annual_net; fiscal <- SC$fiscal; r <- SC$discount
annuity <- function(T, r) sum((1 + r)^(-(0:(T - 1))))

bcr <- P %>% mutate(pv = map_dbl(years, annuity, r = r),
                    ben_earn_low = effect_low * annual_net * pv, ben_earn_high = effect_high * annual_net * pv,
                    ben_fisc_low = effect_low * fiscal * pv, ben_fisc_high = effect_high * fiscal * pv) %>%
  mutate(`Net earnings perspective: low` = ben_earn_low / cost_high, `Net earnings perspective: high` = ben_earn_high / cost_low,
         `Government receipts: low` = ben_fisc_low / cost_high, `Government receipts: high` = ben_fisc_high / cost_low,
         `Earnings plus receipts: low` = (ben_earn_low + ben_fisc_low) / cost_high, `Earnings plus receipts: high` = (ben_earn_high + ben_fisc_high) / cost_low)
write_table(bcr, "interventions_bcr")

long <- bcr %>% select(intervention, target_group, ends_with(": low"), ends_with(": high")) %>%
  tidyr::pivot_longer(-c(intervention, target_group), names_to = c("perspective", "bound"), names_sep = ": ", values_to = "bcr") %>%
  tidyr::pivot_wider(names_from = bound, values_from = bcr) %>%
  mutate(perspective = factor(perspective, levels = c("Net earnings perspective", "Government receipts", "Earnings plus receipts")),
         intervention = factor(intervention, levels = rev(P$intervention)))
save_fig(ggplot(long, aes(y = intervention)) + geom_vline(xintercept = 1, linetype = 2, colour = GREY_MID) +
           geom_segment(aes(x = low, xend = high, yend = intervention), colour = ACCENT_GIRL, linewidth = 2.2, alpha = .5) +
           geom_point(aes(x = low), colour = ACCENT_GIRL, size = 2.4) + geom_point(aes(x = high), colour = "#8A0049", size = 2.4) +
           geom_text(aes(x = high, label = sprintf("%.1f to %.1f", low, high)), hjust = -0.2, size = 2.9, colour = GREY_DARK) +
           facet_wrap(~perspective, nrow = 1, scales = "free_x") + scale_y_discrete(labels = label_wrap(26)) +
           scale_x_continuous(expand = expansion(mult = c(.05, .45))) +
           labs(subtitle = "Benefits per UAH of cost (range from low effect and high cost to high effect and low cost); dashed line = break-even", x = "Benefit-cost ratio", y = NULL,
                caption = "Costs: existing Ukrainian programme amounts where available (see appendix). Effects: evaluations in other countries. Benefits: additional employment sustained for 3 years at the central-scenario wage and fiscal rates, 5% discount. Employer returns not included.") +
           theme_ukr(11) + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey92")), "c5_interventions_en", width = 12, height = 5)

# break-even employment effect needed for each cost level (3 years), by perspective
be <- tidyr::expand_grid(cost = seq(5000, 150000, by = 5000)) %>%
  mutate(`Net earnings perspective` = cost / (annual_net * annuity(3, r)), `Earnings plus receipts` = cost / ((annual_net + fiscal) * annuity(3, r))) %>%
  tidyr::pivot_longer(-cost, names_to = "perspective", values_to = "effect_needed")
write_table(be, "coi_break_even")
pts <- P %>% transmute(intervention, cost = (cost_low + cost_high) / 2, effect = (effect_low + effect_high) / 2)
save_fig(ggplot(be, aes(cost / 1000, effect_needed, colour = perspective)) + geom_line(linewidth = 1) +
           geom_point(data = pts, aes(cost / 1000, effect), inherit.aes = FALSE, colour = UNICEF_DARK, size = 2.6) +
           geom_text(data = pts, aes(cost / 1000, effect, label = str_wrap(intervention, 22)), inherit.aes = FALSE, hjust = -0.08, vjust = -0.2, size = 2.6, colour = UNICEF_DARK, lineheight = .85) +
           scale_colour_manual(values = c("Net earnings perspective" = ACCENT_GIRL, "Earnings plus receipts" = "#8A0049")) +
           scale_y_continuous(labels = percent_format(1), limits = c(0, .6)) + scale_x_continuous(expand = expansion(mult = c(.02, .25))) +
           labs(subtitle = "Additional employment (share of participants, sustained 3 years) needed to cover the cost per participant; dots = illustrative midpoints", x = "Present value of total programme cost per participant (UAH thousand)", y = NULL,
                caption = "Lines: break-even effect at the central-scenario wage and fiscal rates, 5% discount. Dots: midpoints of the cost and effect ranges in code/intervention_parameters.csv.") +
           theme_ukr() + theme(panel.grid.major.x = element_line(colour = "grey92")), "c6_break_even_en", width = 11, height = 5.2)
print(bcr %>% select(intervention, target_group, starts_with("Earnings plus")), width = Inf)

# ---- c7: break-even applications by intervention: employment gain needed at each cost level and duration ----
app <- tidyr::expand_grid(P %>% select(intervention, target_group, cost_low, cost_high, effect_low, effect_high), yrs = c(1, 3, 5)) %>% rename(years = yrs) %>%
  mutate(pv = map_dbl(years, annuity, r = r),
         need_low_cost_earn = cost_low / (annual_net * pv), need_high_cost_earn = cost_high / (annual_net * pv),
         need_low_cost_total = cost_low / ((annual_net + fiscal) * pv), need_high_cost_total = cost_high / ((annual_net + fiscal) * pv))
write_table(app, "interventions_break_even_applications")
tab7 <- app %>% transmute(Intervention = intervention, `Target group` = target_group, Years = years,
                          `Cost per participant (UAH)` = sprintf("%s to %s", format(cost_low, big.mark = ","), format(cost_high, big.mark = ",")),
                          `Needed, net earnings` = sprintf("%s to %s", percent(need_low_cost_earn, accuracy = 1), percent(need_high_cost_earn, accuracy = 1)),
                          `Needed, plus receipts` = sprintf("%s to %s", percent(need_low_cost_total, accuracy = 1), percent(need_high_cost_total, accuracy = 1)),
                          `Evidence range` = sprintf("%s to %s", percent(effect_low, accuracy = 1), percent(effect_high, accuracy = 1)))
write_table(tab7, "interventions_break_even_table")
a3 <- app %>% filter(years == 3) %>% mutate(intervention = factor(intervention, levels = rev(P$intervention)))
save_fig(ggplot(a3) +
           geom_segment(aes(x = need_low_cost_total, xend = need_high_cost_total, y = intervention, yend = intervention), colour = UNICEF_DARK, linewidth = 6, alpha = .35) +
           geom_segment(aes(x = effect_low, xend = effect_high, y = as.numeric(intervention) + .22, yend = as.numeric(intervention) + .22), colour = ACCENT_GIRL, linewidth = 3) +
           geom_text(aes(x = need_high_cost_total, y = intervention, label = sprintf("needed: %s to %s", percent(need_low_cost_total, accuracy = 1), percent(need_high_cost_total, accuracy = 1))), hjust = -0.08, size = 2.8, colour = UNICEF_DARK) +
           geom_text(aes(x = effect_high, y = as.numeric(intervention) + .22, label = sprintf("evidence: %s to %s", percent(effect_low, accuracy = 1), percent(effect_high, accuracy = 1))), hjust = -0.08, size = 2.8, colour = ACCENT_GIRL) +
           scale_y_discrete(labels = label_wrap(26)) + scale_x_continuous(labels = percent_format(1), expand = expansion(mult = c(.02, .6))) +
           labs(subtitle = "Employment gain needed to cover the cost (blue) and effects found in evaluations (pink), 3 years",
                x = "Additional employment among participants", y = NULL,
                caption = "Break-even counting net earnings and government receipts, central-scenario wage and fiscal rates, 5% discount. Costs from Ukrainian programme amounts; effects from evaluations in other countries (sources in the appendix). Pink to the right of blue: pays off; overlap: depends on cost.") +
           theme_ukr(11) + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey92")), "c7_break_even_by_intervention_en", width = 12, height = 5.2)

write_table(P %>% transmute(Intervention = intervention, `Cost per participant (UAH)` = sprintf("%s to %s", format(cost_low, big.mark = ","), format(cost_high, big.mark = ",")),
                            `Cost basis` = cost_basis, `Employment effect` = sprintf("%s to %s pp", format(100 * effect_low), format(100 * effect_high)), `Evidence` = effect_basis), "interventions_sources")
