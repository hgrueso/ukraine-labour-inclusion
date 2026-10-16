# R/plots.R: survey shares and the East Java bar style, shared by the figure scripts
suppressPackageStartupMessages({ library(survey); library(dplyr); library(ggplot2); library(scales) })
options(survey.lonely.psu = "adjust")

# Weighted share of a 0/1 variable with 95% CI, by groups
wt_pct <- function(design, var, by) {
  f <- as.formula(paste0("~", var)); b <- as.formula(paste0("~", paste(by, collapse = "+")))
  r <- svyby(f, b, design, svymean, na.rm = TRUE, vartype = c("se", "ci"))
  tibble::as_tibble(r) %>% rename(estimate = !!var, lo = ci_l, hi = ci_u) %>% select(-se) %>%
    mutate(lo = pmax(lo, 0), hi = pmin(hi, 1))
}

bars_sex <- function(d, x, cap, fills = FILL_SEX) {
  ggplot(d, aes({{ x }}, estimate, fill = sexo)) +
    geom_col(position = position_dodge(.75), width = .7) +
    geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(.75), width = .16, colour = GREY_DARK, linewidth = .4) +
    geom_text(aes(y = hi, label = percent(estimate, accuracy = 1)), position = position_dodge(.75),
              vjust = -0.8, size = 3.1, colour = GREY_DARK) +
    scale_fill_manual(values = fills) + scale_x_discrete(labels = label_wrap(12)) +
    scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .2))) +
    labs(x = NULL, y = NULL, caption = cap) + theme_ukr()
}

hbars_sex <- function(d, x, cap, fills = FILL_SEX) {
  ggplot(d, aes(reorder({{ x }}, estimate), estimate, fill = sexo)) +
    geom_col(position = position_dodge(.75), width = .7) +
    geom_text(aes(label = percent(estimate, accuracy = 1)), position = position_dodge(.75), hjust = -0.2, size = 3, colour = GREY_DARK) +
    coord_flip() + scale_fill_manual(values = fills) +
    scale_y_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .25))) +
    labs(x = NULL, y = NULL, caption = cap) + theme_ukr() +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey92"))
}

heatmap_v <- function(df, vars, w, sub, cap) {
  cats <- lapply(vars, function(v) as_plot_cat(df[[v]])); names(cats) <- vars
  h <- tidyr::expand_grid(a = vars, b = vars) %>%
    mutate(v = purrr::map2_dbl(a, b, ~ if (.x == .y) NA_real_ else cramers_v(cats[[.x]], cats[[.y]], w)),
           a = factor(a, levels = vars), b = factor(b, levels = rev(vars)))
  list(data = h, plot = ggplot(h, aes(a, b, fill = v)) + geom_tile(colour = "white") +
    geom_text(aes(label = ifelse(is.na(v), "", sprintf("%.2f", v))), size = 2.7, colour = GREY_DARK) +
    scale_fill_gradient(low = "#f6f8fa", high = UNICEF_DARK, na.value = "white", limits = c(0, 1), name = "Cramer's V") +
    labs(subtitle = sub, x = NULL, y = NULL, caption = cap) +
    theme_ukr(11) + theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "right",
                          legend.title = element_text(size = 9), panel.grid = element_blank()))
}
