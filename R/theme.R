# R/theme.R: shared palette and ggplot theme (same as the East Java and Nepal decks).
ACCENT_GIRL <- "#E2007A"
ACCENT_BOY  <- "#1CABE2"
UNICEF_BLUE <- "#1CABE2"
UNICEF_DARK <- "#00377C"
GREY_DARK   <- "#374649"
GREY_MID    <- "#7A8487"
FILL_SEX    <- c("Young women" = ACCENT_GIRL, "Young men" = ACCENT_BOY)

theme_ukr <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      plot.title         = ggplot2::element_blank(),
      plot.subtitle      = ggplot2::element_text(colour = GREY_DARK, margin = ggplot2::margin(b = 8)),
      plot.caption       = ggplot2::element_text(colour = GREY_MID, size = 8),
      strip.text         = ggplot2::element_text(face = "bold", size = 12, colour = GREY_DARK),
      panel.spacing.x    = ggplot2::unit(1.1, "lines"),
      legend.position    = "top",
      legend.title       = ggplot2::element_blank()
    )
}
