# R/utils.R: shared helpers used across all Ukraine analysis scripts.
suppressPackageStartupMessages({ library(here); library(ggplot2) })

ensure_dirs <- function() {
  for (d in c("output", "output/figures", "output/tables", "output/models", "output/inventory"))
    dir.create(here::here(d), showWarnings = FALSE, recursive = TRUE)
  invisible(here::here("output"))
}

save_fig <- function(plot, name, width = 8.5, height = 5.2, dpi = 220) {
  path <- here::here("output", "figures", paste0(name, ".png"))
  ggsave(path, plot, width = width, height = height, dpi = dpi, bg = "white")
  message("  saved figure: ", basename(path))
  invisible(path)
}

write_table <- function(df, name, sub = "tables") {
  path <- here::here("output", sub, paste0(name, ".csv"))
  readr::write_csv(df, path)
  message("  saved table: ", sub, "/", basename(path))
  invisible(path)
}

# Cramer's V between two categorical vectors (weighted counts); 0 = no association, 1 = perfect.
cramers_v <- function(x, y, w = NULL) {
  ok <- !is.na(x) & !is.na(y)
  if (!is.null(w)) ok <- ok & !is.na(w)
  if (sum(ok) < 30) return(NA_real_)
  tb <- if (is.null(w)) table(x[ok], y[ok]) else xtabs(w[ok] ~ x[ok] + y[ok])
  tb <- tb[rowSums(tb) > 0, colSums(tb) > 0, drop = FALSE]
  if (min(dim(tb)) < 2) return(NA_real_)
  n <- sum(tb); e <- outer(rowSums(tb), colSums(tb)) / n
  sqrt(sum((tb - e)^2 / e) / (n * (min(dim(tb)) - 1)))
}

# Any variable -> plottable categorical: many-valued numerics become quintile bins,
# long-tail categoricals become top 7 + "Other".
as_plot_cat <- function(x, max_levels = 8) {
  if (is.numeric(x) && dplyr::n_distinct(x, na.rm = TRUE) > max_levels) {
    br <- unique(stats::quantile(x, probs = seq(0, 1, .2), na.rm = TRUE))
    if (length(br) < 3) return(factor(x))
    return(cut(x, breaks = br, include.lowest = TRUE, dig.lab = 6))
  }
  x <- as.character(x)
  tops <- names(sort(table(x), decreasing = TRUE))
  if (length(tops) > max_levels) x[!is.na(x) & !(x %in% tops[1:(max_levels - 1)])] <- "Other"
  factor(x)
}


# ---- HSESS reader --------------------------------------------------------------
# HSESS sheets: row 1 = question text (blank across multi-response columns), then
# an option row and a variable-code row (IND_A3, D1_Mult3...) inside the sheet,
# then the data. Data rows are recognised by the questionnaire key (e.g. 3800_48_2).
# Variable names = codes; labels = question (+ option).
read_hsess <- function(path) {
  sh <- readxl::excel_sheets(path)
  n  <- vapply(sh, function(s) nrow(suppressMessages(readxl::read_excel(path, sheet = s, range = readxl::cell_cols("A"),
                                                                         col_names = FALSE))), numeric(1))
  s  <- sh[which.max(n)]
  raw <- suppressMessages(readxl::read_excel(path, sheet = s, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  is_data <- grepl("^\\d+_\\d+_\\d+$", trimws(as.character(raw[[1]])))
  first <- which(is_data)[1]
  if (is.na(first)) stop("No data rows found in ", basename(path))
  hdr <- raw[seq_len(first - 1), , drop = FALSE]
  cb  <- if ("variables_ukr" %in% sh) suppressMessages(readxl::read_excel(path, sheet = "variables_ukr")) else NULL
  known <- if (!is.null(cb)) as.character(cb[[1]]) else character(0)
  score <- apply(hdr, 1, function(r) sum(r %in% known | grepl("^IND_|_Mult\\d+$|^[A-Z]{1,2}\\d+(_\\d+)*$", r)))
  code_row <- if (nrow(hdr) > 1) which.max(score) else NA
  q <- as.character(unlist(hdr[1, ]))
  for (j in seq_along(q)[-1]) if (is.na(q[j]) || q[j] == "") q[j] <- q[j - 1]
  opt_rows <- setdiff(seq_len(nrow(hdr)), c(1, code_row))
  opt <- if (length(opt_rows)) apply(hdr[opt_rows, , drop = FALSE], 2, function(x) paste(x[!is.na(x) & x != ""], collapse = " / ")) else rep("", ncol(raw))
  codes <- if (!is.na(code_row)) as.character(unlist(hdr[code_row, ])) else rep(NA_character_, ncol(raw))
  nm <- make.unique(ifelse(is.na(codes) | codes == "", q, codes))
  df <- raw[is_data, , drop = FALSE]; names(df) <- nm
  df[] <- lapply(df, function(x) utils::type.convert(x, as.is = TRUE))
  attr(df, "question") <- stats::setNames(q, nm)
  attr(df, "option")   <- stats::setNames(opt, nm)
  attr(df, "sheet")    <- s
  df
}

# Find a column by its code, falling back to a regex on the question text
hcol <- function(df, spec) {
  code <- spec[1]; rx <- if (length(spec) > 1) spec[2] else NA
  if (!is.na(code) && code %in% names(df)) return(code)
  if (!is.na(rx)) { q <- attr(df, "question"); hit <- names(q)[grepl(paste0("(*UCP)(?i)", rx), enc2utf8(q), perl = TRUE)]
    if (length(hit)) return(hit[1]) }
  NA_character_
}

# Ukrainian -> English: first regex in `map` that matches wins
tr <- function(x, map, other = NA_character_) {
  x <- enc2utf8(as.character(x)); out <- rep(other, length(x)); out[is.na(x)] <- NA
  done <- is.na(x)
  for (rx in names(map)) {
    hit <- !done & grepl(paste0("(*UCP)(?i)", rx), x, perl = TRUE)
    out[hit] <- map[[rx]]; done <- done | hit
  }
  out
}
yes_no <- function(x) { x <- trimws(enc2utf8(as.character(x)))
  ifelse(x == "\u0422\u0430\u043a", 1L, ifelse(x == "\u041d\u0456", 0L, NA_integer_)) }
num <- function(x) suppressWarnings(as.numeric(x))

# Weighted median
wmedian <- function(x, w) { ok <- !is.na(x) & !is.na(w); x <- x[ok]; w <- w[ok]
  if (!length(x)) return(NA_real_); o <- order(x); x <- x[o]; w <- w[o]; x[which(cumsum(w) >= sum(w) / 2)[1]] }
