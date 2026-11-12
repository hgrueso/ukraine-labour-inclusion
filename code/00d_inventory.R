# 00d_inventory.R: full report of every variable we hold -------------------
# LFS 2021 and both HSESS files (individual and household). For every variable:
# code, question, answer option, type, missingness, distinct values, range, most
# frequent values (with value labels), and whether it is a key variable for the analysis.
# Output: output/inventory/variable_report.xlsx (one sheet per file, "Key variables",
#         "Value frequencies"), output/inventory/key_variables.csv
source(here::here("code", "00_config.R"))
suppressPackageStartupMessages({ library(haven); library(readxl); library(writexl); library(purrr) })

profile_df <- function(df, file_tag, question = NULL, option = NULL) {
  map_dfr(seq_along(df), function(j) {
    v <- names(df)[j]; x <- df[[j]]
    lab <- if (!is.null(question)) question[[v]] else { l <- attr(x, "label"); if (is.null(l)) NA_character_ else l }
    vl  <- attr(x, "labels")
    xs  <- if (inherits(x, "haven_labelled")) as.character(haven::as_factor(x, levels = "both")) else as.character(x)
    xn  <- num(if (inherits(x, "haven_labelled")) haven::zap_labels(x) else x)
    isnum <- any(!is.na(xn)) && mean(!is.na(xn[!is.na(xs)])) > .95
    tb <- sort(table(xs, useNA = "no"), decreasing = TRUE); k <- seq_len(min(6, length(tb)))
    tibble(file = file_tag, variable = v, question = as.character(lab),
           option = if (!is.null(option)) option[[v]] else "",
           type = if (isnum) "numeric" else "text",
           n_nonmissing = sum(!is.na(xs) & xs != ""), pct_missing = round(100 * mean(is.na(xs) | xs == ""), 1),
           n_distinct = length(tb),
           min = if (isnum) suppressWarnings(min(xn, na.rm = TRUE)) else NA_real_,
           median = if (isnum) suppressWarnings(stats::median(xn, na.rm = TRUE)) else NA_real_,
           max = if (isnum) suppressWarnings(max(xn, na.rm = TRUE)) else NA_real_,
           value_labels = if (!is.null(vl)) paste(paste0(vl, "=", names(vl)), collapse = "; ") else "",
           top_values = if (length(tb)) paste0(names(tb)[k], " (", round(100 * tb[k] / sum(!is.na(xs))), "%)", collapse = "; ") else "")
  })
}
value_freqs <- function(df, file_tag, max_levels = 30) {
  map_dfr(names(df), function(v) {
    x <- df[[v]]
    xs <- if (inherits(x, "haven_labelled")) as.character(haven::as_factor(x, levels = "both")) else as.character(x)
    if (dplyr::n_distinct(xs, na.rm = TRUE) > max_levels) return(NULL)
    tb <- table(xs, useNA = "ifany")
    tibble(file = file_tag, variable = v, value = ifelse(is.na(names(tb)), "(missing)", names(tb)),
           n = as.integer(tb), pct = round(100 * as.numeric(tb) / sum(tb), 1))
  })
}

reports <- list(); freqs <- list(); found <- list()
cat(strrep("=", 78), "\n VARIABLE REPORT, Ukraine analysis\n", strrep("=", 78), "\n", sep = "")

# ---- LFS 2021 -------------------------------------------------------------------
if (!is.na(PATHS$lfs)) {
  lfs <- haven::read_sav(PATHS$lfs); names(lfs) <- gsub("\\s+", "", names(lfs))
  cat(sprintf("\n-- LFS 2021: %s rows (interviews), %s persons, %d variables; sum of weights %s\n",
              format(nrow(lfs), big.mark = ","), format(dplyr::n_distinct(lfs$kod_res), big.mark = ","), ncol(lfs),
              format(round(sum(lfs$weight_year, na.rm = TRUE)), big.mark = ",")))
  r <- profile_df(lfs, "LFS 2021")
  r$value_labels <- ifelse(r$value_labels == "" & r$variable %in% names(LFS_LABELS),
    vapply(r$variable, function(v) if (v %in% names(LFS_LABELS)) paste(paste0(names(LFS_LABELS[[v]]), "=", LFS_LABELS[[v]]), collapse = "; ") else "", ""),
    r$value_labels)
  r$key_variable <- r$variable %in% LFS_KEY
  reports$lfs <- r; freqs$lfs <- value_freqs(lfs, "LFS 2021"); rm(lfs)
}

# ---- HSESS individual and household ------------------------------------------------
for (key in c("hsess_ind", "hsess_hh")) {
  p <- PATHS[[key]]; tag <- if (key == "hsess_ind") "HSESS individual" else "HSESS household"
  if (is.na(p)) { cat("\n-- ", tag, ": not found\n", sep = ""); next }
  d <- read_hsess(p)
  spec <- if (key == "hsess_ind") HSESS_IND else HSESS_HH
  res <- vapply(spec, function(s) hcol(d, s), "")
  found[[key]] <- tibble(file = tag, role = names(res), variable = unname(res))
  cat(sprintf("\n-- %s: %s rows, %d variables (sheet '%s'); key variables found: %d of %d\n",
              tag, format(nrow(d), big.mark = ","), ncol(d), attr(d, "sheet"), sum(!is.na(res)), length(res)))
  if (any(is.na(res))) cat("   NOT FOUND: ", paste(names(res)[is.na(res)], collapse = ", "), "\n")
  r <- profile_df(d, tag, attr(d, "question"), attr(d, "option"))
  r$key_variable <- r$variable %in% res
  reports[[key]] <- r; freqs[[key]] <- value_freqs(d, tag)
}

# ---- MICS 2025/26 (all SPSS files in data/mics7) ---------------------------------------
MICS_FILES <- c(hh = "Households", hl = "Household members", wm = "Women 15-49", bh = "Birth histories",
                ch = "Children under 5", fs = "Children 5-17")
mics_lines <- c("MICS 2025/26 summary", strrep("=", 60))
if (dir.exists(PATHS$mics7_dir)) {
  for (f in sort(list.files(PATHS$mics7_dir, pattern = "\\.sav$", full.names = TRUE))) {
    nm  <- tolower(tools::file_path_sans_ext(basename(f)))
    tag <- paste0("MICS7 ", nm)
    m   <- haven::read_sav(f, user_na = FALSE)
    r   <- profile_df(m, tag); r$key_variable <- FALSE
    reports[[tag]] <- r; freqs[[tag]] <- value_freqs(m, tag)
    # modules = letters at the start of MICS variable names (HL, ED, WB, MA, ...)
    mods <- sort(table(sub("^([A-Za-z]+).*", "\\1", names(m))), decreasing = TRUE)
    hdr <- sprintf("\n-- MICS7 %s (%s): %s rows, %d variables", nm, MICS_FILES[nm], format(nrow(m), big.mark = ","), ncol(m))
    cat(hdr, "\n   modules: ", paste(paste0(names(mods), " (", mods, ")"), collapse = ", "), "\n", sep = "")
    mics_lines <- c(mics_lines, hdr, paste0("   modules: ", paste(paste0(names(mods), " (", mods, ")"), collapse = ", ")))
    # variables relevant to the analysis, matched on their labels
    rx <- "work|employ|job|paid|earn|income|business|occupat|disab|difficult|function|attend|school|educat|grade|level|marri|union|partner|child|birth|pregnan|displac|idp|move|resid|region|oblast|area|urban|rural|wealth|weight|stratum|cluster|age|sex"
    hit <- r %>% filter(grepl(rx, question, ignore.case = TRUE)) %>%
      transmute(line = sprintf("   %-12s %s  [%s values, %s%% missing]", variable, substr(question, 1, 80), n_distinct, pct_missing))
    mics_lines <- c(mics_lines, "   relevant variables:", hit$line)
    rm(m)
  }
} else cat("\n-- MICS 2025/26: folder data/mics7 not found\n")
writeLines(mics_lines, here::here("output", "inventory", "mics7_summary.txt"))

# ---- workbook ------------------------------------------------------------------------
fnd <- bind_rows(found)
kv <- KEY_VARS
write_csv(kv, here::here("output", "inventory", "key_variables.csv"))
write_csv(fnd, here::here("output", "inventory", "hsess_variables_found.csv"))
book <- list(`Read me` = tibble(`How to use this report` = c(
  "One sheet per file. Every variable: code, question, answer option, type, missingness, distinct values, range, top values.",
  "key_variable = TRUE marks the variables used in the analysis (sheet 'Key variables' explains each one).",
  "HSESS variable names are the questionnaire codes (IND_A3, D1_Mult3...); 'question' and 'option' carry the Ukrainian wording.",
  "Sheet 'Value frequencies': full distribution of every variable with 30 or fewer distinct values.")),
  `Key variables` = kv, `HSESS key columns found` = fnd)
for (k in names(reports)) book[[substr(reports[[k]]$file[1], 1, 31)]] <- reports[[k]] %>% relocate(key_variable, .before = variable)
book$`Value frequencies` <- bind_rows(freqs)
writexl::write_xlsx(book, here::here("output", "inventory", "variable_report.xlsx"))
write_csv(bind_rows(reports), here::here("output", "inventory", "variable_report_all.csv"))
cat("\nWorkbook: output/inventory/variable_report.xlsx\n")
