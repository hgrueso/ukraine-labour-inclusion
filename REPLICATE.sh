#!/usr/bin/env bash
# REPLICATE.sh: full pipeline driver. Run from the Ukraine "analysis" folder (the one holding data/).
set -e
cd "$(dirname "$0")"
Rscript code/00d_inventory.R          # variable report -> output/inventory/variable_report.xlsx
Rscript code/01_clean_lfs.R           # LFS 2021        -> output/lfs_analysis_ready.rds
Rscript code/01b_clean_hsess.R        # HSESS 2023/24   -> output/hsess_analysis_ready.rds
Rscript code/01c_clean_mics.R         # MICS 2025/26    -> output/mics_*.rds
Rscript code/02_descriptive.R         # LFS figures     -> output/figures/f*_en.png
Rscript code/02b_hsess_descriptive.R  # HSESS figures   -> output/figures/h*_en.png
Rscript code/02c_mics_descriptive.R   # MICS figures    -> output/figures/m*_en.png
Rscript code/02d_groups.R             # groups, poverty, VET -> output/figures/g*_en.png
Rscript code/03_regressions.R         # LFS, HSESS and MICS models -> output/models/
Rscript code/04_cost_of_inaction.R    # cost of inaction scenarios -> output/figures/c1, c3, c4
Rscript code/05_interventions.R       # intervention comparison (parameters in code/intervention_parameters.csv) -> c5, c6
bash RENDER.sh                         # deck (HTML) and PDF export, with a Playwright fallback
