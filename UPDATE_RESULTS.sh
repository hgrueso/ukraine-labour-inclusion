#!/usr/bin/env bash
# UPDATE_RESULTS.sh: regenerate figures, models and scenarios from the cleaned files, then the deck (no re-cleaning)
set -e
cd "$(dirname "$0")"
Rscript code/02_descriptive.R
Rscript code/02b_hsess_descriptive.R
Rscript code/02c_mics_descriptive.R
Rscript code/02d_groups.R
Rscript code/03_regressions.R
Rscript code/04_cost_of_inaction.R
bash RENDER.sh
