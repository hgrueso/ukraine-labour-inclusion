#!/usr/bin/env bash
# REPLICATE.sh: full pipeline driver. Run from the Ukraine "analysis" folder (the one holding data/).
set -e
cd "$(dirname "$0")"
Rscript code/00d_inventory.R          # variable report -> output/inventory/variable_report.xlsx
Rscript code/01_clean_lfs.R           # LFS 2021        -> output/lfs_analysis_ready.rds
Rscript code/01b_clean_hsess.R        # HSESS 2023/24   -> output/hsess_analysis_ready.rds
Rscript code/02_descriptive.R         # LFS figures     -> output/figures/f*_en.png
Rscript code/02b_hsess_descriptive.R  # HSESS figures   -> output/figures/h*_en.png
Rscript code/03_regressions.R         # LFS and HSESS tests -> output/models/
quarto render code/06_slides_en.qmd   # deck (HTML)
mkdir -p output/slides && mv -f code/06_slides_en.html output/slides/
# PDF export (Chrome headless, as in the East Java deck)
CHROME="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
if [ -x "$CHROME" ]; then
  "$CHROME" --headless=new --disable-gpu --no-pdf-header-footer \
    --run-all-compositor-stages-before-draw --virtual-time-budget=90000 \
    --print-to-pdf="$PWD/output/slides/06_slides_en.pdf" \
    "file://$PWD/output/slides/06_slides_en.html?print-pdf" 2>/dev/null
  echo "Done: output/slides/06_slides_en.pdf"
else
  echo "Google Chrome not found: HTML deck at output/slides/06_slides_en.html"
fi
