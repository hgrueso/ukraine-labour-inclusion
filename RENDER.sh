#!/usr/bin/env bash
# RENDER.sh: render the deck and export it to PDF (figures and tables must already exist in output/)
set -e
cd "$(dirname "$0")"
quarto render code/06_slides_en.qmd
mkdir -p output/slides && mv -f code/06_slides_en.html output/slides/
HTML="$PWD/output/slides/06_slides_en.html"; PDF="$PWD/output/slides/06_slides_en.pdf"; rm -f "$PDF"
CHROME="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
if [ -x "$CHROME" ]; then
  "$CHROME" --headless=new --disable-gpu --no-pdf-header-footer --run-all-compositor-stages-before-draw \
    --virtual-time-budget=300000 --print-to-pdf="$PDF" "file://$HTML?print-pdf" 2>/dev/null || true
fi
PAGES=$(python3 -c "import re,sys,os; p=sys.argv[1]; print(len(re.findall(rb'/Type\s*/Page(?!s)', open(p,'rb').read())) if os.path.exists(p) else 0)" "$PDF")
echo "Chrome export: ${PAGES} pages"
if [ "${PAGES:-0}" -lt 5 ]; then
  echo "Using the Playwright fallback for the PDF (first run installs a local Chromium, a few minutes)"
  if [ ! -x .pdfenv/bin/python ]; then
    python3 -m venv .pdfenv && .pdfenv/bin/pip -q install playwright && .pdfenv/bin/playwright install chromium
  fi
  .pdfenv/bin/python code/export_pdf.py "$HTML" "$PDF"
  PAGES=$(python3 -c "import re,sys; print(len(re.findall(rb'/Type\s*/Page(?!s)', open(sys.argv[1],'rb').read())))" "$PDF")
  echo "Playwright export: ${PAGES} pages"
fi
echo "Done: $PDF"
