#!/usr/bin/env bash
# setup_git.sh: one-shot creation of the public GitHub repository (hgrueso/ukraine-labour-inclusion)
# Requires the gh CLI authenticated as hgrueso (gh auth status). Run from the analysis/ root.
set -e
cd "$(dirname "$0")"
REPO="hgrueso/ukraine-labour-inclusion"
DESC="Young women's labour-market inclusion in Ukraine: SSSU Labour Force Survey 2021 and UNICEF HSESS 2023/24 (R, Quarto)"

gh auth status > /dev/null 2>&1 || { echo "gh CLI not authenticated: run gh auth login"; exit 1; }
echo "GitHub account: $(gh api user --jq .login)"
[ -d .git ] || git init -b main

# Stage code, documentation and aggregate outputs only
git add .gitignore README.md REPLICATE.sh setup_git.sh .here code R
for p in output/figures output/tables output/models output/slides/06_slides_en.pdf output/slides/06_slides_en.html; do
  [ -e "$p" ] && git add "$p"
done

# Safety check: never publish microdata or the variable report
if git diff --cached --name-only | grep -Ei '^data/|\.(rds|rdata|sav|dta|xlsx|xls)$|^output/inventory/'; then
  echo "Stopped: microdata or the variable report is staged. Nothing was pushed."; exit 1
fi

git commit -q -m "Ukraine labour-market inclusion: analysis code, figures and preliminary findings deck" || echo "Nothing new to commit"
if git remote get-url origin > /dev/null 2>&1; then
  git push -u origin main
elif gh repo view "$REPO" > /dev/null 2>&1; then
  git remote add origin "https://github.com/$REPO.git" && git push -u origin main
else
  gh repo create "$REPO" --public --description "$DESC" --source=. --remote=origin --push
fi
echo "Repository: https://github.com/$REPO"
