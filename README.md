# Ukraine: Young Women's Labour-Market Inclusion

This repository contains the R code and slide deck for an analysis of adolescent girls' and young women's labour-market inclusion in Ukraine, prepared for the UNICEF Ukraine Country Office. The analysis combines a pre-war baseline from the State Statistics Service of Ukraine Labour Force Survey (LFS 2021) with the UNICEF Household Social and Economic Status Survey (HSESS 2023/24), covering education, labour-force participation, employment, NEET status, informality, occupations, earnings, childcare and motherhood, with breakdowns by displacement, disability, income and location.

The current output is a preliminary findings deck. The cost-of-inaction estimates and the private-sector engagement case will be added in later stages.

---

## Repository structure

```
analysis/
├── .here                            # project root sentinel
├── README.md                        # this file
├── REPLICATE.sh                     # full pipeline driver (includes the PDF export)
├── setup_git.sh                     # one-shot GitHub repository creation
│
├── R/                               # shared functions
│   ├── theme.R                      # colours and ggplot theme (same as Nepal and East Java)
│   ├── utils.R                      # HSESS reader, helpers, Cramer's V
│   ├── plots.R                      # survey shares and bar-chart style
│   └── translations.R               # Ukrainian answer codes to English
│
├── code/                            # numbered analytical stages
│   ├── 00_config.R                  # paths, codebooks, age groups, variable map
│   ├── 00d_inventory.R              # full report of every variable in every file
│   ├── 01_clean_lfs.R               # LFS 2021 cleaning
│   ├── 01b_clean_hsess.R            # HSESS 2023/24 cleaning (English labels, household roster)
│   ├── 02_descriptive.R             # LFS figures and tables
│   ├── 02b_hsess_descriptive.R      # HSESS figures and tables
│   ├── 03_regressions.R             # linear probability models (LFS and HSESS)
│   └── 06_slides_en.qmd             # revealjs deck (exported to PDF)
│
├── data/                            # input data (NOT committed)
│   ├── mic_poc_rob_syly_21/         # SSSU LFS 2021 anonymised public-use file (.sav)
│   └── *.xlsx                       # UNICEF HSESS 2023/24 household and individual files
│
└── output/
    ├── figures/                     # PNG figures used in the deck
    ├── tables/                      # CSV tables behind each figure
    ├── models/                      # regression estimates
    ├── slides/                      # 06_slides_en.pdf and .html
    └── inventory/                   # variable report (NOT committed: lists free-text answers)
```

---

## Methodology summary

### Data

**SSSU Labour Force Survey 2021, anonymised public-use file.** 265,567 interviews from 151,682 persons aged 15+ in 88.1 thousand households (rotation panel), collected before February 2022. Age is released in bands (15–24, 25–29 and older), so LFS results are reported for 15–24, and for 15–29 for NEET. Estimates use the annual weight (`weight_year`), with standard errors clustered by person.

**UNICEF Household Social and Economic Status Survey (HSESS), December 2023 to February 2024.** 18,838 individuals in 8,024 households, all regions except the occupied territories and Luhansk oblast. Each sheet carries the question text as header, followed by an answer-option row and a variable-code row; `read_hsess()` uses the codes (`IND_A3`, `D1_Mult3`, ...) as variable names. Estimates use the final weight, with standard errors clustered by household.

### Age groups

| Group | Indicators |
|---|---|
| 15–18 | Education, training, NEET (ages 14–17 are legal minors; HSESS asks employment questions from age 15) |
| 19–24 | Labour-force participation, employment, job quality, earnings, NEET |
| 15–24 | Education-to-work transition, NEET |
| 15–29 | NEET (ILO age range, for reference) |

### Definitions

Employment follows the ILO definition (production for own household use only is not counted). NEET: not employed and not in education. HSESS labour-force participation: employed, or looking for work or starting a business. Informal employment: verbal agreement, own-account work without registration, or family farm work for sale. Motherhood: living with own child under 18, identified from the household roster (relationships are recorded relative to the household respondent).

### Descriptive layer

Weighted shares with 95% confidence intervals by sex, age group, urban/rural area, oblast, education, marital status, motherhood, displacement, disability in the household and household income; reasons for inactivity; informality and hours; occupational distribution and dissimilarity index; median wages; reasons why young children do not attend preschool.

### Multivariate analysis

Linear probability models with oblast fixed effects and survey weights (`fixest::feols`), reported in percentage points. Interaction terms estimate how the difference between women and men varies with marital status, motherhood, area, income and education.

---

## Reproducing the analysis

### Prerequisites

```r
install.packages(c(
  "here", "dplyr", "tidyr", "readr", "stringr", "purrr", "tibble", "rlang", "forcats",
  "haven", "readxl", "writexl", "survey", "fixest", "ggplot2", "scales", "knitr", "rmarkdown"
))
```

[Quarto](https://quarto.org) is required to render the deck; Google Chrome is used to export it to PDF.

### Run the full pipeline

```bash
# From the analysis/ root:
bash REPLICATE.sh
```

Output: `output/slides/06_slides_en.pdf`, with all figures in `output/figures/` and tables in `output/tables/`.

---

## Data access

The LFS 2021 public-use file is distributed by the State Statistics Service of Ukraine. The HSESS 2023/24 microdata belong to UNICEF Ukraine (licence CC BY-NC-SA 4.0). Neither is redistributed in this repository (see `.gitignore`). To reproduce the analysis, place the files in `data/` as shown above.

---

## Contact

**Hernando Grueso, UNICEF**
`hgrueso@unicef.org`
