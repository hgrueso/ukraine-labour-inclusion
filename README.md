# Ukraine: Young Women's Labour-Market Inclusion

This repository contains the R code and slide deck for an analysis of adolescent girls' and young women's labour-market inclusion in Ukraine, prepared for the UNICEF Ukraine Country Office. The main source is the Multiple Indicator Cluster Survey (MICS 2025/26), complemented by the UNICEF Household Social and Economic Status Survey (HSESS 2023/24) for comparisons between women and men, and by the State Statistics Service of Ukraine Labour Force Survey 2021 as a reference. The analysis covers education, employment, NEET status, marriage and motherhood, care work, informality, occupations, earnings and childcare, with breakdowns by disability, displacement, wealth, location and changes in households since the beginning of the conflict.

The deck presents the findings, the annual cost of the gender gap in NEET to the economy and to public revenues (with a budget perspective and break-even thresholds), and recommendations grouped by the GenU priorities. Employer returns and programme-specific costing are left for a later stage.

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
│   ├── 01c_clean_mics.R             # MICS 2025/26 cleaning (members, women, children 5-17, households)
│   ├── 02_descriptive.R             # LFS figures and tables
│   ├── 02b_hsess_descriptive.R      # HSESS figures and tables
│   ├── 02c_mics_descriptive.R       # MICS figures and tables
│   ├── 02d_groups.R                 # barriers by group, poverty, child poverty, VET
│   ├── 03_regressions.R             # linear probability models, adjusted comparisons, wage equation
│   ├── 04_cost_of_inaction.R        # cost of the gender gap, budget perspective, sensitivity, break-even
│   ├── archive/                     # earlier illustrative intervention comparison (not run)
│   └── 06_slides_en.qmd             # revealjs deck (exported to PDF)
│
├── data/                            # input data (NOT committed)
│   ├── mics7/                       # MICS 2025/26 SPSS files (hh, hl, wm, bh, ch, fs)
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

**Multiple Indicator Cluster Survey (MICS) 2025/26, State Statistics Service of Ukraine and UNICEF.** 23,708 households, 37,315 household members and 8,213 women aged 15–49 selected for interview, plus children aged 5–17. There is no men's questionnaire: NEET and employment come from the NEET module for women aged 15–24, and school attendance from the household roster for all members aged 3–24. Estimates use the MICS weights with primary sampling units and strata.

**SSSU Labour Force Survey 2021, anonymised public-use file.** 265,567 interviews from 151,682 persons aged 15+ in 88.1 thousand households (rotation panel), collected in 2021 and used as a reference. Age is released in bands (15–24, 25–29 and older), so LFS results are reported for 15–24, and for 15–29 for NEET. Estimates use the annual weight (`weight_year`), with standard errors clustered by person.

**UNICEF Household Social and Economic Status Survey (HSESS), December 2023 to February 2024.** 18,838 individuals in 8,024 households, all regions covered by the survey (Luhansk oblast is not included). Each sheet carries the question text as header, followed by an answer-option row and a variable-code row; `read_hsess()` uses the codes (`IND_A3`, `D1_Mult3`, ...) as variable names. Estimates use the final weight, with standard errors clustered by household.

### Age groups

| Group | Indicators |
|---|---|
| 15–17 | Education, training, NEET (ages 14–17 are legal minors; HSESS asks employment questions from age 15) |
| 18–24 | Labour-force participation, employment, job quality, earnings, NEET |
| 15–24 | Education-to-work transition, NEET |
| 15–29 | NEET (ILO age range, for reference) |

### Definitions

Employment follows the ILO definition (production for own household use only is not counted). NEET: not employed and not in education. HSESS labour-force participation: employed, or looking for work or starting a business. Informal employment: verbal agreement, own-account work without registration, or family farm work for sale. Motherhood: living with own child under 18, identified from the household roster (relationships are recorded relative to the household respondent).

### Descriptive layer

Weighted shares with 95% confidence intervals by sex, age group, urban/rural area, oblast, education, marital status, motherhood, displacement, disability in the household and household income; reasons for inactivity; informality and hours; occupational distribution and dissimilarity index; median wages; reasons why young children do not attend preschool.

### Groups, poverty and vocational education

NEET among women aged 18–24 by motherhood, single-headed household, poverty (wealth quintiles in MICS; income per member below 60% of the median in HSESS), rural residence, displacement and disability; the number of disadvantages; poverty combined with motherhood; child poverty in households with a NEET young woman; a profile of vocational graduates and the vocational gap in NEET with and without controls.

### Cost of the gender gap

The annual cost of the gender gap in NEET at ages 18–24: how much more the economy (value of the work) and public revenues (income tax, military levy, social contributions, VAT) would receive if young women were NEET at the same rate as young men, using Mincer-predicted wages; expressed as a share of 2024 GDP, public revenues and preschool spending, with a sensitivity analysis and break-even thresholds.

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

MICS 2025/26 data are available after registration at mics.unicef.org; their terms of use do not allow redistribution. The LFS 2021 public-use file is distributed by the State Statistics Service of Ukraine. The HSESS 2023/24 microdata belong to UNICEF Ukraine (licence CC BY-NC-SA 4.0). None of the data are included in this repository (see `.gitignore`). To reproduce the analysis, place the files in `data/` as shown above.

---

## Contact

**Hernando Grueso, UNICEF**
`hgrueso@unicef.org`

## Cost of the gender gap: assumptions

The cost of inaction is the earning potential lost each year that young women aged 18–24 remain NEET at a higher rate than young men: how much more the economy and public revenues would receive if the gap were closed (`code/04_cost_of_inaction.R`).

| Element | Assumption | Source |
|---|---|---|
| Gap | NEET rate of women aged 18–24 minus that of men aged 18–24 | HSESS 2023/24 (the only source with both sexes) |
| Population | Weighted count of women aged 18–24 in the areas covered by HSESS (weights are population-scaled); override with an official estimate in `COI$pop_w1824` | HSESS 2023/24 |
| Earnings | Wage predicted for each NEET young woman by a Mincer equation estimated on employed women aged 18–64 (log wage on education level, age, age squared, area and oblast; smearing retransformation); no wage growth | HSESS 2023/24, nominal UAH, Dec 2023 to Feb 2024 |
| Counterfactual | Zero earnings while NEET | Assumption |
| Formal jobs | Formal share among employed women aged 18–24 | HSESS 2023/24 |
| Economy | Gross wage plus employer social contribution for formal jobs, net wage for informal jobs (a lower bound on the value of the work) | Statutory rates |
| Public revenues | 18% income tax and 5% military levy on the gross wage; 22% employer social contribution (ESV); 20% VAT on 80% of net earnings | Statutory rates; consumption share assumed |
| Labour demand | Jobs available at the wage of employed women with the same profile; no displacement of other workers | Assumption; employer surveys in 2026 report widespread shortages |
| Budget comparators (2024, UAH bn) | GDP 7,658.7; state budget taxes and fees over 3,100 plus ESV 546.1; health spending 238.7; Programme of Medical Guarantees 159; education spending 348.4; preschool education 52.6 (budgeted); state support to low-income families 17.9 | State Statistics Service of Ukraine (GDP estimate, 31 March 2025); Ministry of Finance (2024 budget execution releases, 31 January 2025; State Budget 2025 overview); National Health Service of Ukraine (PMG 2024); UNICEF Ukraine (2024), A Brighter Future |
| Not included | Effects on children, employers and the wider economy; lost experience; women's work-related expenses | |

A sensitivity table (`output/tables/coi_sensitivity.csv`) changes one assumption at a time: the wage (25th percentile and median of employed women aged 18–24), formality (with and without tertiary education), and the number of women (only those already looking for work; the gap measured with the MICS NEET rate). A break-even table (`output/tables/coi_break_even.csv`) shows the share of participants who would need to move into work for a programme of a given cost to recover it, using only these estimates.

An earlier illustrative comparison of intervention types, based on evaluations in other countries, is kept in `code/archive/` and is not part of the pipeline or the deck.

