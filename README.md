# Ukraine: Young Women's Labour-Market Inclusion

This repository contains the R code and slide deck for an analysis of adolescent girls' and young women's labour-market inclusion in Ukraine, prepared for the UNICEF Ukraine Country Office. The main source is the Multiple Indicator Cluster Survey (MICS 2025/26), complemented by the UNICEF Household Social and Economic Status Survey (HSESS 2023/24) for comparisons between women and men, and by the State Statistics Service of Ukraine Labour Force Survey 2021 as a reference. The analysis covers education, employment, NEET status, marriage and motherhood, care work, informality, occupations, earnings and childcare, with breakdowns by disability, displacement, wealth, location and changes in households since the beginning of the conflict.

The deck presents the findings, the cost of young women remaining NEET under pessimistic, central and optimistic scenarios, an illustrative comparison of intervention types with break-even thresholds, and recommendations grouped by the GenU priorities. Employer returns and programme-specific costing are left for a later stage.

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
│   ├── 04_cost_of_inaction.R        # cost of inaction: pessimistic, central and optimistic scenarios
│   ├── 05_interventions.R           # illustrative comparison of intervention types
│   ├── intervention_parameters.csv  # editable costs and effects per intervention
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

### Cost of inaction and intervention options

Pessimistic, central and optimistic scenarios for the economic and fiscal cost of young women aged 18–24 remaining NEET (lost net earnings, output, income tax and levy, employer contributions, VAT), per 1,000 NEET young women, lifetime per woman, by group, and nationally once a 2026 population figure is set in `04_cost_of_inaction.R`. An illustrative comparison of intervention types (employment services, internships and first jobs, childcare and flexible work, and a combined package) with editable costs and effects in `code/intervention_parameters.csv`.

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

## Assumptions behind the cost of inaction and the investment options

All scenario parameters are computed from the surveys in `code/04_cost_of_inaction.R`:

| Element | Assumption | Source |
|---|---|---|
| Population | NEET women aged 18–24; results per 1,000 NEET young women (a unit of account) and per 100,000 women aged 18–24. A national total needs a 2026 population count (`COI$pop_w1824`) | MICS 2025/26 |
| Potential earnings | Central: wage predicted for each NEET young woman by a wage equation estimated on employed women aged 18–64 (log wage on education level, age, age squared, area and oblast; smearing retransformation). Bounds: 25th percentile and median wage of employed women aged 18–24 | HSESS 2023/24 |
| Counterfactual | Zero earnings while NEET | Assumption |
| Share who could work | Those already looking for work; those needed to close the NEET gap with young men; those looking for work or out of work because of care duties | HSESS 2023/24 |
| Formal jobs | Formal share among employed women aged 18–24: without tertiary education, overall, with tertiary education | HSESS 2023/24 |
| Government receipts | 18% income tax and 5% military levy on the gross wage; 22% employer social contribution; 20% VAT on 80% of net earnings | Statutory rates; consumption share assumed |
| Time | Annual cost; present value over 5 years at 5%; lifetime to age 60 | Discount rate as in A Brighter Future (UNICEF Ukraine, 2024) |
| Not included | Employer returns, women's work-related expenses, effects on children and other household members | |

For each parameter the lowest survey value goes to the pessimistic scenario and the highest to the optimistic one.

Investment options (`code/05_interventions.R`, parameters in `code/intervention_parameters.csv`):

| Service | Cost per participant | Employment effect |
|---|---|---|
| Employment services | UAH 3,000 to 8,000 (assumption; no public unit cost) | +1.6 to +5.4 pp (Card, Kluve and Weber 2018) |
| Employer-supported internships and first jobs | 3 to 6 months of the 2026 minimum wage (UAH 8,647) | +5.4 to +8.7 pp (Card, Kluve and Weber 2018) |
| Childcare support with flexible work | 12 to 24 months of the eYasla payment (UAH 8,000 a month) | +7 to +14 pp (Berlinski and Galiani 2007; Clark et al. 2019; Halim, Perova and Reynolds 2023) |
| Package | Sum of the two above | Sum of the two above (assumption) |

None of the evaluations comes from a country in conflict; the ranges are indicative and should be replaced with Ukrainian evidence when available.

References: Card, D., Kluve, J. and Weber, A. (2018), What works? A meta analysis of recent active labor market program evaluations, Journal of the European Economic Association 16(3). Berlinski, S. and Galiani, S. (2007), The effect of a large expansion of pre-primary school facilities on preschool attendance and maternal employment, Labour Economics 14. Clark, S., Kabiru, C., Laszlo, S. and Muthuri, S. (2019), The impact of childcare on poor urban women's economic empowerment in Africa, Demography 56. Halim, D., Perova, E. and Reynolds, S. (2023), Childcare and mothers' labor market outcomes in lower- and middle-income countries, World Bank Research Observer 38(1). UNICEF Ukraine (2024), A Brighter Future: An Investment Case in Early Childhood Development in Ukraine.
