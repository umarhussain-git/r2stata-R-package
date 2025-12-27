# r2stata

[![CRAN version](https://www.r-pkg.org/badges/version/r2stata)](https://cran.r-project.org/package=r2stata)  

**r2stata** is an R package that provides interactive Stata-like console functions for statistical analyses. The package allows R users to perform analyses such as logistic regression, t-tests, ANOVA, chi-squared tests, and post-hoc comparisons while outputting results in a format closely resembling Stata.

---

## Features

- **Stata-style Logistic Regression Console** (`stata_logit`)
  - Fit binary logistic regression models with Stata-like output.
  - Option to display odds ratios instead of coefficients.
  - Includes likelihood-ratio tests, pseudo R², z-statistics, p-values, and confidence intervals.

- **Stata-style T-tests**
  - Independent sample t-test (`stata_ttest_indep`)
  - Paired t-test (`stata_ttest_paired`)
  - One-sample t-test (`stata_ttest_onesample`)
  - Mann–Whitney U test (rank-sum)

- **Stata-style ANOVA** (`stata_anova`)
  - Standard ANOVA with Tukey post-hoc tests.
  - Includes Kruskal-Wallis non-parametric test.

- **Chi-squared and Fisher's exact tests** (`stata_chi2`, `stata_fisher`)
  - Tables with observed, expected counts, chi-squared statistics.
  - Automatic use of Fisher’s exact test if assumptions are violated.

- **Post-hoc and summary functions**
  - Tukey post-hoc comparisons.
  - Detailed descriptive statistics in Stata-style tables.

---

## Installation

You can install the latest development version from GitHub using `devtools`:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install r2stata from GitHub
devtools::install_github("umarhussain-git/r2stata-R-package")
# or using remotes
remotes::install_github("umarhussain-git/r2stata-R-package")
```
