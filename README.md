# KC Housing Price Appreciation

Beta and sigma convergence analysis of home prices across Kansas City MSA ZIP codes, 2000–2026.

**[Full Report](https://matthewong01.github.io/kc-housing-appreciation/writeup)** · **[Interactive Map](https://matthewong01.github.io/kc-housing-appreciation/map/)** · **[Dashboard](https://matthewong01.github.io/kc-housing-appreciation/dashboard)**

## Overview

This project tests whether lower-priced ZIP codes in the Kansas City metro are catching up in price to higher-priced ones (beta convergence), and whether the overall spread in prices across the metro is narrowing over time (sigma convergence). The analysis is corrected for spatial autocorrelation using a Spatial Error Model, identified through Moran's I and Lagrange Multiplier diagnostics.

**Key findings:**
- Lower-priced ZIP codes converge toward higher-priced ones at roughly 3% per year, a rate stable since 2012
- Price dispersion narrowed overall, but reversed 2007–2014 during the foreclosure crisis
- Appreciation clusters geographically (Moran's I = 0.632), so comps should be pulled by radius rather than restricted to a single ZIP code

## Data

| Dataset | Source | Coverage |
|---|---|---|
| Home Value Index (ZHVI) | Zillow | 2000–2026, ZIP-level |
| American Community Survey | US Census Bureau | 2024 five-year estimate |

Raw files are in `data/`. `target_zips.csv` is the crosswalk used to filter Zillow's national dataset down to Kansas City MSA ZIP codes.

## Method

- OLS beta convergence regressions across four study windows (2001–2026, 2012–2026, 2018–2026, 2020–2026), with coefficients annualized via the Barro & Sala-i-Martin transformation for cross-period comparability
- Sigma convergence tracked via the standard deviation of log home prices by year, with linear trends fit within three identified regimes
- Spatial autocorrelation tested via Moran's I and robust Lagrange Multiplier diagnostics on a Queen contiguity weights matrix; corrected using a Spatial Error Model (`spatialreg::errorsarlm`)

Full derivations and discussion are in the [writeup](https://matthewong01.github.io/kc-housing-appreciation/writeup).

## Repo structure

- `analysis.R` — full analysis pipeline, run top to bottom
- `data/` — source datasets
- `tables/` — HTML table outputs, embedded in the writeup
- `figures/` — exported PNG figures, embedded in the writeup
- `weights_matrix_2012.csv` — Full Queen's Weight Matrix used for spatial autocorrelation testing

## Reproducing

```r
install.packages(c("tidyverse", "readr", "janitor", "tigris", "sf", "broom",
                    "gt", "stargazer", "spdep", "spatialreg", "knitr", "kableExtra"))
source("analysis.R")
```

## Limitations

ZIP codes missing from earlier study periods are disproportionately low-value, low-turnover markets, which may bias early-period convergence estimates downward. This is an unconditional convergence test; no causal inference is drawn. See the [writeup](https://matthewong01.github.io/kc-housing-appreciation/writeup#limitations) for the full discussion.

---

Matthew Ong · [Portfolio](https://matthewong01.github.io) · [LinkedIn](https://linkedin.com/in/matthewong01)

---

**Matthew Ong · Real Estate & Economic Analysis**

[LinkedIn](https://linkedin.com/in/matthewong01) · [GitHub](https://github.com/matthewong01)
