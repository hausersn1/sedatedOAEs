# Statistical Analysis and Plotting Code
## Hauser et al., 2026

This repository contains the R code used for all statistical analyses and figures in Hauser et al., 2026.

---

## Requirements

### R Packages
The following packages must be installed prior to running the script:

- `tidyverse`
- `ggplot2`
- `lme4`
- `car`
- `emmeans`
- `ggpubr`


---

## Data Files

The following input files are required and should be placed in the same directory as the script:

| File | Description |
|------|-------------|
| `sfoae_data.csv` | SFOAE amplitude and phase measurements for all subjects |
| `dpoae_data.csv` | DPOAE amplitude and noise floor measurements for all subjects |
| `chin_info.csv` | Subject metadata including estimated age in months |
| `singleChin.csv` | Single-animal example data for Figure 1 |
| `Q443_TimePoints.csv` | Time-course DPOAE and SFOAE data for subject Q443 (ESM3) |
| `Q443_Qerb_TimePoints.csv` | Time-course Qerb data for subject Q443 (ESM3) |

Output figures are saved to a `figs/` subdirectory which should be created before running the script. 



## Statistical Models

All models use linear mixed effects regression (`lme4::lmer`) with Subject as a random intercept to account for repeated measures. Fixed effects are tested using Type II F-tests (`car::Anova`). Pairwise contrasts between Awake and Sedated conditions at each frequency are computed using estimated marginal means (`emmeans::emmeans`) with Tukey adjustment for multiple comparisons.

### Primary models (Figures 2–4)
Test the effect of sedation on each OAE measure across frequency:
```
outcome ~ freqFactor * Sedated + (1|Subject)
```
Applied separately for DPOAE amplitude, SFOAE amplitude, and Qerb.

### Sex models (Figure 5)
Test whether the sedation effect differs by sex:
```
outcome ~ freqFactor * Sedated + Sedated:Sex + Sex + (1|Subject)
```
Applied separately for DPOAE amplitude, SFOAE amplitude, and Qerb.

---

## Figures

| Figure | Description | Output file |
|--------|-------------|-------------|
| Figure 1 | Example DPOAE and SFOAE from a single animal, awake vs. sedated | `figs/fig1.tiff` |
| Figure 2 | DPOAE amplitude awake vs. sedated (A) and sedation difference (B) | `figs/fig2.tiff` |
| Figure 3 | SFOAE amplitude awake vs. sedated (A) and sedation difference (B) | `figs/fig3.tiff` |
| Figure 4 | Qerb awake vs. sedated (A) and sedation difference (B) | `figs/fig4.tiff` |
| Figure 5 | Sedation difference by sex for DPOAE (A), SFOAE (B), and Qerb (C) | `figs/fig5.tiff` |


All figures are saved as TIFF at 600 dpi.

---

## Contact

For questions about the code or data, please contact Samantha Hauser, samantha.hauser@pitt.edu.
