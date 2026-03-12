
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SWT_TTE_Appx

<!-- badges: start -->

<!-- badges: end -->

This repository provides code for the sample size calculations described
in Appendix 1 of “Emulating Longitudinal Cluster Randomized and Stepped
Wedge Trials to Evaluate Group-Level Policies and Interventions”.

Created By: Lee Kennedy-Shaffer, PhD, Assistant Professor, Department of
Biostatistics, Yale School of Public Health, New Haven, CT, USA

Contact: lee.kennedy-shaffer (at) yale.edu

## Key Files

- `Vax_data.Rda`: This data was compiled by the author from information
  provided by Fuller et al. 2022
  (<https://doi.org/10.1371/journal.pone.0274374>), downloaded from
  Harvard Dataverse (<https://doi.org/10.7910/DVN/K1XX02>). Periods
  refer to CDC MMWR weeks, as determined from
  <https://ndc.services.cdc.gov/wp-content/uploads/W2021-22.pdf>.

- `Sample_Size_Calculations.R`: This file gets approximate parameters
  from the data, creates study schematic data sets for the three
  proposed study designs, and estimates power for each using the
  `swdpwr` package (see
  <https://jiachenchen322.github.io/swdpwr_r_manual/>).
