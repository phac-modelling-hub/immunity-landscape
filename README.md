# Immunity landscape

Code and data repository accompanying the manuscript "Making inferences with incomplete epidemiological data: a proof-of-concept estimating measles vaccine coverage across Canada" by Liza Hadley, Rachael M. Milwid, Valerie Hongoh, Rania Wasfi, Stephen M. Kissler, Irena Papst.

# Main files

Vaccine coverage data is collated in `1_collate-data.qmd`. This document includes notes and citations on all data sources used, and outputs a full, standardized dataset (`data/coverage/generated/measles_vax-coverage-data.csv`). These data are then cleaned for use in Gaussian Process models in `2_clean-data.qmd`. Gaussian Process models are created in `3_gp-model.qmd`. Finally, manuscript figures are generated in `results/ms_figs_table.qmd`.

# One-time setup for using the code

This project uses `{renv}` to manage R package versions. If you are unfamiliar with this software, please first review the package's [get started guide](https://rstudio.github.io/renv/articles/renv.html).
