# Immunity landscape

Code and data repository accompanying the manuscript "How do you make inferences with incomplete epidemiological data? Estimating measles vaccine coverage in Canada" by Liza Hadley, Rachael M. Milwid, Valerie Hongoh, Rania Wasfi, Stephen M. Kissler, Irena Papst.

# How this repository is organized

Vaccine coverage data is collated in `1_collate-data.qmd`. This document includes notes and citations on all data sources use. These data are then cleaned for use in Gaussian Process moels in `2_clean-data.qmd`. Gaussian Process models are created in `3_gp-model.qmd`. Finally, manuscript figures are generated in `results/ms_figs_table.qmd`.

# One-time setup for using the code

## Virtual environment

This project uses `{renv}` to manage R package versions. If you are unfamiliar with this software, please first review the package's [get started guide](https://rstudio.github.io/renv/articles/renv.html).

When opening this project in a R session, be sure that the `{renv}` project has been activated. Be sure to resolve any inconsistencies in the state of the project when prompted. When you open this project for the first time, you will need to `renv::restore()` to install all a packages required by this project.