# Immunity landscape

Code and data repository accompanying the manuscript "Making inferences with incomplete epidemiological data: a proof-of-concept estimating measles vaccine coverage across Canada" by Liza Hadley, Rachael M. Milwid, Valerie Hongoh, Rania Wasfi, Stephen M. Kissler, Irena Papst.

# Repository contents

## Main files

Vaccine coverage data is collated in `1_collate-data.qmd`. This document includes notes and citations on all data sources used, and outputs a full, standardized vaccine coverage dataset (`data/coverage/generated/measles_vax-coverage-data.csv`). These data are then cleaned for use in Gaussian Process models in `2_clean-data.qmd`. Gaussian Process models are created in `3_gp-model.qmd`. Finally, manuscript figures and tables are generated in `4_figs_tables.qmd`.

## Overall repository structure

- `*.qmd`: the main analysis documents, numbered in the order they should be run.
- `_targets.R`: a [`{targets}`](https://docs.ropensci.org/targets/) pipeline for the England validation analysis.
- `R/`: helper and model-fitting functions (Gaussian Process kernels, posterior computation, plotting, and pipeline helpers) sourced by the analysis documents and the pipeline.
- `data/`: raw and generated data. See `data/README.md` for details on the datasets.
- `results/`: generated model outputs (`.rds`) and manuscript figures.
- `tests/`: `{testthat}` unit tests.

# One-time setup for using the code

This project uses `{renv}` to manage R package versions. If you are unfamiliar with this software, please first review the package's [get started guide](https://rstudio.github.io/renv/articles/renv.html).

After cloning the repository, restore the project library:

```r
renv::restore()
```

# Reproducing the analysis

Run the Quarto documents in numerical order (`1_collate-data.qmd` through `4_figs_tables.qmd`) to fully reproduce the analysis. Be sure to toggle the "re-run" flags in the `4_figs_tables.qmd` document to `TRUE` for a complete reproduction. However, note that in doing so, the analysis may take a few hours to run completely on a laptop with standard specifications (processing power and RAM) as of mid-2026.

# License

The code for this project is licensed under the terms of the GNU General Public License v3.0. See `LICENSE` for details.
