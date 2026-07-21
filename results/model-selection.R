#' Run GP model selection over `ms_grid` for one province-relation framework.
#'
#' By default the result is returned without writing to disk. Pass a `csv_name`
#' to also write the combinations to results/<csv_name>; results_setup.R is the
#' sole producer of the GPcombinations_*_witherror.csv artifacts consumed by the
#' figure/table scripts, so code-demo.qmd calls this without `csv_name` (purely
#' exploratory).
#'
#' @param vax_dataset standardised vaccine coverage dataset (tibble)
#' @param prov_values province-relation indicator, e.g. "Gini" (character)
#' @param csv_name optional file name (within results/) to write the
#'   combinations to; if NULL (default) nothing is written (character or NULL)
#' @param grid hyperparameter grid; defaults to the shared `ms_grid` (list)
#' @return the model-selection combinations tibble (written to CSV only if
#'   `csv_name` is supplied)

#' Shared hyperparameter grid searched during model selection.
ms_grid <- list(
  l1         = c(1, 1.25, 1.5, 1.75, 2, 2.25, 2.5),
  l2         = c(0.1, 0.2, 0.5, 1, 1.5, 2, 2.5, 3.0, 3.5, 4.0, 6.0, 8.0, 10, 25, 50, 75, 100, 150, 200, 250, 500),
  b          = c(0.5, 1, 1.5),
  meas_error = c(0.5, 0.1, 0.05, 0.01, 0)
)

run_model_selection <- function(vax_dataset, prov_values, csv_name = NULL, grid = ms_grid) {
  combos <- select_GP(
    vax_dataset,
    prov_values = prov_values,
    k_list      = list(ksqexp = ksqexp, kexp = kexp),
    l1          = grid$l1,
    l2          = grid$l2,
    b           = grid$b,
    meas_error  = grid$meas_error
  )
  if (!is.null(csv_name)) {
    readr::write_csv(combos, here::here("results", csv_name))
  }
  combos
}