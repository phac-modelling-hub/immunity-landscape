#' Lookup table for PT names 
pt_lookup <- function() {
  read_csv(here::here("data", "pt-relation", "pt_lookup.csv"), show_col_types = "FALSE") |> 
    transmute(pt = al_code, location = nm_en) |> 
    bind_rows(tibble::tibble(pt = "CA", location = "Canada"))
}