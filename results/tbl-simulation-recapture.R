# tbl-simulation-recapture.R
# Table simulation-recapture: SBC summary statistics.

SBC_results <- readRDS(here::here("results", "SBC_results_Gini.rds"))

# ── Summary table: recovery rates per parameter and dataset type ───────────────
tbl_sbc <- SBC_results %>%
  group_by(sample) %>%
  summarise(
    n               = n(),
    exact_recovery  = mean(exact_recovery),
    k_correct       = mean(k_correct),
    l1_correct      = mean(l1_correct),
    l2_correct      = mean(l2_correct),
    b_correct       = mean(b_correct),
    mean_norm_score = mean(normalized_score),
    .groups = "drop"
  ) %>%
  mutate(sample = recode(sample,
                         "full"    = "Full synthetic data",
                         "partial" = "Partial synthetic data")) %>%
  rename(`Dataset type`          = sample,
         `N iterations`          = n,
         `Exact recovery`        = exact_recovery,
         `Kernel correct`        = k_correct,
         `l1 correct`            = l1_correct,
         `l2 correct`            = l2_correct,
         `b correct`             = b_correct,
         `Mean normalised score` = mean_norm_score)

tbl_sbc %>%
  flextable() %>%
  colformat_double(digits = 2) %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  save_as_docx(path = here::here("results", "tbl-simulation-recapture.docx"))

# ── Per-iteration detail (partial dataset) ────────────────────────────────────
SBC_results %>%
  filter(sample == "partial") %>%
  select(i, true_k_name, true_l1, true_l2, true_b,
         predicted_k_name, predicted_l1, predicted_l2, predicted_b,
         normalized_score, exact_recovery) %>%
  arrange(i) %>%
  flextable() %>%
  colformat_double(digits = 2) %>%
  save_as_docx(path = here::here("results", "tbl-simulation-recapture_detail.docx"))
