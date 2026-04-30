# Generates all model results needed for manuscript figures and tables.
# Source setup.R first, then run this file (or set rerun=TRUE in ms_figs_tables.qmd).
# Each output is saved to its own file in results/, overwriting existing outputs.

# ── Model framework definitions ────────────────────────────────────────────────
# l2_lim: plausible range for l2 used to filter the model selection grid before
#         identifying the best model. meas_error is chosen separately from the
#         CSV selection (edit here to change the value used for posteriors/LOPO).
l2_lim <- list(Gini = c(0.1, 50), LI = c(0.2, 25), VH = c(0.1, 8))

model_configs <- list(
  Gini = list(
    prov_values = "Gini",
    vax_dataset = vax_clean,
    meas_error  = 0.01,
    l2_lim      = l2_lim$Gini,
    csv         = "GPcombinations_Gini_witherror.csv",
    title_lab   = "Gini"
  ),
  LI = list(
    prov_values = "low_income_families",
    vax_dataset = vax_cleanLI,
    meas_error  = 0.01,
    l2_lim      = l2_lim$LI,
    csv         = "GPcombinations_lowincome_witherror.csv",
    title_lab   = "Low-income"
  ),
  VH = list(
    prov_values = "vaccine_hesitancy",
    vax_dataset = vax_cleanVH,
    meas_error  = 0.01,
    l2_lim      = l2_lim$VH,
    csv         = "GPcombinations_vaccinehesitancy_witherror.csv",
    title_lab   = "Vaccine hesitancy"
  )
)

# ── 1. Model selection (overwrites existing GPcombinations CSVs) ───────────────
rerun_select_GP <- F
if (rerun_select_GP == T) {
  cat("Running model selection...\n")
  for (nm in names(model_configs)) {
    cfg <- model_configs[[nm]]
    cat(" ", nm, "\n")
    select_GP(
      cfg$vax_dataset,
      prov_values = cfg$prov_values,
      k_list      = list(ksqexp = ksqexp, kexp = kexp),
      l1          = c(1, 1.25, 1.5, 1.75, 2, 2.25, 2.5),
      l2          = c(0.1, 0.2, 0.5, 1, 1.5, 2, 2.5, 3.0, 3.5, 4.0, 6.0, 8.0, 10, 25, 50),
      b           = c(0.5, 1, 1.5),
      meas_error  = c(0.5, 0.1, 0.05, 0.01, 0)
    ) %>%
      readr::write_csv(here::here("results", cfg$csv))
  }
}

# ── 2. Extract best params (k, l1, l2, b from max lppd_exact within l2_lim) ───
best_params <- imap(model_configs, function(cfg, nm) {
  best <- read_csv(here::here("results", cfg$csv), show_col_types = FALSE) %>%
    filter(l2 >= cfg$l2_lim[1], l2 <= cfg$l2_lim[2]) %>%
    slice_max(lppd_exact, n = 1)
  list(
    k_name     = best$k_name,
    l1         = best$l1,
    l2         = best$l2,
    b          = best$b,
    meas_error = cfg$meas_error   # chosen separately, not from CSV
  )
})

# ── 3. Posterior draws (saved as posterior_<name>.rds) ────────────────────────
cat("Running Canada posteriors...\n")
for (nm in names(model_configs)) {
  cfg <- model_configs[[nm]]
  bp  <- best_params[[nm]]
  cat(" ", nm, sprintf("(k=%s l1=%.2f l2=%.2f b=%.2f psi=%.2f)\n",
                       bp$k_name, bp$l1, bp$l2, bp$b, bp$meas_error))
  run_GP(
    vax_dataset = cfg$vax_dataset,
    prov_values = cfg$prov_values,
    k           = get(bp$k_name),
    l1          = bp$l1,
    l2          = bp$l2,
    b           = bp$b,
    meas_error  = bp$meas_error,
    show_plots  = FALSE
  ) %>%
    saveRDS(here::here("results", paste0("posterior_", nm, ".rds")))
}

# ── 4. LOPO comparisons (saved as lopo_<name>.rds) ────────────────────────────
cat("Running LOPO comparisons...\n")
for (nm in names(model_configs)) {
  cfg  <- model_configs[[nm]]
  bp   <- best_params[[nm]]
  k_fn <- get(bp$k_name)
  cat(" ", nm, "\n")
  best_lopo <- compute_lppd_lopo(cfg$vax_dataset, prov_values = cfg$prov_values,
                                 k = k_fn, l1 = bp$l1, l2 = bp$l2,
                                 b = bp$b, meas_error = bp$meas_error)
  age_lopo  <- compute_lppd_lopo(cfg$vax_dataset, prov_values = cfg$prov_values,
                                 k = k_fn, l1 = bp$l1, l2 = 500,
                                 b = bp$b, meas_error = bp$meas_error)
  tibble(
    province   = names(best_lopo$mean_lppd_by_province),
    best_model = best_lopo$mean_lppd_by_province,
    age_model  = age_lopo$mean_lppd_by_province,
    diff       = best_lopo$mean_lppd_by_province - age_lopo$mean_lppd_by_province
  ) %>%
    left_join(cfg$vax_dataset %>% distinct(location, pt),
              by = c("province" = "location")) %>%
    saveRDS(here::here("results", paste0("lopo_", nm, ".rds")))
}

# ── 5. Age assumption test data (saved as province_pairs_*.rds) ───────────────
cat("Computing pairwise age differences...\n")

compute_pairwise_diffs <- function(df) {
  df <- df %>% arrange(age_current)
  expand_grid(age1 = df$age_current, age2 = df$age_current) %>%
    filter(age1 < age2) %>%
    left_join(df %>% select(age1 = age_current, cov1 = value), by = "age1") %>%
    left_join(df %>% select(age2 = age_current, cov2 = value), by = "age2") %>%
    mutate(diff      = abs(cov1 - cov2),
           age_gap   = age2 - age1,
           gap_group = case_when(age_gap == 1 ~ "1 year apart",
                                 age_gap == 2 ~ "2 years apart",
                                 age_gap == 3 ~ "3 years apart",
                                 age_gap  > 3 ~ ">3 years apart"))
}

vax_noadults <- read_csv(
  here::here("data", "measles_vax-coverage-data-cleaned_noadults.csv"),
  show_col_types = FALSE
)
vax_noadults %>%
  filter(n_doses %in% c("1", "1+"), age_current >= 5, age_current <= 21) %>%
  group_by(location) %>%
  group_modify(~ compute_pairwise_diffs(.x)) %>%
  saveRDS(here::here("results", "province_pairs_1plus.rds"))

vax_noadults %>%
  filter(n_doses %in% c("2", "2+")) %>%
  group_by(location) %>%
  group_modify(~ compute_pairwise_diffs(.x)) %>%
  saveRDS(here::here("results", "province_pairs_2plus.rds"))

# ── 6. England model selection + posteriors ────────────────────────────────────
# cat("Loading England data...\n")
# current_year <- as.integer(format(Sys.Date(), "%Y"))
# vax_england <- read_csv(here::here("data", "ukhsa-chart-download-mmr1-regions.csv"),
#                         show_col_types = FALSE) %>%
#   mutate(age         = 5,
#          n_doses     = "1+",
#          value       = metric_value / 100,
#          location    = geography,
#          year_report = year,
#          age_current = current_year - year_report + age) %>%
#   select(location, year_report, age, n_doses, value, age_current)
# 
# england_model_configs <- list(
#   Gini = list(prov_values = "UK-Gini",               meas_error = 0.05,
#               csv = "GPcombinations_englandGini_witherror.csv", title_lab = "Gini"),
#   LI   = list(prov_values = "UK-low_income_families", meas_error = 0.05,
#               csv = "GPcombinations_englandLI_witherror.csv",   title_lab = "Low-income")
# )
# 
# cat("Running England model selection...\n")
# for (nm in names(england_model_configs)) {
#   ecfg <- england_model_configs[[nm]]
#   cat(" England", nm, "\n")
#   select_GP(
#     vax_england,
#     prov_values = ecfg$prov_values,
#     k_list      = list(ksqexp = ksqexp, kexp = kexp),
#     l1          = c(1, 1.25, 1.5, 1.75, 2, 2.25, 2.5),
#     l2          = c(0.1, 0.2, 0.5, 1, 1.5, 2, 2.5, 3.0, 3.5, 4.0, 6.0, 8.0, 10,
#                     25, 50, 75, 100, 150, 200, 250, 500),
#     b           = c(0.5, 1, 1.5),
#     meas_error  = c(0.5, 0.1, 0.05, 0.01, 0)
#   ) %>%
#     readr::write_csv(here::here("results", ecfg$csv))
# }
# 
# england_best_params <- imap(england_model_configs, function(ecfg, nm) {
#   best <- read_csv(here::here("results", ecfg$csv), show_col_types = FALSE) %>%
#     slice_max(lppd_exact, n = 1)
#   list(k_name = best$k_name, l1 = best$l1, l2 = best$l2, b = best$b,
#        meas_error = ecfg$meas_error)
# })
# 
# cat("Running England posteriors...\n")
# for (nm in names(england_model_configs)) {
#   ebp <- england_best_params[[nm]]
#   cat(" England", nm, sprintf("(k=%s l1=%.2f l2=%.2f b=%.2f psi=%.2f)\n",
#                                ebp$k_name, ebp$l1, ebp$l2, ebp$b, ebp$meas_error))
#   run_GP(
#     vax_dataset = vax_england,
#     prov_values = england_model_configs[[nm]]$prov_values,
#     k           = get(ebp$k_name),
#     l1          = ebp$l1,
#     l2          = ebp$l2,
#     b           = ebp$b,
#     meas_error  = ebp$meas_error,
#     show_plots  = FALSE
#   ) %>%
#     saveRDS(here::here("results", paste0("posterior_england_", nm, ".rds")))
# }

# ── 7. SBC (overwrites SBC_results_Gini.rds; synthetic CSVs not saved) ────────
rerun_SBC <- F
if (rerun_SBC == T) {
  cat("Running SBC...\n")
  
  k_select <- function(j) {
    if (j == 1) list(fn = ksqexp, name = "ksqexp") else list(fn = kexp, name = "kexp")
  }
  
  SBC_results_list <- vector("list", 50)
  for (i in 1:50) {
    k_object <- k_select(sample(0:1, 1))
    k  <- k_object$fn
    l1 <- sample(c(1, 1.25, 1.5, 1.75, 2, 2.25, 2.5), 1)
    l2 <- sample(c(0.2, 0.5, 1, 1.5, 2, 2.5, 3.0, 3.5, 4.0, 6.0, 8.0, 10, 25), 1)
    b  <- sample(c(0.5, 1, 1.5), 1)
    meas_error <- 0
    params <- list(k = k, k_name = k_object$name, l1 = l1, l2 = l2,
                   b = b, meas_error = meas_error)
    
    xvals      <- 5:21
    prov_vals  <- extract_province_relation("Gini", vax_dataset = vax_clean)
    prov_levels <- names(prov_vals)
    yvals      <- prov_vals * l2
    
    prior <- compute_prior2D(xvals = xvals, yvals = yvals, k = k, l1 = l1,
                             ndrws = 50, prov_levels = prov_levels, b = b)
    a <- sample(1:50, 1)
    synthetic_data <- prior %>%
      filter(draw == a) %>%
      mutate(age_current = x_i,
             location    = as.character(y_label),
             value       = invlogit(prior2D),
             n_doses     = "1+") %>%
      select(age_current, location, value, n_doses)
    
    # full synthetic data
    ms_full <- select_GP(synthetic_data, prov_values = "Gini",
                         k_list     = list(ksqexp = ksqexp, kexp = kexp),
                         l1         = c(1, 1.25, 1.5, 1.75, 2, 2.25, 2.5),
                         l2         = c(0.2, 0.5, 1, 1.5, 2, 2.5, 3.0, 3.5, 4.0, 6.0, 8.0, 10, 25),
                         b          = c(0.5, 1, 1.5),
                         meas_error = c(0))
    
    # partial synthetic data (same age×province grid as vax_clean)
    data_points <- vax_clean %>%
      filter(age_current >= 5, age_current <= 21, n_doses != "2+") %>%
      distinct(age_current, location)
    ms_partial <- select_GP(
      synthetic_data %>% semi_join(data_points, by = c("age_current", "location")),
      prov_values = "Gini",
      k_list      = list(ksqexp = ksqexp, kexp = kexp),
      l1          = c(1, 1.25, 1.5, 1.75, 2, 2.25, 2.5),
      l2          = c(0.2, 0.5, 1, 1.5, 2, 2.5, 3.0, 3.5, 4.0, 6.0, 8.0, 10, 25),
      b           = c(0.5, 1, 1.5),
      meas_error  = c(0)
    )
    
    extract_one <- function(ms, sample_name) {
      true_row <- ms %>%
        filter(k_name == params$k_name, l1 == params$l1, l2 == params$l2,
               b == params$b, meas_error == params$meas_error) %>%
        select(-model_no, -lppd_LOPO) %>%
        rename_with(~ paste0("true_", .x))
      pred_row <- ms %>%
        slice_max(lppd_exact, n = 1) %>%
        select(-model_no, -lppd_LOPO) %>%
        rename_with(~ paste0("predicted_", .x))
      true_row %>%
        mutate(i = i, sample = sample_name, .before = 1) %>%
        bind_cols(pred_row) %>%
        mutate(min_lppd = min(ms$lppd_exact), max_lppd = max(ms$lppd_exact))
    }
    
    SBC_results_list[[i]] <- bind_rows(
      extract_one(ms_full,    "full"),
      extract_one(ms_partial, "partial")
    )
    cat(" SBC iteration", i, "/50\n")
  }
  
  bind_rows(SBC_results_list) %>%
    mutate(
      normalized_score = (true_lppd_exact - min_lppd) / (max_lppd - min_lppd),
      exact_recovery   = (true_k_name == predicted_k_name & true_l1 == predicted_l1 &
                            true_l2 == predicted_l2 & true_b == predicted_b),
      k_correct        = true_k_name == predicted_k_name,
      l1_correct       = true_l1 == predicted_l1,
      l2_correct       = true_l2 == predicted_l2,
      b_correct        = true_b  == predicted_b
    ) %>%
    saveRDS(here::here("results", "SBC_results_Gini.rds"))
}

cat("results_setup.R complete.\n")
