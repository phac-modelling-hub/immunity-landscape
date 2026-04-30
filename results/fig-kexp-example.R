# fig-kexp-example.R
# Supp kexp-example: illustrates the difference between kexp and ksqexp kernels,
# and shows example posteriors for both on the Gini model.

# ── Panel A: kernel function shape comparison ──────────────────────────────────
r_seq <- seq(0, 10, length.out = 200)
kernel_curves <- bind_rows(
  tibble(r = r_seq, k = kexp(r_seq,   l = 2.5), kernel = "Exponential (kexp)"),
  tibble(r = r_seq, k = ksqexp(r_seq, l = 2.5), kernel = "Squared exponential (ksqexp)")
)

fig_kernels <- ggplot(kernel_curves, aes(x = r, y = k, colour = kernel, linetype = kernel)) +
  geom_line(linewidth = 0.9) +
  scale_colour_ms(name = NULL) +
  scale_linetype_manual(values = c("Exponential (kexp)"          = "solid",
                                   "Squared exponential (ksqexp)" = "dashed"),
                        name = NULL) +
  labs(x = "Distance r", y = "Covariance k(r)",
       title = "Kernel function shapes (l = 2.5, b = 1)") +
  theme(legend.position = "bottom")

# ── Panel B: posterior comparison — kexp (best model) vs ksqexp ───────────────
post_Gini_kexp <- readRDS(here::here("results", "posterior_Gini.rds"))

# Read best Gini params to use same l1/l2/b for ksqexp comparison
best_Gini <- read_csv(here::here("results", "GPcombinations_Gini_witherror.csv"),
                      show_col_types = FALSE) %>%
  filter(l2 >= 0.1, l2 <= 50) %>%
  slice_max(lppd_exact, n = 1)

post_Gini_ksqexp <- run_GP(
  vax_dataset = vax_clean,
  prov_values = "Gini",
  k           = ksqexp,
  l1          = best_Gini$l1,
  l2          = best_Gini$l2,
  b           = best_Gini$b,
  meas_error  = 0,
  show_plots  = FALSE
)

obs_Gini <- vax_clean %>% filter(age_current >= 5, age_current <= 21, n_doses != "2+")

make_kernel_post_plot <- function(post2D, obs_df, kernel_label) {
  ggplot(post2D) +
    geom_line(aes(x = x_i, y = post2D_constrained * 100, group = factor(draw)),
              alpha = 0.15, linewidth = 0.3) +
    geom_point(data = obs_df, aes(x = age_current, y = value * 100),
               colour = "red", size = 1.2) +
    scale_x_continuous(name = "Current age", limits = c(5, 21),
                       breaks = seq(5, 21, by = 5)) +
    scale_y_continuous(name = "Vaccine coverage (%)", limits = c(0, 100)) +
    facet_wrap(~ y_label) +
    ggtitle(paste0("Gini model — ", kernel_label))
}

fig_kexp_posterior   <- make_kernel_post_plot(post_Gini_kexp,   obs_Gini, "kexp")
fig_ksqexp_posterior <- make_kernel_post_plot(post_Gini_ksqexp, obs_Gini, "ksqexp")

ggsave(here::here("results", "fig-kexp-example_kernels.pdf"),
       fig_kernels, width = fig_w * 0.6, height = fig_h)
ggsave(here::here("results", "fig-kexp-example_kexp.pdf"),
       fig_kexp_posterior, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-kexp-example_ksqexp.pdf"),
       fig_ksqexp_posterior, width = fig_w, height = fig_h)
