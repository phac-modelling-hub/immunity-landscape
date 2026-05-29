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

ggsave(here::here("results", "fig-kexp-example_kernels.pdf"),
       fig_kernels, width = fig_w * 0.6, height = fig_h)
