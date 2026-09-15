# Load data from pipeline
targets::tar_load(accuracy_supp)
targets::tar_load(error_tolerance)
id_branch <- 1
targets::tar_load(data_split_supp, branch = id_branch)
targets::tar_load(fit_hp_supp, branch = id_branch)
accuracy_supp_branch <- targets::tar_read(accuracy_supp, branch = id_branch)

# Source plotting function
source(here::here("R", "make_fig_england_accuracy.R"))

# fig_england_accuracy
fig_england_accuracy_supp <- make_fig_england_accuracy(accuracy_supp, error_tolerance, data_split_supp, fit_hp_supp, accuracy_supp_branch)

ggsave(
  here::here("results", "fig-england-accuracy_supp.pdf"),
  fig_england_accuracy_supp, 
  width = fig_w, height = 1.5*fig_h,
  dpi = 300
)

ggsave(
  here::here("results", "fig-england-accuracy_supp.jpeg"), fig_england_accuracy_supp, 
  width = fig_w, height = 1.5*fig_h, 
  dpi = 300
)
