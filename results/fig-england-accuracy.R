# Load data from pipeline
targets::tar_load(accuracy)
targets::tar_load(error_tolerance)
id_branch <- 3
targets::tar_load(data_split, branch = id_branch)
targets::tar_load(fit_hp, branch = id_branch)
accuracy_branch <- targets::tar_read(accuracy, branch = id_branch)

# Source plotting function
source(here::here("R", "make_fig_england_accuracy.R"))

# fig_england_accuracy
fig_england_accuracy <- make_fig_england_accuracy(accuracy, error_tolerance, data_split, fit_hp, accuracy_branch)
fig_england_accuracy

ggsave(
  here::here("results", "fig-england-accuracy.pdf"),
  fig_england_accuracy, 
  width = fig_w, height = 1.5*fig_h,
  dpi = 300
)

ggsave(
  here::here("results", "fig-england-accuracy.jpeg"), fig_england_accuracy, 
  width = fig_w, height = 1.5*fig_h, 
  dpi = 300
)
