# Load data from pipeline
targets::tar_load(accuracy)
targets::tar_load(error_tolerance)

# Source plotting function
source(here::here("R", "make_fig_england_accuracy.R"))

# fig_england_accuracy
fig_england_accuracy <- make_fig_england_accuracy(accuracy, error_tolerance)

# fig_england_accuracy

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
