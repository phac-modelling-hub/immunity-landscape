# Load data from pipeline
targets::tar_load(accuracy)
targets::tar_load(error_tolerance)

# Prep data for plotting and summarizing
branch_names <- unique(stringr::str_extract(names(accuracy), "accuracy_[[:alnum:]]+"))
df <- purrr::map(branch_names, function(nm) {
  tibble::tibble(
    id_rep = accuracy[[paste0(nm, "_id_rep")]],
    score = accuracy[[paste0(nm, "_score_accuracy")]]
  )
}) |> bind_rows()

# --- Across reps

# Panel A: distribution of accuracy across reps
panel_A <- ggplot(df, aes(x = score)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = mean(df$score), color = "red") +
  annotate(
    geom = "label",
    x = mean(df$score),
    y = 300,
    label = stringr::str_wrap(
      glue::glue("Mean accuracy: {scales::percent_format()(mean(df$score))}"),
      10
    ),
    color = "red",
    size = 3
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "A: Distribution of accuracy scores",
    subtitle = glue::glue("across all {length(unique(df$id_rep))} subsamplings of the England data (error tolerance: {scales::percent_format()(error_tolerance)})"),
    x = "Accuracy per test point",
    y = "Count of test points"
  )

# Panel B: posterior draws with test points overtop, coloured by accuracy

# use make_england_posterior_plot()
# need 
# - posterior object: output from run_GP()
# - obs_df
# - title

# pull data and hyperparameters for specific subsample
id_branch <- 5
targets::tar_load(data_split, branch = id_branch)
targets::tar_load(fit_hp, branch = id_branch)
train <- data_split[[which(str_detect(names(data_split), "train"))]]
prov_values <- fit_hp[[which(str_detect(names(fit_hp), "prov_relation"))]]
hyperparams <- fit_hp[[which(str_detect(names(fit_hp), "(?<!score_)hyperparams"))]]
test <- data_split[[which(str_detect(names(data_split), "test"))]]
targets::tar_load(accuracy, branch = id_branch)
scores <- accuracy[[which(str_detect(names(accuracy), "score_accuracy"))]]

# get posterior draws
post2D <- run_GP(
  vax_dataset = train,
  prov_values = prov_values,
  k = get(hyperparams$k_name),
  l1 = hyperparams$l1,
  l2 = hyperparams$l2,
  b = hyperparams$b,
  meas_error = hyperparams$meas_error,
  show_plots = FALSE,
  ndrws = 100
)

# prep obs
train <- train |>
  mutate(
    y_label = factor(location, levels = levels(post2D$y_label))
  )
test <- test |>
  mutate(
    accuracy_score = scores,
    y_label = factor(location, levels = levels(post2D$y_label))
  )

# base plot
panel_B <- make_england_posterior_plot(
  post2D = post2D
) +
  # train data
  geom_point(data = train, aes(x = age_current, y = value, fill = "Training data"),
             colour = "grey50", size = 1.5, shape = 21) +
  scale_fill_manual(name = NULL, values = c("Training data" = "grey50")) +
  # test data with accuracy scores
  geom_point(data = test, aes(x = age_current, y = value, colour = accuracy_score),
             size = 1.5, shape = 16) +
  viridis::scale_colour_viridis(option = "turbo", direction = -1, begin = 0.35, labels = scales::label_percent(), limits = c(min(scores), 1)) +
  labs(
    title = "B: Posterior draws from model fit",
    subtitle = "for one subsample of the England data",
    colour = stringr::str_wrap(glue::glue("Accuracy score (the probability that the fitted model’s predictions would come within {scales::percent_format()(error_tolerance)} of the true test value)"), width = 40) 
  ) + 
  theme(
    legend.position = "bottom"
  )

fig_england_accuracy <- panel_A / panel_B + patchwork::plot_layout(heights = c(1, 2))

ggsave(
  here::here("results", "fig-england-accuracy.pdf"),
  fig_england_accuracy, 
  width = fig_w, height = 1.5*fig_h
)

ggsave(
  here::here("results", "fig-england-accuracy.jpeg"), fig_england_accuracy, 
  width = fig_w, height = 1.5*fig_h, 
  dpi = 300
)
