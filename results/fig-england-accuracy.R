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
fig_england_accuracy <- ggplot(df, aes(x = score)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = mean(df$score), linetype = "dashed", color = "red") +
  annotate(
    geom = "label",
    x = mean(df$score),
    y = 325,
    label = stringr::str_wrap(
      glue::glue("Mean accuracy: {scales::percent_format()(mean(df$score))}"),
      10
    ),
    color = "red"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribution of accuracy scores",
    subtitle = glue::glue("across all {length(unique(df$id_rep))} subsamplings of England data (error tolerance: {scales::percent_format()(error_tolerance)})"),
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

post_england_subsample <- run_GP(
  vax_dataset = train,
  prov_values = prov_values,
  k = get(hyperparams$k_name),
  l1 = hyperparams$l1,
  l2 = hyperparams$l2,
  b = hyperparams$b,
  meas_error = hyperparams$meas_error,
  show_plots = FALSE
)

make_england_posterior_plot(
  post2D = post_england_subsample,
  obs_df = train,
  title = "Posterior draws for one England subsample"
)
