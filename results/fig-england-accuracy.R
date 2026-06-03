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

# Distribution of accuracy across reps
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
