library(dplyr)
library(ggplot2)

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
ggplot(df, aes(x = score)) +
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

# --- By rep

# Distribution of accuracy by rep
ggplot(df, aes(x = score)) +
  geom_histogram(bins = n_bins) +
  geom_vline(xintercept = mean(df$score), linetype = "dashed", color = "red") +
  scale_x_continuous(labels = scales::percent_format()) +
  facet_wrap(~id_rep) +
  labs(
    title = "Distribution of accuracy scores by replication",
    subtitle = glue::glue("error tolerance: {scales::percent_format()(error_tolerance)}"),
    x = "Accuracy per test point",
    y = "Count of test points"
  )

# Average accuracy by rep
df |>
  group_by(id_rep) |>
  summarize(mean_score = mean(score), .groups = "drop") |>
  ggplot(aes(x = mean_score)) +
  geom_histogram(bins = 10) +
  labs(
    title = "Mean accuracy per replication",
    subtitle = glue::glue("error tolerance: {scales::percent_format()(error_tolerance)}"),
    x = "Mean accuracy per replication",
    y = "Count"
  )
