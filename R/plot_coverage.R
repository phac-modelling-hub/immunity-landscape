#| label: plot_coverage

#' Plot vaccine coverage from our standardized format
#' 
#' @param df a data frame with four required columns: 
#'  - age: age (character)
#'  - value: vaccine coverage as a proportion (numeric)
#'  - location: geographic location (character)
#'  - n_doses: number of doses use to calculate vaccine coverage (character)
#' @param x the column name of the variable to plot on the x-axis (as a string)
#' @param plot_year_report should the plot include the year reported for each estimate? (logical)
#' @param ncol number of columns for the plot grid
plot_coverage <- function(df, x = "age", plot_year_report = TRUE, ncol = 3){
  labs_x <- case_when(
    x=="age" ~ "Age at estimate",
    x=="year_birth" ~ "Birth year",
    x=="age_current" ~ "Current age",
    T ~ x
  )  
  
  p <- (df
        |> {\(.) if (!is.numeric(.[[x]])) numericize(., !!sym(x)) else .}()
        |> {\(.) if ("n_doses" %in% colnames(.)) factorize(. , n_doses) else . }()
        |> {\(.) if ("year_report" %in% colnames(.)) factorize(. , year_report) else . }()
        |> ggplot(aes(x = !!sym(x), y = value, shape = n_doses))
        + facet_wrap(~ location, ncol = ncol)
        + scale_x_continuous(expand = expansion(add = 1))
        + scale_y_continuous(
          limits = c(0,1),
          labels = label_percent()
        )
        + scale_shape_manual(values = c(18, 16, 17, 15, 13))
        + labs(
          x = labs_x,
          y = "Vaccine coverage",
          shape = "Number of doses"
        )
  )
  
  if("year_report" %in% colnames(df) & plot_year_report){
    p <- p + geom_point(aes(colour = year_report), size = 2, stroke = 1.5) + scale_colour_viridis_d(option = "G", direction = -1, end = 0.9) + labs(linetype = "Year reported", colour = "Year reported") 
  } else {
    p <- p + geom_point(size = 2)
  }
  
  p
}