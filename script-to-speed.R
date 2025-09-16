## Self-contained script for lppd() function. The following lines would benefit from speed up: lines 39-57.

# Step 1: initial set up.
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)
library(purrr)
library(mnormt) # for multivariate normal
library(LaplacesDemon) # for logistic transform

invisible(lapply(list.files(here::here("R"), full.names = TRUE), source))  # load functions

#' read cleaned data from 'take 2' approach
vax_clean <- readr::read_csv(here::here("data", "measles_vax-coverage-data-clean2.csv"), show_col_types = FALSE) %>% filter(!(pt %in% c("SK","YT","NB"))) #%>% filter(location %in% unique(pt_lookup()$location))  # filtering out sub-provinces etc estimates
vax_schedule <- readr::read_csv(here::here("data", "measles_raw", "vax_schedule.csv"), show_col_types = FALSE)
popsize <- readr::read_csv(here::here("data", "popsize.csv"), show_col_types = FALSE)
current_year <- as.integer(format(Sys.Date(), "%Y")) # for ageing estimates
age_pre1970 <- current_year - 1969 # min age of those born strictly before 1970


# Step 2: lppd() function. 

#' Calculate the lppd for one iteration of our GP model (i.e. for one choice of parameter-set {k, l1, l2, b}).
#' Note: relation of provinces currently uses GDP data.
#' 
#' @param vax_dataset data set of observed vaccine coverage data in our standardised format (tibble)
#' @param last_agecurrent last age for the GP model, i.e. largest x-variable entry (numeric)
#' @param ndrws specify number of draws to be taken from the prior and posterior distributions
#' @param k covariance function chosen from the following list: (function) **Functionality not added yet for matern.
#'          - ksqexp,
#'          - kexp,
#'          - kmatern.
#' @param l1 x-lengthscale parameter, for use with covariance functions ksqexp and kexp
#' @param l2 relative lengthscale of x values to y values, for use with covariance functions ksqexp and kexp
#' @param b scale for covariance function determining the output variance
lppd <- function(vax_dataset, last_agecurrent=30, ndrws=50, k=ksqexp, l1=NA, l2=NA, b=1) {
  #' prepare the multiple training datasets (take-one-out)
  vax_dataset <- vax_dataset %>% filter((age_current<=last_agecurrent) & n_doses!="2")  # filter out unused data (2-dose & older ages)
  vax_datasets <- map(1:nrow(vax_dataset), ~ vax_dataset[-.x, ])
  names(vax_datasets) <- paste0("vax_dataset", 1:nrow(vax_dataset))
  
  #' run model for each training set and extract the 50 draw estimates of the removed point
  lppd_value <- rep(NA,nrow(vax_dataset))
  for (i in 1:nrow(vax_dataset)) {
    xout <- vax_dataset[i, ] %>% pull(age_current)  # age_current of removed data point
    yout <- vax_dataset[i, ] %>% pull(location)  # province of removed data point
    zout <- vax_dataset[i, ] %>% pull(value)  # correct coverage value of removed data point
    ppd <- run_GP(vax_datasets[[i]], show_plots=F, last_agecurrent=last_agecurrent, ndrws=ndrws, k=k, l1=l1, l2=l2, b=b) %>%
      filter(x_i==xout & y_label==yout) %>% pull(post2D_constrained) %>% 
      between(zout - 0.01, zout + 0.01) %>% sum()  # freq of ppd evaluated at true removed value +/-1%
    lppd_value[i] <- ppd  # not taking logs currently; should be log(ppd) eventually
  }
  return(sum(lppd_value))
}

# Step 3: Call the function. Currently takes approx 60-70 seconds.

lppd(vax_clean, k=ksqexp, l1=1.5, l2=3, b=1)
