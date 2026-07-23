# Load various datasets used throughout our anaylsis

#' read cleaned data
vax_clean <- readr::read_csv(here::here("data", "coverage", "generated", "measles_vax-coverage-data-cleaned.csv"), show_col_types = FALSE)
vax_schedule <- readr::read_csv(here::here("data", "coverage", "raw", "measles_raw", "vax_schedule.csv"), show_col_types = FALSE)
popsize <- readr::read_csv(here::here("data", "pt-relation", "popsize.csv"), show_col_types = FALSE)
current_year <- as.integer(format(Sys.Date(), "%Y")) # for ageing estimates
age_pre1970 <- current_year - 1969 # min age of those born strictly before 1970
vax_cNICS <- readr::read_csv(here::here("data", "coverage", "raw", "measles_CNICS.csv"), show_col_types = FALSE) %>%
  mutate(age_current = current_year - year_birth)

#' cleaned data for use with low-income and vaccine hesitancy province relation indicators
vax_cleanLI <- vax_clean   # "low_income_families" now has data for all PTs
vax_cleanVH <- vax_clean %>% mutate(location = if_else(location %in% c("Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick"), "Atlantic region", location)) %>% mutate(location = if_else(location %in% c("Yukon", "Northwest Territories", "Nunavut"), "Northern region", location)) %>%  mutate(pt = if_else(location %in% c("Atlantic region"), "AR", pt)) %>% mutate(pt = if_else(location %in% c("Northern region"), "NR", pt)) %>% group_by(across(-value)) %>% summarise(value = mean(value), .groups = "drop")  # "vaccine_hesitancy" groups Atlantic PTs and Northen PTs
