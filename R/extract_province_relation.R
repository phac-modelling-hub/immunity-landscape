#' Extract province relational values from a selection of socioeconomic datasets
#' 
#' Outputs a named ordered vector giving numerical y-axis values associated to each province, unscaled.
#' 
#' @param data Choice of: (character/ named numeric vector)
#'              - "GDP", for log(GDP) data from StatCan
#'              - 
#'              - a named ordered numeric vector giving y-axis values associated to each province,
#'               unscaled, in which case the function will return this vector unchanged.
#' @param vax_dataset Used only to filter out provinces with no public coverage data (tibble)              
extract_province_relation <- function(data="GDP", vax_dataset) {
  if (is.character(data)) {
    # read in data, with columns labelled 'location' and 'value'
    if (data=="GDP") {  # province GDP data from StatCan (log)
      province_relation_data <- readr::read_csv(here::here("data", "3610040201-noSymbol.csv"), show_col_types = F) %>%
        mutate(value = log(Dollars))
      
    } else if (data=="HIDT") {
      
    }
    
    # filter provinces with no public coverage data
    province_relation_data <- province_relation_data %>% filter(location %in% unique(vax_dataset$location))
    
    # convert to named vector
    prov_levels <- province_relation_data %>% pull(location) # useful shortcut for defining province factors
    prov_values <- province_relation_data %>% pull(value)
    names(prov_values) <- prov_levels
    
  } else if (is.numeric(data)) {
    prov_values <- data  # (do nothing)
  }
  
  return(prov_values)
}