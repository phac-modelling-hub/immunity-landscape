#' Extract province relational values from a selection of socioeconomic datasets
#' 
#' Outputs a named ordered vector giving numerical y-axis values associated to each province, unscaled.
#' Each metric is coded such that large values are optimal i.e. we expect a positive correlation with vaccination coverage (by taking 1-x where necessary).
#' Note that "low_income_families" does not have data for NT, NU, YT.
#' 
#' @param data Choice of: (character/ named numeric vector)
#'              - "GDP", for log(GDP) data from StatCan;
#'              - "low_income_families" , for rate of children in low-income families (Market Basket Measure) from Health Inequalities Data Tool;
#'              - "vaccine_hesitancy", for prevalence of parents' vaccine hesitancy (refuse all + hesitant) from cNICS;
#'              - any named ordered numeric vector giving y-axis values associated to each province,
#'               unscaled, in which case the function will return this vector unchanged.
#' @param vax_dataset Used only to filter out provinces with no public coverage data (tibble)              
extract_province_relation <- function(data="GDP", vax_dataset) {
  if (is.character(data)) {
    # read in data, with columns labelled 'location' and 'value'
    if (data=="GDP") {  # GDP data from StatCan (log)
      province_relation_data <- readr::read_csv(here::here("data", "3610040201-noSymbol.csv"), show_col_types = F) %>%
        mutate(value = log(Dollars))
      
    } else if (data=="low_income_families") {  # % children in low-income families (Health Inequalities Data Tool, Market Basket Measure, age-standardized rate)
      province_relation_data <- readr::read_csv(here::here("data", "health-ineq-data-tool-children-in-low-income-families-MBM-ASR.csv"), show_col_types = F) %>%
        filter(Sex == "Both sexes") %>% mutate(value = (100 - `Age-standardized rate`), location = stri_trans_general(Region, "latin-ascii")) %>% select(location, value)  # 100-X for positive trend
      province_relation_data <- province_relation_data %>% mutate(value = as.numeric(value)) %>% arrange(value)
      
    } else if (data=="vaccine_hesitancy") {  # vaccine hesitancy among parents (cNICS, refuse all + hesitant)
      province_relation_data <- readr::read_csv(here::here("data", "cNICS-vaccine-hesitancy.csv"), show_col_types = F) %>%
        mutate(value = 100 - (refuse_all + hesitant)) %>% arrange(value)  # 100-X for positive trend
      
    } else if (data=="UK-GDP") {  # UK GDP 2023 data from ONS (log)
      province_relation_data <- readr::read_csv(here::here("data", "uk-ONSdownload-regionalgdp.csv"), show_col_types = F, skip=1) %>% # skip title row
        filter(ITL %in% c("ITL1","Other")) %>% mutate(location = `Region name`, value = log(`2023`)) %>%
        mutate(location = if_else(location == "East", "East of England", location)) %>%
        select(location, value) %>% arrange(value)
    } else if (data=="UK-low_income_families") {  # UK % children in low-income families (DWP Official Statistics, FYE 2023)
      province_relation_data <- readr::read_csv(here::here("data", "uk-officialstatistics-childreninlowincomefamilies.csv"), show_col_types = F, skip=1) %>% # skip title row
        add_row(location="England", value=21.296) %>% select(location, value) %>%  # estimated from regions
        mutate(value = (100 - value)) %>% arrange(value)
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