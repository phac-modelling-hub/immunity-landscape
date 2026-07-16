#' Extract province relational values from a selection of socioeconomic datasets
#' 
#' Outputs a named ordered vector giving numerical y-axis values associated to each province, unscaled.
#' Each metric is coded such that large values are optimal i.e. we expect a positive correlation with vaccination coverage (by taking 1-x where necessary).
#' Note that "low_income_families" does not have data for NT, NU, YT.
#' 
#' @param data Choice of: (character/ named numeric vector)
#'              - "GDP", for log(GDP) data from StatCan;
#'              - "low_income_families" , for 2021 rate of children in low-income families (Market Basket Measure) from Health Inequalities Data Tool;
#'              - "vaccine_hesitancy", for 2017 prevalence of parents' vaccine hesitancy (refuse all + hesitant) from cNICS;
#'              - "Gini", for 2020 Gini index on adjusted household after-tax income from StatCan;
#'              - any named ordered numeric vector giving y-axis values associated to each province,
#'               unscaled, in which case the function will return this vector unchanged.
#' @param vax_dataset Used only to filter out provinces with no public coverage data (tibble)              
extract_province_relation <- function(data="GDP", vax_dataset) {
  if (is.character(data)) {
    # read in data, with columns labelled 'location' and 'value'
    if (data=="GDP") {  # GDP data from StatCan (log)
      province_relation_data <- readr::read_csv(here::here("data", "pt-relation", "3610040201-noSymbol.csv"), show_col_types = F) %>%
        dplyr::mutate(value = log(Dollars))
      
    } else if (data=="low_income_families") {  # % children in low-income families (Health Inequalities Data Tool, Low-Income Cut-Off, age-standardized rate)
      province_relation_data <- readr::read_csv(here::here("data", "pt-relation", "health-ineq-data-tool-children-in-low-income-families-LICO-detailed.csv"), show_col_types = F, skip = 3) %>%
        dplyr::select(Geography, Stratifier, Sex, Numerator, Denominator, `Age-standardized rate`) %>% dplyr::filter(Sex == "Both sexes", Stratifier == "Overall") %>% 
        dplyr::mutate(location = Geography, crude_value = (100*as.numeric(Numerator)/as.numeric(Denominator))) %>% mutate(value=round(crude_value, 2)) %>% dplyr::select(location, value)  
      province_relation_data <- province_relation_data %>% dplyr::arrange(value)

    } else if (data=="vaccine_hesitancy") {  # vaccine hesitancy among parents (cNICS, refuse all + hesitant)
      province_relation_data <- readr::read_csv(here::here("data", "pt-relation", "cNICS-vaccine-hesitancy.csv"), show_col_types = F) %>%
        dplyr::mutate(value = (refuse_all + hesitant)) %>% dplyr::arrange(value)

    } else if (data=="Gini") {  # Gini index on adjusted household after-tax income, currently mean of 2015 and 2020 values
      province_relation_data <- readr::read_csv(here::here("data", "pt-relation", "9810009601_databaseLoadingData_StatCanGini.csv"), show_col_types = F) %>%
        dplyr::filter(`Inequality measures (5)` == "Gini index on adjusted household after-tax income") %>% dplyr::rename(location = GEO, value = VALUE, year = `Year (2)`) %>%
        dplyr::mutate(value = 100*value) %>% dplyr::select(location, year, value) %>% dplyr::group_by(location) %>% dplyr::summarise(value = mean(value)) %>% dplyr::arrange(value)

    } else if (data=="UK-GDP") {  # UK GDP 2023 data from ONS (log) https://www.ons.gov.uk/datasets/regional-gdp-by-year/editions/time-series/versions/6
      province_relation_data <- readr::read_csv(here::here("data", "pt-relation", "uk-ONSdownload-regionalgdp.csv"), show_col_types = F, skip=1) %>% # skip title row
        dplyr::filter(ITL %in% c("ITL1","Other")) %>% dplyr::mutate(location = `Region name`, value = log(`2023`)) %>%
        dplyr::mutate(location = if_else(location == "East", "East of England", location)) %>%
        dplyr::select(location, value) %>% dplyr::arrange(value)
      
    } else if (data=="UK-low_income_families") {  # UK % children in low-income families (DWP Official Statistics, FYE 2023)
      province_relation_data <- readr::read_csv(here::here("data", "pt-relation", "uk-officialstatistics-childreninlowincomefamilies.csv"), show_col_types = F, skip=1) %>% # skip title row
        dplyr::add_row(location="England", value=21.296) %>% dplyr::select(location, value) %>%  # estimated from regions
        dplyr::mutate(value = (100 - value)) %>% dplyr::arrange(value)

    } else if (data=="UK-Gini") {  # UK Gini index for total wealth (ONS, April 2016-March 2018)
      province_relation_data <- readr::read_csv(here::here("data", "pt-relation", "ONS_analysingregionaleconomicandwellbeingtrends_fig7.csv"), show_col_types = F) %>%
        dplyr::select(location, `April 2016 to March 2018`) %>% dplyr::rename(value =`April 2016 to March 2018`) %>% dplyr::arrange(value)

    }
    
    # filter provinces with no public coverage data
    province_relation_data <- province_relation_data %>% dplyr::filter(location %in% unique(vax_dataset$location))

    # convert to named vector
    prov_levels <- province_relation_data %>% dplyr::pull(location) # useful shortcut for defining province factors
    prov_values <- province_relation_data %>% dplyr::pull(value)
    names(prov_values) <- prov_levels
    
  } else if (is.numeric(data)) {
    prov_values <- data  # (do nothing)
  }
  
  return(prov_values)
}