library(here)
library(readr)
library(tibble)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(stringi)
library(forcats)
library(purrr)
library(scales)
library(ggplot2); theme_set(theme_bw())
library(mnormt) # for multivariate normal
library(LaplacesDemon) # for logistic transform
library(flextable) # for exporting tables in .docx

# load functions
invisible(lapply(list.files(here::here("R"), full.names = TRUE), source))  