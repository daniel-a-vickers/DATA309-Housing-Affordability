# 00_import_data
# imports and collates all csv's from data folder in repository

# library imports
if (!requireNamespace("tidyverse", quietly = TRUE)) {install.packages("tidyverse")}

# load libraries
library(tidyverse)

# import all csv data sets in the repository
file_list <- list.files("data/", pattern = "*.csv", full.names = TRUE)


# Reads in data per file
read_data <- function(file_name) {
  read_csv(file_name, col_types = cols())
}


# read in all files
all_data <- lapply(file_list, read_data) %>%
  bind_rows()
