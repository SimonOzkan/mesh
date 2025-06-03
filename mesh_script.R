## scripts for MESH project

# Relevant packages

required_packages <- c("dplyr","ggplot2", "tidyr", "purrr", "readr", "lubridate", "stringr", "forcats", "scales")

# Check if packages are installed
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}
# Load packages
lapply(required_packages, library, character.only = TRUE)
