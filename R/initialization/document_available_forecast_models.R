
##
#### SETUP ####
##
# Load packages
devtools::load_all(".")
library(tidyverse)

# Disable scientific notation
options(scipen = 999)


##
#### BUILD MARKDOWN ####
##
# Build markdown to output location
rmarkdown::render(
  input = "R/initialization/Rmarkdown/document_available_forecast_models.Rmd",
  output_format = "html_document",
  output_dir = "inst/documentation",
  output_file = "forecast_models.html"
)