


source("./RWE3033/RWE3033 01_setup.R", echo = T)
source("./RWE3033/RWE3033 02_cohort.R", echo = T)
source("./RWE3033/RWE3033 03_treatment.R", echo = T)
source("./RWE3033/RWE3033 04_outcome.R", echo = T)
source("./RWE3033/codelist.R", echo = T)
source("./RWE3033/CCI.R", echo = T)
source("./RWE3033/RWE3033 05_demographics.R", echo = T)

rmarkdown::render(
  "./RWE3033/RWE3033 results.Rmd",
  output_file =  paste0("RWE3033 results ",lubridate::today(),".html"),
  output_dir = "./RWE3033/"
)
