# Read the raw CSV
targetSample <- read.csv("data-raw/yield_quality_data.csv", stringsAsFactors = FALSE) |>
  dplyr::filter(Site=='ColliOrientali',Variety=='Carmenere',Variable=='yield')

# Save to /data as colliOrientali.rda
usethis::use_data(targetSample, overwrite = TRUE)
