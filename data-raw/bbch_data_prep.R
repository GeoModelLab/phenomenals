# Read the raw CSV
bbchSample <- read.csv("data-raw/referenceBBCH.csv", stringsAsFactors = FALSE) |>
  dplyr::filter(Site=='ColliOrientali',Variety=='Carmenere')

# Convert date
bbchSample$Date <- as.Date(bbchSample$Date, format = "%m/%d/%Y")

# Save to /data as colliOrientali.rda
usethis::use_data(bbchSample, overwrite = TRUE)

