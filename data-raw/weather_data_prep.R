# Read the raw CSV
colliOrientali <- read.csv("data-raw/colliOrientali_full.csv", stringsAsFactors = FALSE)

# Clean up column names
names(colliOrientali) <- c("Longitude", "Latitude", "Date", "Tmin", "Tmax", "Precipitation",
                           "RelativeHumidityMax", "RelativeHumidityMin", "WindSpeed", "Radiation")

# Convert date format (assumes mm/dd/yyyy or similar)
#colliOrientali$Date <- as.Date(colliOrientali$Date, format = "%m/%d/%Y")

# Optional: Add a "Site" column (needed by your package)
colliOrientali$Site <- "ColliOrientali"

# Reorder columns to match standard structure
colliOrientali <- colliOrientali[, c("Site", "Date", "Tmax", "Tmin", "Precipitation",
                                     "WindSpeed", "RelativeHumidityMax", "RelativeHumidityMin",
                                     "Radiation", "Latitude", "Longitude")]
# Save to /data as colliOrientali.rda
usethis::use_data(colliOrientali, overwrite = TRUE)
