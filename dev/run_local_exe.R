# dev/run-local.R
# Esegui solo in sviluppo locale
if (interactive()) {
  # Load functions in dev mode
  remove.packages("phenomenals")
  devtools::document()
  devtools::install(upgrade = "never")
  library(phenomenals)
  listAvailableVarieties()

  # Dati di esempio (crea tu o carica)
  weather_data <- read.csv('C:/Users/simoneugomaria.brega/OneDrive - CREA/Documenti - geoModelClan/agrarsense/papers/phenomenals/src_phenomenals/inputFiles/weather/daily/wurzburg.csv')
  weather_data$Site <- 'wurzburg'
  referenceBBCH <- read.csv('C:/Users/simoneugomaria.brega/OneDrive - CREA/Documenti - geoModelClan/agrarsense/papers/phenomenals/src_phenomenals/inputFiles/referenceData/referenceBBCH.csv')
  referenceBBCH$Site <- 'wurzburg'
  sites<-c('wurzburg')
  varieties<-c('Riesling')
  start_year=1970
  end_year=2024
  iterations=1
  is_calibration=TRUE
  timestep='daily'
  source("R//phenologyCalibration.R")
  # Esegui il test principale
  result <- phenologyCalibration(
    weather_data = weather_data,
    referenceBBCH = referenceBBCH,
    phenomenalsParameters,
    start_year = start_year,
    end_year = end_year,
    sites = "all",
    varieties="Merlot",#varieties,
    iterations = iterations,
    timestep='daily'
  )
  parameters<-result[[1]]
  phenology<-result[[2]]

  listAvailableVarieties()

  #Esegui test phenomenals
  rolling_window = 5
  evaluation_range = list(c(0,400))
  multicollinearity_threshold=0.8
  max_phenomenals=4
  bin_size=1
  sites=c('wurzburg')
  target_traits = c("yield")
  target_data <- read.csv("inst//bin//inputFiles//referenceData//yield_quality_data.csv")

  weather_data$Site<-'wurzburg'
  target_data$Site<-'wurzburg'
  varieties=c('Riesling')
  weather_data<-weather_data |>
    dplyr::rename(Latitude=lat,
           Longitude=lon)
  source("R//runPhenomenals.R")

  phenomenals<-runPhenomenals(weather_data = weather_data,
                              target_data = target_data,
                              phenomenalsParameters,
                              start_year = start_year,
                              end_year = end_year,
                              sites = sites,
                              varieties=varieties,
                              timestep='daily',
                              target_traits = target_traits,
                              rolling_window = 1,
                              evaluation_range = list(c(0,200)),
                              multicollinearity_threshold=0.8,
                              max_phenomenals=max_phenomenals,
                              bin_size=1)

  library(tidyverse)
  smoothed<-phenomenals$data$smoothed |>
    mutate(year=year(Date))

  ggplot(smoothed) +
    geom_line(aes(x=CycleCompletion, y = TempF))+
    facet_wrap(~year)

  processed<-phenomenals$data$processed

  ggplot(processed) +
    geom_line(aes(x=cyclePercentage, y = TempF))+
    facet_wrap(target~year)


  signalized <- phenomenals$data$signalized

Y}
