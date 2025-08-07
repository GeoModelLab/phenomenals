## ----table, include=F, warning=FALSE, message=FALSE---------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=TRUE, warning=FALSE, message=FALSE------------------------
library(phenomenals)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(lubridate)

## ----load-data----------------------------------------------------------------
data(colliOrientali)
data(bbchSample)
parameters <- phenomenalsParameters

## ----inspect-inputs-----------------------------------------------------------
kable(head(colliOrientali), caption = "Example weather data") %>%
  kable_styling(font_size = 10)
kable(head(bbchSample), caption = "Example BBCH data") %>%
  kable_styling(font_size = 10)

## ----run-calibration----------------------------------------------------------
calib_result <- phenologyCalibration(
  weather_data = colliOrientali,
  referenceBBCH = bbchSample,
  phenomenalsParameters =phenomenalsParameters,
  start_year = 2006,
  end_year = 2012,
  sites = "ColliOrientali",
  varieties = "Carmenere",
  iterations = 100,
  timestep = "daily"
)

## ----result-structure---------------------------------------------------------
str(calib_result, max.level = 1)

## ----view-parameters----------------------------------------------------------
filtered_params <- calib_result$parameters |>
  dplyr::filter(site == "colliorientali", variety == "carmenere")

knitr::kable(filtered_params, caption = "Calibrated parameters for Carmenere at ColliOrientali")

## ----view-results, fig.width = 7, fig.height=3--------------------------------
ggplot(calib_result$phenology |> 
         mutate(Date=as.Date(Date,format='%m/%d/%Y'),
                year=year(Date)),aes(x=Date)) + 
  geom_line(aes(y=BBCHPhase))+
  geom_point(aes(y=BBCHRef))+
  theme_classic()+
  geom_line(aes(y=ChillState),col='blue4')



