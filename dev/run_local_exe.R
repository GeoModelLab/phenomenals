# Load and prepare ----
rm(list = ls())

remove.packages("phenomenals")
devtools::document()
#devtools::install(upgrade = "never")

library(phenomenals)
library(dplyr)
library(purrr)
library(readr)
library(tools)
library(tibble)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Parameters ----
start_year <- 1950
end_year <- 2024
iterations <- 100
timestep <- 'daily'
rolling_window <- 3
#evaluation_range <- list(c(0, 400))
evaluation_range <- list(c(100, 200), c(300,400))
multicollinearity_threshold <- 0.9
max_phenomenals <- 3
bin_size <- 1
target_traits <- c("yield")

# Input data ----
referenceBBCH <- read.csv("C:/Users/simoneugomaria.brega/OneDrive - CREA/Documenti - geoModelClan/agrarsense/papers/phenomenals/src_phenomenals/inputFiles/referenceData/referenceBBCH.csv")
allTarget_data <- read.csv("..//inst//bin//inputFiles//referenceData//yield_quality_data.csv")

allSites <- c('wurzburg', 'wurzburg', rep('napa',4), 'oysterBay', 'penedes', rep('Luxembourg',2), rep('ColliOrientali',20))
allVarieties <- c('Riesling', 'Riesling','CabernetS','Merlot','SauvignonB','Chardonnay', 'Sauvignon', 'Chardonnay', 'Riesling','MullerThurgau',
                  'CabernetS','Merlot','SauvignonB','Carmenere', 'Picolit', 'Pignolo','RefoscoPr','Schioppettino','TocaiFr','Ribolla',
                  'CabernetS','Merlot','SauvignonB','Carmenere', 'Picolit', 'Pignolo','RefoscoPr','Schioppettino','TocaiFr','Ribolla')
allTargets<-c('yield','sugar',rep('brix',4),'yield','yield',rep('yield',2),rep('yield',10),rep('sugar',10))

# Output folders and map ----
base_output_dir <- "outputs"
dir.create(base_output_dir, showWarnings = FALSE)

output_paths <- list(
  smoothed               = c("data", "smoothed"),
  processed              = c("data", "processed"),
  signalized             = c("data", "signalized"),
  correlations           = c("selection", "correlations"),
  multicollinearity_mtx  = c("selection", "multicollinearity_matrices"),
  multicollinearity_step = c("selection", "multicollinearity_step"),
  stepwise_step          = c("selection", "stepwise_step"),
  predictions            = c("results", "predictions"),
  coefficients           = c("results", "coefficients"),
  diagnostics            = c("results", "diagnostics"),
  relative_importance    = c("results", "relative_importance")
)

# Initialize collectors
all_parameters <- list()
all_phenology <- list()
all_phenomenals <- list()

i<-1

source('..//R//RunPhenomenals.R')
# Main loop ----
for (i in seq_along(allSites)) {
  this_site <- allSites[i]
  this_variety <- allVarieties[i]

  this_target_trait = allTargets[i]
  key <- paste(this_site, this_variety, this_target_trait, sep = "_")

  cat(glue::glue("🔄 Running PhenoMeNals for {key}...\n"))

  # Weather
  fileWeather <- paste0(
    "C:/Users/Administrator/OneDrive - CREA/Documenti - geoModelClan/agrarsense/papers/phenomenals/src_phenomenals/inputFiles/weather/daily/",
    this_site, ".csv"
  )
  weather_data <- read.csv(fileWeather)
  weather_data$Site <- file_path_sans_ext(basename(fileWeather))

  # BBCH
  this_referenceBBCH <- referenceBBCH %>%
    filter(Site == this_site, Variety == this_variety)

  # Calibration
  # result <- phenologyCalibration(
  #   weather_data = weather_data,
  #   referenceBBCH = this_referenceBBCH,
  #   phenomenalsParameters = phenomenalsParameters,
  #   start_year = start_year,
  #   end_year = end_year,
  #   sites = this_site,
  #   varieties = this_variety,
  #   iterations = iterations,
  #   timestep = timestep
  # )
  #
  # all_parameters[[key]] <- result[[1]]
  # all_phenology[[key]] <- result[[2]] %>%
  #   filter(site == this_site, variety == this_variety)

  # Target data

  this_target <- allTarget_data %>%
    filter(Site == this_site)


  # Model
  phenomenals <- runPhenomenals(
    weather_data = weather_data,
    target_data = this_target,
    phenomenalsParameters = phenomenalsParameters,
    start_year = start_year,
    end_year = end_year,
    sites = this_site,
    varieties = this_variety,
    timestep = timestep,
    target_traits = this_target_trait,
    rolling_window = rolling_window,
    evaluation_range = evaluation_range,
    multicollinearity_threshold = multicollinearity_threshold,
    max_phenomenals = max_phenomenals,
    bin_size = bin_size
  )

  all_phenomenals[[key]] <- phenomenals

  # ➕ Save multicollinearity matrices
  multi_dir <- file.path(base_output_dir, "multicollinearity_matrices")
  dir.create(multi_dir, recursive = TRUE, showWarnings = FALSE)

  multi_matrices <- phenomenals$selection$multicollinearity_matrices
  if (is.list(multi_matrices) && all(purrr::map_lgl(multi_matrices, is.matrix))) {
    purrr::iwalk(multi_matrices, function(mat, mat_name) {
      matrix_df <- as.data.frame(as.table(mat)) |>
        dplyr::rename(row = Var1, col = Var2, value = Freq)
      write_csv(matrix_df, file.path(multi_dir, paste0(key, "_", mat_name, ".csv")))
    })
  }

  # ➕ Save stepwise-selected predictors
  stepwise_dir <- file.path(base_output_dir, "stepwise_selected_predictors")
  dir.create(stepwise_dir, recursive = TRUE, showWarnings = FALSE)

  stepwise_list <- phenomenals$selection$stepwise_step
  if (is.list(stepwise_list)) {
    stepwise_selected <- tibble::tibble(
      key = names(stepwise_list),
      predictor = unlist(stepwise_list, use.names = FALSE)
    ) |>
      tidyr::separate(key, into = c("site", "variety", "target"), sep = "\\|") |>
      dplyr::mutate(group_key = paste(site, variety, target, sep = "_"))

    this_key <- paste(this_site, this_variety, this_target_trait, sep = "_")
    this_stepwise <- dplyr::filter(stepwise_selected, group_key == this_key)

    if (nrow(this_stepwise) > 0) {
      write_csv(this_stepwise, file.path(stepwise_dir, paste0(this_key, ".csv")))
    }
  }

  # Save results by category ----
  for (name in names(output_paths)) {
    path <- output_paths[[name]]
    obj <- purrr::pluck(phenomenals, !!!path)
    out_dir <- file.path(base_output_dir, name)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    if (is.data.frame(obj)) {
      write_csv(obj, file.path(out_dir, paste0(key, ".csv")))

    } else if (is.list(obj) && is.data.frame(obj[[1]])) {
      write_csv(obj[[1]], file.path(out_dir, paste0(key, ".csv")))

    } else if (is.matrix(obj)) {
      matrix_df <- as.data.frame(as.table(obj)) |>
        rename(row = Var1, col = Var2, value = Freq)
      write_csv(matrix_df, file.path(out_dir, paste0(key, ".csv")))

    } else if (is.list(obj) && all(map_lgl(obj, is.matrix))) {
      # list of matrices
      iwalk(obj, function(mat, mat_name) {
        matrix_df <- as.data.frame(as.table(mat)) |>
          rename(row = Var1, col = Var2, value = Freq)
        write_csv(matrix_df, file.path(out_dir, paste0(key, "_", mat_name, ".csv")))
      })
    }
  }

  # Save phenology & parameter calibration
  # write_csv(all_parameters[[key]], file.path(base_output_dir, "parameters", paste0(key, ".csv")))
  # write_csv(all_phenology[[key]], file.path(base_output_dir, "phenology", paste0(key, ".csv")))
}

# Save combined outputs ----
dir.create("outputs/rds", showWarnings = FALSE)

# parameters_df <- bind_rows(all_parameters, .id = "site_variety")
# phenology_df <- bind_rows(all_phenology, .id = "site_variety")
#
# saveRDS(parameters_df, "outputs/rds/all_parameters.rds")
# saveRDS(phenology_df, "outputs/rds/all_phenology.rds")
saveRDS(all_phenomenals, "outputs/rds/all_phenomenals.rds")

all_phenomenals<-readRDS("outputs/rds/all_phenomenals.rds")
# Extract outputs by section into master list ----
extract_nested_df <- function(obj_list, path) {
  map_dfr(obj_list, function(entry) {
    x <- purrr::pluck(entry, !!!path)
    if (is.data.frame(x)) return(x)
    else if (is.list(x) && is.data.frame(x[[1]])) return(x[[1]])
    else return(NULL)
  }, .id = "site_variety_trait")
}

all_outputs <- purrr::map(output_paths, ~ extract_nested_df(all_phenomenals, .x))
write_rds(all_outputs, 'all_outputs.rds')

cat("\n✅ All outputs saved successfully.\n")

all_outputs<-read_rds( 'all_outputs.rds')

# For simulated dates
library(tidyverse)
#phenology evaluation

# sim_dates <- phenology_df  |>
#   mutate(
#     parsed_date = as.POSIXct(Date, format = "%d/%m/%Y %H:%M:%S"),
#     year = as.integer(format(parsed_date, "%Y"))
#   ) |>
#   group_by(site, variety, year, bbch = BBCHPhase)  |>
#   summarise(sim_date = min(parsed_date), .groups = "drop") |>
#   filter(!is.na(bbch))
#
# ref_dates <- phenology_df %>%
#   mutate(
#     parsed_date = as.POSIXct(Date, format = "%d/%m/%Y %H:%M:%S"),
#     year = as.integer(format(parsed_date, "%Y"))
#   ) |>
#   group_by(site, variety, year, bbch = BBCHRef) |>
#   summarise(ref_date = min(parsed_date), .groups = "drop") |>
#   filter(!is.na(bbch))
#
# compare_df <- inner_join(sim_dates, ref_dates,
#                          by = c("site", "variety", "year", "bbch")) |>
#   filter(!is.na(year))
#
# #error metrics
# compare_df <- compare_df |>
#   mutate(
#     error_days = as.numeric(difftime(sim_date, ref_date, units = "days")),
#     abs_error = abs(error_days)
#   )
# metrics <- compare_df  |>
#   summarise(
#     MAE = mean(abs_error),
#     RMSE = sqrt(mean(error_days^2)),
#     Bias = mean(error_days),
#     N = n()
#   )
# library(ggplot2)
#
# ggplot(compare_df |> mutate(doy_sim=yday(sim_date),
#                             doy_ref=yday(ref_date)),
#        aes(x = doy_sim, y = doy_ref, color = as.factor(site))) +
#   geom_point(size = 2, alpha = 0.7) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   facet_wrap(~bbch, scales = "free") +
#   labs(
#     title = "Simulated vs Observed BBCH Dates",
#     x = "Observed (Reference) Date",
#     y = "Simulated Date",
#     color = "BBCH Stage"
#   ) +
#   theme_minimal()
#



#chart correlation phenology
## 2a. Chart - phenology dynamics ----
pheno_labels_dynamic_all <- all_outputs$correlations |>
  dplyr::mutate(label = dplyr::case_when(
    dplyr::between(Completion, 5, 15)    ~ "ENDO",
    dplyr::between(Completion, 45, 55)   ~ "ECO",
    relative_year == "year_1" & BBCHPhase %in% 5:15   ~ "DIFF",   # bud differentiation
    relative_year == "year_1" & BBCHPhase %in% 50:59   ~ "DEV",    # early development
    relative_year == "year_1" & BBCHPhase >= 10      ~ "SETUP",  # inflorescence set

    relative_year == "year_0" & BBCHPhase %in% 5:10   ~ "BUD",    # budbreak
    relative_year == "year_0" & BBCHPhase %in% 11:69  ~ "FLO",    # flowering to fruit set
    relative_year == "year_0" & BBCHPhase %in% 71:79  ~ "VER",    # veraison
    relative_year == "year_0" & BBCHPhase >= 81       ~ "MAT",    # ripening

    TRUE ~ NA_character_
  )) |>
  dplyr::filter(!is.na(label)) |>
  dplyr::group_by(target_trait, relative_year, label) |>
  dplyr::summarise(
    Completion = median(Completion, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    block = dplyr::case_when(
      relative_year == "year_1" ~ 0,
      relative_year == "year_0" ~ 2
    ),
    x_position = Completion + (block - 1) * 100 + 100,
    y_position = -0.7
  )

correlations<-all_outputs$correlations
ggplot(correlations |> filter(phenomenals=='TempF'), aes(x = cyclePerc, y = cor)) +
  stat_summary(fun.data=mean_se,geom = 'area',
               aes(y = ChillState / 300),
               fill = "steelblue", linewidth = 0.8, alpha = .4) +
  stat_summary(fun.data=mean_se,geom = 'area', aes(y = BBCHPhase / 100 / 2),
               fill = "green3", linewidth = 0.8, alpha = .4) +
  stat_summary(fun.data=mean_se,geom = 'col',
               aes(fill = sign, alpha = abs(cor)*2), width = 1) +
  geom_text(data = pheno_labels_dynamic_all,
            aes(x = x_position, y = y_position, label = label),
            inherit.aes = FALSE, angle = 90, vjust = -0.2, hjust = 0, size = 2.5, color = "black") +
  scale_fill_manual(values = c("Positive" = "slateblue4", "Negative" = "tomato3")) +
  scale_alpha_identity() +
  scale_x_continuous(breaks = c(50, 150 + 50), labels = c("Y-1", "Y0"), expand = c(0.01, 0)) +
  facet_wrap(~ paste0(site,"|",variety, "|",phenomenals,"|",target_trait), scales = "free_y",
             ncol =3 ) +
  labs(
    title = "Rolling Correlation between Climate Variables and All Target Traits (Binned)",
    subtitle = glue::glue("Bin size = {1}; Bars = correlation; lines = BBCH (green), dormancy (blue); labels = phases"),
    x = "Phenological completion across two seasons", y = "Pearson correlation"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10))
  )+
  ylim(-1,1)

coefficients<-all_outputs$coefficients

selected<-coefficients |>
  ungroup() |>
  group_by(site,term,target) |>
  slice_head() |>
  filter(term!='(Intercept)') |>
  select(-c(cyclePerc,target,site_variety_trait,estimate:p.value))

diagnostics<-all_outputs$diagnostics
library(ggplot2)
library(dplyr)
library(tidyr)
library(lemon)

# Prepare data
test_filtered <- diagnostics %>%
  filter(!variety %in% c('TocaiFr')) %>%
  mutate(
    cyclePerc_f = factor(cyclePerc, levels = sort(unique(cyclePerc))),
    var_site_target = paste(variety, site, target, sep = " | ")
  )

# Reshape data to long format for plotting
plot_data <- test_filtered %>%
  select(var_site_target, site, variety, target, cyclePerc_f,
         R2_full, RMSE_full, R2_loocv, RMSE_loocv) %>%
  pivot_longer(cols = c(R2_full, RMSE_full, R2_loocv, RMSE_loocv),
               names_to = "metric", values_to = "value") %>%
  mutate(
    metric = recode(metric,
                    "R2_full" = "R² (Full Model)",
                    "RMSE_full" = "RMSE (Full Model)",
                    "R2_loocv" = "R² (LOOCV)",
                    "RMSE_loocv" = "RMSE (LOOCV)",
    )
  )

# Plot with all varieties and custom facet labels
# Add a short label column for facet titles
plot_data <- plot_data %>%
  mutate(facet_label = paste0(
    toupper(substr(site, 1, 3)), " | ",
    toupper(substr(variety, 1, 3)), " | ",
    toupper(substr(target, 1, 3))
  ))

ggplot(plot_data, aes(x = cyclePerc_f, y = value, group = metric)) +
  # Gray rectangle from 100 to 200
  annotate("rect", xmin = 0, xmax = 100, ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.5) +
  # Full model as grey line
  geom_line(
    data = filter(plot_data, grepl("Full Model", metric)),
    aes(linetype = metric),
    color = "black",
    linewidth = 0.4
  ) +
  # LOOCV metrics as black lines
  geom_line(
    data = filter(plot_data, grepl("LOOCV", metric)),
    aes(linetype = metric),
    color = "grey55",
    linewidth = 0.4
  ) +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed")) +
  facet_wrap(~ facet_label, ncol = 7) +
  scale_y_continuous(limits = c(0, 1)) +
  # X-axis with fewer ticks (every 100)
  scale_x_discrete(
    breaks = c("100", "200"),
    labels = c("Year–1", "Year 0")
  ) +

  labs(
    x = "",
    y = "Performance metrics",
    linetype = NULL,
    shape = NULL
  ) +
  theme_classic() +
  theme_classic(base_size = 12) +
  theme(
    legend.position = 'top',
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(hjust = -.2)  # Shift right (try 0.3–0.5)
  )+
  guides(
    shape = guide_legend(override.aes = list(size = 3))
  )

ggsave("Figure4b.png", width = 12, height = 8, dpi = 300)

statistics<-plot_data |>
  filter(cyclePerc_f==400) |>
  group_by(site,metric, target) |>
  summarise(r2=mean(value))
write.csv(statistics,'statistics.csv')

smoothed=all_outputs$smoothed |>
  mutate(AridityF=1-AridityF,
         ColdF=1-ColdF,
         HeatF=1-HeatF) |>
  pivot_longer(TempF:DiseaseF,names_to='phenomenals')

library(dplyr)

se <- function(x, na.rm = FALSE) {
  x <- if (na.rm) na.omit(x) else x
  sd(x) / sqrt(length(x))
}

percentile_data <- smoothed |>
  mutate(doy = yday(Date)) |>
  filter(site %in% c('ColliOrientali', 'Luxembourg', 'penedes', 'wurzburg',
                     'oysterBay', 'coonawarra'),
         phenomenals != 'WindF') |>
  group_by(site, phenomenals, doy) |>
  summarize(
    pse = se(value, na.rm = TRUE),
    psd = sd(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    .groups = 'drop'
  )

ggplot(percentile_data |> mutate(date_fake = as.Date(doy, origin = "2020-01-01"),)) +
  # 25–75% ribbon
  geom_ribbon(aes(x = date_fake, ymin = mean-pse, ymax = mean+pse, fill = phenomenals), alpha = 0.7) +
  #geom_ribbon(aes(x = date_fake, ymin = mean-psd/2, ymax = max(1,(mean+psd)/2), fill = phenomenals), alpha = 0.3) +
  # Median line
  geom_line(aes(x = date_fake, y = mean, color = phenomenals), size = 0.5) +
  lemon::facet_rep_grid(site ~ phenomenals, scales = 'free', repeat.tick.labels = TRUE) +
  scale_color_manual(values = c('black', 'red', 'blue', 'gold3', 'green', 'brown', 'orange')) +
  scale_fill_manual(values = c('black', 'red', 'blue', 'gold3', 'green', 'brown', 'orange')) +
  scale_x_date(date_labels = "%b", date_breaks = "3 month")+
  theme_bw() +
  theme(legend.position = 'none')

