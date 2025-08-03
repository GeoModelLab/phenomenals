# Clean environment
rm(list = ls())

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(readr)
library(forcats)
library(tibble)
library(purrr)

# Parameters file
df <- read.csv("parSearch.csv", sep = ";")

# Cleaning
df_clean <- df |>
  select(PhenoMeNal, Parameter, Value) |>
  filter(!is.na(Value)) |>
  mutate(
    Value = as.numeric(Value)
  )

# Stats
stats <- df_clean |>
  group_by(PhenoMeNal, Parameter) |>
  summarise(
    mean = mean(Value),
    sd = sd(Value),
    n = n(),
    min_val = min(Value),
    max_val = max(Value),
    range = max_val - min_val,
    .groups = "drop"
  )  %>%
  mutate(
    FacetLabel = paste0(
      PhenoMeNal, "\n", Parameter,
      " (μ=", round(mean, 2),
      ", σ=", round(sd, 2),
      ", n=", n, ")"
    )
  )

# Add this to force the order of FacetLabel based on PhenoMeNal
stats <- stats |>
  mutate(
    FacetLabel = factor(FacetLabel,
                        levels = stats %>%
                          arrange(factor(PhenoMeNal, levels = c("ColdF", "TempF", "DroughtF", "VPDeF","HeatF",  "LightF", "WindF", "DiseaseF"))) %>%
                          pull(FacetLabel))
  )

df_plot <- df_clean |>
  left_join(stats, by = c("PhenoMeNal", "Parameter")) |>
  mutate(FacetLabel = factor(FacetLabel, levels = levels(stats$FacetLabel)))

# Padding
padding <- stats |>
  transmute(
    PhenoMeNal, Parameter, FacetLabel,
    Value = list(c(min_val - 0.1 * range, max_val + 0.1 * range))
  ) %>%
  unnest(Value)

df_expanded <- bind_rows(df_plot, padding %>%
                           mutate(FacetLabel = factor(FacetLabel, levels = levels(stats$FacetLabel))))

# Colors
phenomenal_colors <- c(
  TempF = "#2ca02c",
  ColdF = "#1f78b4",
  HeatF = "#e6550d",
  LightF = "#ffd700",
  WindF = "#a6cee3",
  DiseaseF = "#b8860b",
  VPDeF = "#cb181d",
  DroughtF = "pink3"
)
dev.new()
# Plot
p <- ggplot(df_expanded, aes(x = Value)) +
  geom_density(data = df_plot, aes(fill = PhenoMeNal), alpha = 0.8) +
  geom_vline(data = stats, aes(xintercept = mean), color = "black", size = 1) +
  geom_vline(data = stats, aes(xintercept = mean - sd), color = "black", linetype = "dashed") +
  geom_vline(data = stats, aes(xintercept = mean + sd), color = "black", linetype = "dashed") +
  facet_wrap(~ FacetLabel, scales = "free", ncol = 4) +
  scale_fill_manual(values = phenomenal_colors, guide = "none") +
  labs(
    title = "Density Plots of PhenoMeNals Parameter Values",
    subtitle = "Mean (μ, solid), ±1 SD (dashed)",
    x = "Value", y = "Density"
  ) +
  theme_classic(base_size = 13) +
  theme(
    strip.text = element_text(size = 11),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = 'none'
  )
p
ggsave("parDistributions.png",height=7,width=11.5)

#TempF----
# Estrai parametri per TempF
params_df <- stats %>%
  filter(PhenoMeNal == "TempF", Parameter %in% c("Tmin", "Topt", "Tmax")) %>%
  select(Parameter, mean, sd)

temp_f <- function(T, Tmin, Topt, Tmax) {
  if (T < Tmin || T > Tmax) return(0)

  val <- tryCatch({
    ((Tmax - T)/(Tmax - Topt)) *
      ((T - Tmin)/(Topt - Tmin))^((Topt - Tmin)/(Tmax - Topt))
  }, warning = function(w) return(0),
  error = function(e) return(0))

  if (!is.finite(val) || is.na(val)) return(0) else return(val)
}

# Range temperatura
x_vals <- seq(5, 40, length.out = 200)

# Parametri medi e sd
Tmin_mu <- params_df$mean[params_df$Parameter == "Tmin"]
Topt_mu <- params_df$mean[params_df$Parameter == "Topt"]
Tmax_mu <- params_df$mean[params_df$Parameter == "Tmax"]

Tmin_sd <- params_df$sd[params_df$Parameter == "Tmin"]
Topt_sd <- params_df$sd[params_df$Parameter == "Topt"]
Tmax_sd <- params_df$sd[params_df$Parameter == "Tmax"]

# Costruisci tutte le combinazioni ± sd (2^3 = 8 combinazioni)
n_sim <- 600  # o più

set.seed(123)  # per riproducibilità

param_grid <- tibble(
  Tmin = runif(n_sim, Tmin_mu - Tmin_sd/2, Tmin_mu + Tmin_sd/2),
  Topt = runif(n_sim, Topt_mu - Topt_sd/2, Topt_mu + Topt_sd/2),
  Tmax = runif(n_sim, Tmax_mu - Tmax_sd/2, Tmax_mu + Tmax_sd/2)
) %>%
  filter(Tmin < Topt, Topt < Tmax)  # biologicamente valido

# Calcola tutte le curve in grigio
gray_curves <- param_grid %>%
  mutate(id = row_number()) %>%
  group_by(id) %>%
  summarise(
    x = list(x_vals),
    y = list(sapply(x_vals, function(x) temp_f(x, Tmin, Topt, Tmax))),
    .groups = "drop"
  ) %>%
  unnest(c(x, y))

# Calcola curva media
curve_mean <- tibble(
  x = x_vals,
  y = sapply(x_vals, function(x) temp_f(x, Tmin_mu, Topt_mu, Tmax_mu))
)

# Plot finale
n_values <- stats %>%
  filter(PhenoMeNal == "TempF", Parameter == "Tmin") %>%
  pull(n)

# Plot
ggplot() +
  geom_line(data = gray_curves, aes(x = x, y = y, group = id),
            color = "#b2dfb2", size = 0.1, alpha = 0.3) +
  geom_line(data = curve_mean, aes(x = x, y = y),
            color = "#2ca02c", size = 1.2) +

  # Add points at Tmin, Topt, Tmax
  geom_point(aes(x = Tmin_mu, y = 0), color = "darkgreen", size = 3) +
  geom_point(aes(x = Topt_mu, y = 1), color = "darkgreen", size = 3) +
  geom_point(aes(x = Tmax_mu, y = 0), color = "darkgreen", size = 3) +

  # Annotated labels placed near points, slightly offset to avoid overlap
  annotate("text", x = Tmin_mu, y = 0.05, label = expression(T[min]),
           hjust = 0.5, vjust = 0, size = 7) +
  annotate("text", x = Topt_mu, y = 0.9, label = expression(T[opt]),
           hjust = 0.5, vjust = -1, size = 7) +
  annotate("text", x = Tmax_mu+2, y = 0.05, label = expression(T[max]),
           hjust = 0.5, vjust = 0, size = 7) +

  labs(
    title = "TempF",
    x = "Temperature (°C)",
    y = "Response"
  ) +
  coord_cartesian(ylim = c(0, 1.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))+
  theme_classic(base_size = 19)


ggsave('TempF.png', width=6,height=4)


#ColdF----
# Funzione cold response
# Estrai parametri per TempF
params_df <- stats %>%
  filter(PhenoMeNal %in% c("ColdF",'TempF'), Parameter %in% c("Tmin", "Tcold")) %>%
  select(PhenoMeNal,Parameter, mean, sd)

cold_f <- function(T, Tmin, Tcrit) {
  midpoint <- (Tmin + Tcrit) / 2
  width <- abs(Tmin - Tcrit)
  ifelse(T > Tmin, 1,
         ifelse(T <= Tcrit, 0, 1 / (1 + exp(10 / -width * (T - midpoint)))))
}

# Range temperatura
x_vals <- seq(-5, 12, length.out = 200)  # adatto per stress da freddo

# Parametri medi e sd (estrai da df originale)
Tmin_mu <- params_df$mean[params_df$Parameter == "Tmin" & params_df$PhenoMeNal == "TempF"]
Tcrit_mu <- params_df$mean[params_df$Parameter == "Tcold" & params_df$PhenoMeNal == "ColdF"]

Tmin_sd <- params_df$sd[params_df$Parameter == "Tmin" & params_df$PhenoMeNal == "TempF"]
Tcrit_sd <- params_df$sd[params_df$Parameter == "Tcold" & params_df$PhenoMeNal == "ColdF"]

# Sampling combinazioni plausibili
n_sim <- 600
set.seed(123)
param_grid <- tibble(
  Tmin = runif(n_sim, Tmin_mu - Tmin_sd/2, Tmin_mu + Tmin_sd/2),
  Tcrit = runif(n_sim, Tcrit_mu - Tcrit_sd/2, Tcrit_mu + Tcrit_sd/2)
) %>%
  filter(Tcrit < Tmin)

# Calcolo delle curve
gray_curves <- param_grid %>%
  mutate(id = row_number()) %>%
  group_by(id) %>%
  summarise(
    x = list(x_vals),
    y = list(sapply(x_vals, function(x) cold_f(x, Tmin, Tcrit))),
    .groups = "drop"
  ) %>%
  unnest(c(x, y))

# Curva media
curve_mean <- tibble(
  x = x_vals,
  y = sapply(x_vals, function(x) cold_f(x, Tmin_mu, Tcrit_mu))
)

# Etichetta parametri
subtitle_text <- sprintf(
  "Tmin (°C) = %.2f ± %.2f | Tcold (°C) = %.2f ± %.2f",
  Tmin_mu, Tmin_sd, Tcrit_mu, Tcrit_sd
)

ggplot() +
  geom_line(data = gray_curves, aes(x = x, y = y, group = id),
            color = "#c6dbef", size = 0.1, alpha = 0.2) +
  geom_line(data = curve_mean, aes(x = x, y = y),
            color = "#1f78b4", size = 1.2) +

  # Points at Tmin and Tcold
  geom_point(aes(x = Tmin_mu, y = 1), color = "#1f78b4", size = 3) +
  geom_point(aes(x = Tcrit_mu, y = 0), color = "#1f78b4", size = 3) +

  # Parameter labels near points
  annotate("text", x = Tmin_mu, y = 1, label = expression(T[min]),
           hjust = 0.5, vjust = -0.4, size = 7) +
  annotate("text", x = Tcrit_mu, y = 0.05, label = expression(T[cold]),
           hjust = 0.5, vjust = 0, size = 7) +

  # Axes and formatting
  labs(
    title = "ColdF",
    x = "Temperature (°C)",
    y = "Response"
  ) +
  coord_cartesian(ylim = c(0, 1.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  theme_classic(base_size = 19)


# Save the plot
ggsave("ColdF.png", width = 6, height = 4)


#HeatF----
# Funzione per il calore
heat_f <- function(T, Tmax, Tcrit) {
  midpoint <- (Tmax + Tcrit) / 2
  width <- abs(Tcrit - Tmax)
  ifelse(T < Tmax, 1,
         ifelse(T >= Tcrit, 0, 1 / (1 + exp(10 / width * (T - midpoint)))))
}

# Selezione parametri da stats
params_df <- stats %>%
  filter(PhenoMeNal %in% c("TempF", "HeatF"), Parameter %in% c("Tmax", "Theat")) %>%
  select(PhenoMeNal, Parameter, mean, sd)

# Range temperatura per heat stress
x_vals <- seq(30, 42, length.out = 200)

# Estrai medie e sd
Tmax_mu <- params_df$mean[params_df$Parameter == "Tmax" & params_df$PhenoMeNal == "TempF"]
Tcrit_mu <- params_df$mean[params_df$Parameter == "Theat" & params_df$PhenoMeNal == "HeatF"]

Tmax_sd <- params_df$sd[params_df$Parameter == "Tmax" & params_df$PhenoMeNal == "TempF"]
Tcrit_sd <- params_df$sd[params_df$Parameter == "Theat" & params_df$PhenoMeNal == "HeatF"]

# Sampling combinazioni valide
n_sim <- 600
set.seed(123)
param_grid <- tibble(
  Tmax = runif(n_sim, Tmax_mu - Tmax_sd/2, Tmax_mu + Tmax_sd/2),
  Tcrit = runif(n_sim, Tcrit_mu - Tcrit_sd/2, Tcrit_mu + Tcrit_sd/2)
) %>%
  filter(Tmax < Tcrit)

# Curve campionate
gray_curves <- param_grid %>%
  mutate(id = row_number()) %>%
  group_by(id) %>%
  summarise(
    x = list(x_vals),
    y = list(sapply(x_vals, function(x) heat_f(x, Tmax, Tcrit))),
    .groups = "drop"
  ) %>%
  unnest(c(x, y))

# Curva media
curve_mean <- tibble(
  x = x_vals,
  y = sapply(x_vals, function(x) heat_f(x, Tmax_mu, Tcrit_mu))
)

# Etichetta parametri
subtitle_text <- sprintf(
  "Tmax (°C) = %.2f ± %.2f | Theat (°C) = %.2f ± %.2f",
  Tmax_mu, Tmax_sd, Tcrit_mu, Tcrit_sd
)

# Plot
ggplot() +
  geom_line(data = gray_curves, aes(x = x, y = y, group = id),
            color = "#fde0c5", size = 0.1, alpha = 0.45) +
  geom_line(data = curve_mean, aes(x = x, y = y),
            color = "#e6550d", size = 1.2) +

  # Points at Tmax and Theat
  geom_point(aes(x = Tmax_mu, y = 1), color = "#e6550d", size = 3) +
  geom_point(aes(x = Tcrit_mu, y = 0), color = "#e6550d", size = 3) +

  # Parameter labels
  annotate("text", x = Tmax_mu, y = 1, label = expression(T[max]),
           hjust = 0.5, vjust = -0.4, size = 7) +
  annotate("text", x = Tcrit_mu + 1, y = 0.05, label = expression(T[heat]),
           hjust = 0.5, vjust = 0, size = 7) +

  # Axes and labels
  labs(
    title = "HeatF",
    x = "Temperature (°C)",
    y = "Response"
  ) +
  scale_x_continuous(breaks = c(30, 33, 36, 39, 42, 45)) +
  coord_cartesian(ylim = c(0, 1.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  theme_classic(base_size = 19)

# Save the figure
ggsave("HeatF.png", width = 6, height = 4)


#VPD function----
# Funzione sigmoide per VPDeF
f_vpd_sigmoid <- function(VPD, VPD50, k) {
  1 / (1 + exp(k * (VPD - VPD50)))
}

# Estrai parametri da stats
params_df <- stats %>%
  filter(PhenoMeNal == "VPDeF", Parameter %in% c("VPDmin", "VPDmax")) %>%
  select(Parameter, mean, sd)

# Medie e SD
VPDmin_mu <- params_df$mean[params_df$Parameter == "VPDmin"]
VPDmax_mu <- params_df$mean[params_df$Parameter == "VPDmax"]

VPDmin_sd <- params_df$sd[params_df$Parameter == "VPDmin"]
VPDmax_sd <- params_df$sd[params_df$Parameter == "VPDmax"]

# Deriva VPD50 come punto medio tra i due
VPD50_mu <- (VPDmin_mu + VPDmax_mu) / 2

# Gamma VPD
x_vals <- seq(0, 5, length.out = 200)

# Simulazioni con sampling ±½ SD
n_sim <- 600
set.seed(123)
param_grid <- tibble(
  VPDmin = runif(n_sim, VPDmin_mu - VPDmin_sd/2, VPDmin_mu + VPDmin_sd/2),
  VPDmax = runif(n_sim, VPDmax_mu - VPDmax_sd/2, VPDmax_mu + VPDmax_sd/2)
) %>%
  filter(VPDmin < VPDmax) %>%
  mutate(VPD50 = (VPDmin + VPDmax) / 2)

# Curve simulate
k <- 5  # coefficiente di pendenza

gray_curves <- param_grid %>%
  mutate(id = row_number()) %>%
  group_by(id) %>%
  summarise(
    x = list(x_vals),
    y = list(sapply(x_vals, function(x) f_vpd_sigmoid(x, VPD50, k))),
    .groups = "drop"
  ) %>%
  unnest(c(x, y))

# Curva media
curve_mean <- tibble(
  x = x_vals,
  y = sapply(x_vals, function(x) f_vpd_sigmoid(x, VPD50_mu, k))
)

# Etichetta per sottotitolo
subtitle_text <- sprintf(
  "VPDmin = %.2f ± %.2f | VPDmax = %.2f ± %.2f",
  VPDmin_mu, VPDmin_sd, VPDmax_mu, VPDmax_sd, VPD50_mu
)

ggplot() +
  geom_line(data = gray_curves, aes(x = x, y = y, group = id),
            color = "orange", size = 0.1, alpha = 0.1) +
  geom_line(data = curve_mean, aes(x = x, y = y),
            color = "#cb181d", size = 1.2) +

  # Points at VPDmin and VPDmax
  geom_point(aes(x = VPDmin_mu, y = 1), color = "#cb181d", size = 3) +
  geom_point(aes(x = VPDmax_mu, y = 0), color = "#cb181d", size = 3) +

  # Parameter labels near the points
  annotate("text", x = VPDmin_mu, y = 1, label = expression(VPD[min]),
           hjust = 0.5, vjust = -0.4, size = 7) +
  annotate("text", x = VPDmax_mu + 0.1, y = 0.05, label = expression(VPD[max]),
           hjust = 0.5, vjust = 0, size = 7) +

  # Axes and labels
  labs(
    title = "VPDeF",
    x = "Vapor Pressure Deficit (kPa)",
    y = "Response"
  ) +
  coord_cartesian(ylim = c(0, 1.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  theme_classic(base_size = 19)

# Save the plot
ggsave("VPDeF.png", width = 6, height = 4)


#LightF----
# Funzione risposta alla luce
# Funzione risposta alla luce (MJ m⁻² h⁻¹)
f_light <- function(R, LightMax, k = 5) {
  1 - exp(-k * R / LightMax)
}

# Estrai LightMax da stats
params_df <- stats %>%
  filter(PhenoMeNal == "LightF", Parameter == "LightMax") %>%
  select(mean, sd)

# Parametri
LightMax_mu <- params_df$mean
LightMax_sd <- params_df$sd

# Asse R: radiazione in MJ m⁻² h⁻¹
x_vals <- seq(0, 1, length.out = 200)  # range adatto per MJ m⁻² h⁻¹

# Sampling ±½ SD
n_sim <- 600
set.seed(123)
LightMax_samples <- runif(n_sim, LightMax_mu - LightMax_sd/2, LightMax_mu + LightMax_sd/2)

# Calcolo delle curve campionate
gray_curves <- tibble(
  id = 1:n_sim,
  LightMax = LightMax_samples
) %>%
  rowwise() %>%
  mutate(
    x = list(x_vals),
    y = list(f_light(x_vals, LightMax))
  ) %>%
  unnest(c(x, y))

# Curva media
curve_mean <- tibble(
  x = x_vals,
  y = f_light(x_vals, LightMax_mu)
)

subtitle_text <- bquote(LightMax == .(round(LightMax_mu, 2)) %+-% .(round(LightMax_sd, 2)) ~ "MJ m"^{-2} ~ "h"^{-1})

ggplot() +
  geom_line(data = gray_curves, aes(x = x, y = y, group = id),
            color = "#ffd700", size = 0.1, alpha = 0.05) +
  geom_line(data = curve_mean, aes(x = x, y = y),
            color = "gold2", size = 1.2) +

  # Point at LightMax
  geom_point(aes(x = LightMax_mu, y = 1), color = "gold2", size = 3) +

  # Parameter label
  annotate("text", x = LightMax_mu, y = 1, label = expression(L[max]),
           hjust = 0.5, vjust = -0.32, size = 7) +

  # Axes and labels
  labs(
    title = "LightF",
    x = expression(Photosynthetically~Active~Radiation~"("~MJ~m^{-2}~h^{-1}*")"),
    y = "Response"
  ) +
  coord_cartesian(ylim = c(0, 1.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  xlim(0, 1) +
  theme_classic(base_size = 19) +
  theme(axis.title.x = element_text(size = 17.3))  # smaller x-axis title

# Save plot
ggsave("LightF.png", width = 6, height = 4)

#WindF----
f_wind <- function(W, WindMin, k = 1) {
  ifelse(W <= WindMin,
         1,
         exp(-k * (W - WindMin))
  )
}

# Estrai i parametri per WindF
params_df <- stats %>%
  filter(PhenoMeNal == "WindF", Parameter == "WindMin") %>%
  select(mean, sd)

WindMin_mu <- params_df$mean
WindMin_sd <- params_df$sd

# Range velocità vento (in m/s)
x_vals <- seq(0, 12, length.out = 200)

# Sampling
n_sim <- 600
set.seed(123)
WindMin_samples <- runif(n_sim, WindMin_mu - WindMin_sd/2, WindMin_mu + WindMin_sd/2)

# Calcolo curve multiple
gray_curves <- tibble(
  id = 1:n_sim,
  WindMin = WindMin_samples
) %>%
  rowwise() %>%
  mutate(
    x = list(x_vals),
    y = list(f_wind(x_vals, WindMin))
  ) %>%
  unnest(c(x, y))

# Curva media
curve_mean <- tibble(
  x = x_vals,
  y = f_wind(x_vals, WindMin_mu)
)

# Etichetta
subtitle_text <- bquote(WindMin == .(round(WindMin_mu, 2)) %+-% .(round(WindMin_sd, 2)) ~ "m"~s^{-1})


ggplot() +
  geom_line(data = gray_curves, aes(x = x, y = y, group = id),
            color = "#deebf7", size = 0.1, alpha = 0.14) +
  geom_line(data = curve_mean, aes(x = x, y = y),
            color = "#00BFFF", size = 1.2) +

  # Point at WindMin
  geom_point(aes(x = WindMin_mu, y = 1), color = "#00BFFF", size = 3) +

  # Parameter label
  annotate("text", x = WindMin_mu, y = 1, label = expression(W[min]),
           hjust = 0.5, vjust = -0.4, size = 7) +

  # Axes and labels
  labs(
    title = "WindF",
    x = expression(Wind~Speed~"("~m~s^{-1}*")"),
    y = "Response"
  ) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15)) +
  coord_cartesian(ylim = c(0, 1.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  theme_classic(base_size = 19)

# Save the figure
ggsave("WindF.png", width = 6, height = 4)


#DroughtF----

# Funzione Kc semplificata
# Funzione per la curva Kc
kc_curve_exp <- function(bbch, Kcini, Kcmax, k = 0.1, max_bbch = 65) {
  delta <- (1 - exp(-k * bbch)) / (1 - exp(-k * max_bbch))
  kc_val <- Kcini + (Kcmax - Kcini) * delta
  ifelse(bbch <= max_bbch, kc_val, Kcmax)
}

# Estrai parametri da stats
params_df <- stats %>%
  filter(PhenoMeNal == "DroughtF", Parameter %in% c("Kcini", "Kcmax")) %>%
  select(Parameter, mean, sd)

Kcini_mu <- params_df$mean[params_df$Parameter == "Kcini"]
Kcmax_mu <- params_df$mean[params_df$Parameter == "Kcmax"]
Kcini_sd <- params_df$sd[params_df$Parameter == "Kcini"]
Kcmax_sd <- params_df$sd[params_df$Parameter == "Kcmax"]

# BBCH range
x_vals <- seq(0, 89, length.out = 200)

# Simulazioni
n_sim <- 600
set.seed(123)
param_grid <- tibble(
  Kcini = runif(n_sim, Kcini_mu - Kcini_sd/2, Kcini_mu + Kcini_sd/2),
  Kcmax = runif(n_sim, Kcmax_mu - Kcmax_sd/2, Kcmax_mu + Kcmax_sd/2)
)

# Curve grigie (simulazioni)
gray_curves <- param_grid %>%
  mutate(id = row_number()) %>%
  group_by(id) %>%
  summarise(
    x = list(x_vals),
    y = list(kc_curve_exp(x_vals, Kcini, Kcmax)),
    .groups = "drop"
  ) %>%
  unnest(c(x, y))

# Curva media
curve_mean <- tibble(
  x = x_vals,
  y = kc_curve_exp(x_vals, Kcini_mu, Kcmax_mu)
)

# Etichetta
subtitle_text <- sprintf(
  "Kcini = %.2f ± %.2f | Kcmax = %.2f ± %.2f",
  Kcini_mu, Kcini_sd, Kcmax_mu, Kcmax_sd
)

# Plot
ggplot() +
  # Gray curves from sampled parameters
  geom_line(data = gray_curves, aes(x = x, y = y, group = id),
            color = "mistyrose", size = 0.1, alpha = 0.25) +

  # Mean curve
  geom_line(data = curve_mean, aes(x = x, y = y),
            color = "pink3", size = 1.2) +

  # Points at Kcini and Kcmax reference positions
  geom_point(aes(x = 0, y = Kcini_mu), color = "pink3", size = 3) +
  geom_point(aes(x = 65, y = Kcmax_mu), color = "pink3", size = 3) +

  # Parameter labels near the points
  annotate("text", x = 10, y = Kcini_mu, label = expression(Kc[ini]),
           hjust = 0.5, vjust = 0, size = 7) +
  annotate("text", x = 65, y = Kcmax_mu + 0.1, label = expression(Kc[full]),
           hjust = 0.5, vjust = 0, size = 7) +

  # Axes and theme
  labs(
    title = "DroughtF",
    x = "BBCH phase",
    y = "Crop Coefficient (Kc)"
  ) +
  xlim(0, 89) +
  coord_cartesian(ylim = c(0, 1.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  theme_classic(base_size = 19)
# Save
ggsave("DroughtF.png", width = 6, height = 4)


#DiseaseF
# --- Funzioni ---
temp_f <- function(T, Tmin, Topt, Tmax) {
  if (T < Tmin || T > Tmax) return(NA)
  val <- ((Tmax - T) / (Tmax - Topt)) *
    ((T - Tmin) / (Topt - Tmin))^((Topt - Tmin) / (Tmax - Topt))
  if (!is.finite(val)) return(NA) else return(val)
}

wetness_req <- function(T, Tmin, Topt, Tmax, Wmin, Wopt) {
  if (T < Tmin || T > Tmax) return(NA)  # fuori dal range fisiologico
  f_val <- temp_f(T, Tmin, Topt, Tmax)
  if (is.na(f_val) || f_val == 0) return(NA)  # protezione ulteriore
  W <- Wmin / f_val
  return(min(W, Wopt))  # limitato a Wopt
}


# --- Parametri da stats ---
params_df <- stats %>%
  filter(PhenoMeNal == "DiseaseF", Parameter %in% c("Tmin", "Topt", "Tmax", "WDmin", "WDopt")) %>%
  select(Parameter, mean, sd)

p_mu <- setNames(params_df$mean, params_df$Parameter)
p_sd <- setNames(params_df$sd, params_df$Parameter)

Tmin_mu <- p_mu["Tmin"]; Tmin_sd <- p_sd["Tmin"]
Topt_mu <- p_mu["Topt"]; Topt_sd <- p_sd["Topt"]
Tmax_mu <- p_mu["Tmax"]; Tmax_sd <- p_sd["Tmax"]
WDmin_mu <- p_mu["WDmin"]; WDmin_sd <- p_sd["WDmin"]
WDopt_mu <- p_mu["WDopt"]; WDopt_sd <- p_sd["WDopt"]

# --- Sampling ---
n_sim <- 300
set.seed(123)
sim_params <- tibble(
  Tmin = runif(n_sim, Tmin_mu - Tmin_sd/2, Tmin_mu + Tmin_sd/2),
  Topt = runif(n_sim, Topt_mu - Topt_sd/2, Topt_mu + Topt_sd/2),
  Tmax = runif(n_sim, Tmax_mu - Tmax_sd/2, Tmax_mu + Tmax_sd/2),
  WDmin = runif(n_sim, WDmin_mu - WDmin_sd/2, WDmin_mu + WDmin_sd/2),
  WDopt = runif(n_sim, WDopt_mu - WDopt_sd/2, WDopt_mu + WDopt_sd/2)
) %>%
  filter(Tmin < Topt, Topt < Tmax, WDmin < WDopt)

# --- Calcolo delle curve ---
x_vals <- seq(0, 45, length.out = 100)

# Curve grigie wetness
gray_curves <- sim_params %>%
  mutate(id = row_number()) %>%
  group_by(id) %>%
  summarise(
    x = list(x_vals),
    y = list(sapply(x_vals, function(x) wetness_req(x, Tmin, Topt, Tmax, WDmin, WDopt))),
    .groups = "drop"
  ) %>%
  unnest(c(x, y))

# Curve grigie temperatura
gray_curves_temp <- sim_params %>%
  mutate(id = row_number()) %>%
  group_by(id) %>%
  summarise(
    x = list(x_vals),
    y = list(sapply(x_vals, function(x) temp_f(x, Tmin, Topt, Tmax))),
    .groups = "drop"
  ) %>%
  unnest(c(x, y))

# Curve medie
curve_temp <- tibble(
  x = x_vals,
  y1 = sapply(x_vals, function(x) temp_f(x, Tmin_mu, Topt_mu, Tmax_mu))
)

curve_wet <- tibble(
  x = x_vals,
  y2 = sapply(x_vals, function(x) {
    if (x < Tmin_mu || x > Tmax_mu) {
      return(NA_real_)  # fuori range → costante
    } else {
      wet_vals <- sapply(1:nrow(sim_params), function(i) {
        wetness_req(x,
                    sim_params$Tmin[i], sim_params$Topt[i], sim_params$Tmax[i],
                    sim_params$WDmin[i], sim_params$WDopt[i])
      })
      return(mean(wet_vals, na.rm = TRUE))
    }
  })
)


# --- Etichetta ---
subtitle_text <- sprintf(
  "Tmin = %.1f ± %.1f | Topt = %.1f ± %.1f | Tmax = %.1f ± %.1f | WDmin = %.1f ± %.1f | WDopt = %.1f ± %.1f",
  Tmin_mu, Tmin_sd, Topt_mu, Topt_sd, Tmax_mu, Tmax_sd,
  WDmin_mu, WDmin_sd, WDopt_mu, WDopt_sd
)

ggplot() +
  # Wetness gray curves (scaled)
  geom_line(data = gray_curves, aes(x = x, y = y / WDopt_mu, group = id),
            color = "#eee2ba", size = 0.1, alpha = 0.25) +

  # Temperature gray curves
  geom_line(data = gray_curves_temp, aes(x = x, y = y, group = id),
            color = "gray80", size = 0.1, alpha = 0.15) +

  # Temperature mean curve (black)
  geom_line(data = curve_temp, aes(x = x, y = y1),
            color = "black", size = 1.2) +

  # Wetness mean curve (goldenrod)
  geom_line(data = curve_wet, aes(x = x, y = y2 / WDopt_mu),
            color = "#b8860b", size = 1.2) +

  # Points for temperature parameters
  geom_point(aes(x = Tmin_mu, y = 0.05), color = "black", size = 3) +
  geom_point(aes(x = Topt_mu, y = 1), color = "black", size = 3) +
  geom_point(aes(x = Tmax_mu, y = 0.05), color = "black", size = 3) +

  # Points for wetness requirement parameters (scaled)
  geom_point(aes(x = Tmin_mu, y = .95), color = "#b8860b", size = 3) +
  geom_point(aes(x = Tmax_mu, y = .95), color = "#b8860b", size = 3) +

  # Labels for temperature parameters (with double subscripts)
  annotate("text", x = Tmin_mu - 3, y = 0.05, label = expression(T[min*','*D]),
           hjust = 0.5, vjust = 0, size = 7) +
  annotate("text", x = Topt_mu, y = 1.05, label = expression(T[opt*','*D]),
           hjust = 0.5, vjust = 0, size = 7) +
  annotate("text", x = Tmax_mu + 2.2, y = 0.05, label = expression(T[max*','*D]),
           hjust = 0.5, vjust = 0, size = 7) +

  # Labels for wetness requirement (scaled on secondary axis)
  annotate("text", x = Tmin_mu, y = 1, label = expression(WD[min]),
           hjust = 0.5, vjust = 0, size = 7, color = "#b8860b") +
  annotate("text", x = Tmax_mu, y = 1, label = expression(WD[opt]),
           hjust = 0.5, vjust = 0, size = 7, color = "#b8860b") +

  # Axis settings
  scale_y_continuous(
    name = "Temperature Response",
    limits = c(0, 1.1),
    breaks = seq(0, 1, 0.2),
    sec.axis = sec_axis(~ . * WDopt_mu, name = "Wetness Requirement (h)")
  ) +
  labs(
    title = "DiseaseF",
    x = "Temperature (°C)"
  ) +
  coord_cartesian(xlim = c(5, 35), ylim = c(0, 1.2)) +
  theme_classic(base_size = 19) +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "#b8860b"),
    axis.text = element_text(color = "black")
  )

# Save plot
ggsave("DiseaseF.png", width = 6, height = 4)


