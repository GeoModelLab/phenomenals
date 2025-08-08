#' Run phenomenals
#'
#' This function runs the Phenomenals method by passing weather data and target traits (e.g., yield, Brix, sugar) directly from R to the underlying C# executable.
#' Users do not need to manage configuration files or binaries manually. All paths and execution logic are handled internally, and only core settings are exposed.
#'
#' @param weather_data A data frame containing hourly weather data. Must include the following columns: `Latitude`, `Longitude`, `DateTime` (as POSIXct or character), `Temperature` (°C), `Precipitation` (mm), `RelativeHumidity` (%), `Radiation` (MJ/m²), and `WindSpeed` (m/s).
#' @param target_data A data frame containing the target data (e.g., yield, brix, berry number, malic acid). The dataframe must include: `Site`, `Latitude`, `Longitude`, `Variety`, `Year`, `Variable`, `Value`, and `Unit`.
#' @param phenomenalsParameters A nested list of model parameters (usually loaded from `phenomenals::phenomenalsParameters`), structured as `list[species][[class]][[parameter]]`, where each parameter is a list with:
#'   - `min`: Minimum calibration value (numeric)
#'   - `max`: Maximum calibration value (numeric)
#'   - `value`: Default value used in simulation (numeric)
#'   - `calibration`: Logical; `TRUE` if the parameter is subject to calibration
#'
#'   You can start with `phenomenals::phenomenalsParameters` and modify it as needed. This list will be automatically converted to the CSV format required by the C# executable.
#' @param start_year The first year of the simulation or calibration period (numeric, default is 2000).
#' @param end_year The last year of the simulation or calibration period (numeric, default is 2025).
#' @param sites A character vector of site names (e.g., `"ColliOrientali"`).
#' @param varieties A character vector of variety names to include in the calibration (default: `"all"`).
#' @param timestep Time step of the input weather data; must be `"daily"` (default) or `"hourly"`.
#' @param rolling_window The window pane size for phenomenals smoothing (default is 5 days).
#' @param target_traits The target traits on which applying the phenomenals (default is `"yield"`).
#' @param evaluation_range A list of numeric vectors specifying the percentage range of the plant cycle to compute phenomenals (0–200; 0–100 = dormancy, 101–200 = growing season). (default is `list(c(0,200))`)
#' @param multicollinearity_threshold The threshold to consider phenomenals multicorrelations on each unit of cycle percentage (default is 0.8).
#' @param max_phenomenals The maximum number of phenomenals to retain (default is 4).
#' @param bin_size The range of cycle percentage to aggregate phenomenals (default is 1).
#'
#' @return A named list with three components:
#' \describe{
#'   \item{data}{Raw and processed phenological signals}
#'   \item{selection}{Results from signal selection (correlation, multicollinearity, stepwise)}
#'   \item{results}{Model diagnostics, predictions, coefficients, and variable importance}
#' }
#'
#' @examples
#' \dontrun{
#' result <- phenologyCalibration(
#'   weather_data = colliOrientali,
#'   target_data = targetSample,
#'   phenomenalsParameters,
#'   start_year = 2010,
#'   end_year = 2020,
#'   sites = "ColliOrientali",
#'   varieties = "Merlot",
#'   timestep = 'daily',
#'   rolling_window = 5,
#'   target_traits = c('yield'),
#'   evaluation_range = list(c(0,400)),
#'   multicollinearity_threshold=0.8,
#'   max_phenomenals=4,
#'   bin_size=1)
#
#' head(result$phenomenals$processed)
#' }
#'
#' @export
#' @importFrom dplyr filter mutate select arrange case_when group_by summarise rename ungroup left_join pull distinct slice_head n_distinct
#' @importFrom tidyr pivot_longer pivot_wider unnest
#' @importFrom purrr map map_dfr map_dbl pmap_lgl flatten compact set_names
#' @importFrom broom tidy
#' @importFrom glue glue
#' @importFrom caret train trainControl
#' @importFrom relaimpo calc.relimp
#' @importFrom jsonlite write_json
#' @importFrom data.table fread as.data.table
#' @importFrom lubridate yday mdy_hms year wday month day hour minute second isoweek week quarter mday
#' @importFrom zoo rollapply
#' @importFrom MASS stepAIC
#' @importFrom janitor clean_names
#' @importFrom stats cor sd coef lm as.formula na.omit complete.cases
#' @importFrom utils read.csv write.csv write.table
runPhenomenals <- function(weather_data,
                           target_data,
                           phenomenalsParameters,
                           start_year = 2000,
                           end_year = 2025,
                           sites = "all",
                           varieties = "all",
                           timestep = "daily",
                           target_traits = c('yield'),
                           rolling_window = 5,
                           evaluation_range = list(c(0,200)),
                           multicollinearity_threshold=0.8,
                           max_phenomenals=4,
                           bin_size=1)
{
  #VALIDATION----

  pkg_path <- system.file("", package = "phenomenals")
  exe_path <- system.file("bin", "phenomenals.exe", package = "phenomenals")
  if (!file.exists(exe_path)) stop("❌ Executable not found.")

  list_files_out <- list.files(
    path = file.path(dirname(exe_path), "outputsValidationDaily"),
    pattern = "\\.csv$",
    full.names = TRUE
  )

  # Remove all matching files
  if (length(list_files_out) > 0) {file.remove(list_files_out)}


  suppressPackageStartupMessages({
    library(ggplot2)
    library(lattice)
    library(caret)
  })

  # Check weather_data
  if (!is.data.frame(weather_data)) stop("❌ 'weather_data' must be a data frame.")
  # Case-insensitive check for 'Site'
  site_col <- names(weather_data)[tolower(names(weather_data)) == "site"]
  if (length(site_col) == 0) {
    stop("❌ 'weather_data' must contain a 'Site' column (case-insensitive).")
  } else {
    names(weather_data)[names(weather_data) == site_col] <- "Site"
  }

  # Check phenomenalsParameters
  if (!is.list(phenomenalsParameters)) stop("❌ 'phenomenalsParameters' must be a nested list.")

  # Check years
  if (!is.numeric(start_year) || length(start_year) != 1) stop("❌ 'start_year' must be a single numeric value.")
  if (!is.numeric(end_year) || length(end_year) != 1) stop("❌ 'end_year' must be a single numeric value.")
  if (start_year > end_year) stop("❌ 'start_year' must be less than or equal to 'end_year'.")

  # Check sites
  if (!(is.character(sites) || is.null(sites))) stop("❌ 'sites' must be a character vector or 'all'.")

  # Check varieties
  if (!(is.character(varieties) || is.null(varieties))) stop("❌ 'varieties' must be a character vector or 'all'.")

  # Check timestep
  if (!timestep %in% c("daily", "hourly")) {
    stop("❌ 'timestep' must be either 'daily' or 'hourly'.")
  }

  # Check rolling_window
  if (!is.numeric(rolling_window) || length(rolling_window) != 1 || rolling_window < 1) {
    stop("❌ 'rolling_window' must be a single positive number.")
  }

  # Check evaluation_range
  if (!is.list(evaluation_range) || any(!sapply(evaluation_range, function(x) is.numeric(x) && length(x) == 2))) {
    stop("❌ 'evaluation_range' must be a list of numeric vectors of length 2.")
  }

  # Check multicollinearity_threshold
  if (!is.numeric(multicollinearity_threshold) || length(multicollinearity_threshold) != 1 ||
      multicollinearity_threshold < 0 || multicollinearity_threshold > 1) {
    stop("❌ 'multicollinearity_threshold' must be a numeric value between 0 and 1.")
  }

  # Check max_phenomenals
  if (!is.numeric(max_phenomenals) || length(max_phenomenals) != 1 || max_phenomenals < 1) {
    stop("❌ 'max_phenomenals' must be a single positive number.")
  }

  # Check bin_size
  if (!is.numeric(bin_size) || length(bin_size) != 1 || bin_size <= 0) {
    stop("❌ 'bin_size' must be a single positive number.")
  }

  #VARIETIES CHECK----
  # === Load parameter files ===

  # Define paths
  pkg_path <- system.file("", package = "phenomenals")
  param_dir <- file.path(pkg_path, "bin", "calibratedVarieties")
  output_dir <- param_dir  # Use the same for reading/writing

  # List parameter files
  param_files <- list.files(param_dir, pattern = "^parameters_.*\\.csv$", full.names = TRUE)

  # Extract Site and Variety from filenames
  info <- strcapture(
    "^parameters_([^_]+)_(.+)\\.csv$",
    basename(param_files),
    proto = list(Site = character(), Variety = character())
  )

  # Combine metadata and file paths
  available_params <- data.frame(info, File = basename(param_files), stringsAsFactors = FALSE)

  # === Read and collect all parameter data ===
  all_params_list <- list()

  for (i in seq_along(param_files)) {
    df <- read.csv(param_files[i], stringsAsFactors = FALSE)
    if (!all(c("param", "value") %in% names(df))) {
      warning(paste0("⚠️ Skipping invalid file: ", param_files[i]))
      next
    }

    df$Site <- info$Site[i]
    df$Variety <- info$Variety[i]
    all_params_list[[i]] <- df
  }

  # Combine all into one dataframe
  all_params <- do.call(rbind, all_params_list)

  # Expand 'all'
  target_varieties <- if (identical(varieties, "all")) unique(all_params$Variety) else varieties
  target_sites <- if (identical(sites, "all")) unique(all_params$Site) else sites

  # Validate
  missing_varieties <- setdiff(target_varieties, unique(all_params$Variety))
  if (length(missing_varieties) > 0) {
    for (v in missing_varieties) {
      stop(paste0("❌ Variety '", v, "' not found in any calibration. Please perform calibration first."))
    }
  }
  valid_varieties <- setdiff(target_varieties, missing_varieties)

  # Initialize outputs
  averaged_parameters_df <- data.frame()
  temporary_files_to_delete <- character()

  if (!identical(varieties, "all")) {
    for (v in valid_varieties) {
      matching_rows <- available_params[available_params$Variety == v, ]
      calibrated_sites <- matching_rows$Site

      for (s in target_sites) {
        output_filename <- paste0("parameters_", s, "_", v, ".csv")
        output_filepath <- file.path(output_dir, output_filename)

        if (s %in% calibrated_sites) {
          message(paste0("✅ Variety '", v, "' is calibrated for site '", s, "'."))

          file_name <- matching_rows$File[matching_rows$Site == s]
          file_path <- file.path(param_dir, file_name)

          df <- read.csv(file_path, stringsAsFactors = FALSE)
          if (!all(c("param", "value") %in% names(df))) {
            stop(paste0("❌ Invalid parameter file format for ", v, " at ", s))
          }

          df$Variety <- v
          df$Site <- s
          df <- df[, c("Variety", "Site", "param", "value")]

          # Ensure Site is correctly replaced in the output file
          df_to_write <- df
          if ("Site" %in% names(df_to_write)) {
            df_to_write$Site <- s
          }
          if ("Variety" %in% names(df_to_write)) {
            df_to_write$Variety <- v
          }

          # Write only the two columns expected by the C# executable
          write.csv(df_to_write[, c("param", "value")], output_filepath, row.names = FALSE, quote = FALSE)

          #message(glue::glue("📄 Written averaged parameter file for {v} at {s}:"))
          averaged_parameters_df <- rbind(averaged_parameters_df, df)

        } else {
          message(paste0("⚠️ Variety '", v, "' is calibrated, but not for site '", s, "'."))
          cat("Do you want to proceed using averaged parameters? (Y = YES, N = NO): ")
          user_input <- readline()

          if (user_input == "Y") {
            param_paths <- file.path(param_dir, matching_rows$File)

            param_list <- lapply(param_paths, function(file) {
              df <- read.csv(file, stringsAsFactors = FALSE)
              if (!all(c("param", "value") %in% names(df))) {
                stop(paste0("❌ Invalid parameter file format: ", file))
              }
              df
            })

            if (length(param_list) == 1) {
              df <- param_list[[1]]
            } else {
              merged_params <- Reduce(function(x, y) merge(x, y, by = "param", all = TRUE), param_list)
              value_cols <- setdiff(names(merged_params), "param")
              merged_params$avg_value <- rowMeans(merged_params[, value_cols, drop = FALSE], na.rm = TRUE)
              df <- data.frame(param = merged_params$param, value = merged_params$avg_value)
            }

            df$Variety <- v
            df$Site <- s
            df <- df[, c("Variety", "Site", "param", "value")]

            # Ensure correct site/variety are written
            df_to_write <- df
            if ("Site" %in% names(df_to_write)) {
              df_to_write$Site <- s
            }
            if ("Variety" %in% names(df_to_write)) {
              df_to_write$Variety <- v
            }

            # Write only the two columns expected by the C# executable
            write.csv(df_to_write[, c("param", "value")], output_filepath, row.names = FALSE, quote = FALSE)

            #message(glue::glue("📄 Written averaged parameter file for {v} at {s}"))
            temporary_files_to_delete <- c(temporary_files_to_delete, output_filepath)
            averaged_parameters_df <- rbind(averaged_parameters_df, df)
          }
        }
      }
    }
  }

  if (identical(varieties, "all")) {
    message("ℹ️ Averaging all parameters for all varieties across all sites...")

    # 1. Get varieties with valid parameter files
    all_varieties_with_params <- unique(available_params$Variety)

    # 2. Clean and normalize target_data
    target_data <- janitor::clean_names(target_data)
    if (!"variety" %in% names(target_data)) {
      stop("❌ ERROR: `target_data` must contain a 'Variety' column (case-insensitive).")
    }

    # 3. Get varieties that exist in target data
    all_varieties_in_target <- unique(target_data$variety)

    # 4. Intersection: Only keep varieties present in both
    valid_varieties <- intersect(all_varieties_with_params, all_varieties_in_target)

    if (length(valid_varieties) == 0) {
      stop("❌ No varieties found with both parameter files and target data.")
    }

    for (v in valid_varieties) {
      matching_rows <- available_params[available_params$Variety == v, ]
      param_paths <- file.path(param_dir, matching_rows$File)

      if (length(param_paths) == 0) {
        warning(glue::glue("⚠️ No parameter files found for variety '{v}' — skipping."))
        next
      }

      # Load and rename 'value' columns uniquely
      param_list <- lapply(seq_along(param_paths), function(i) {
        file <- param_paths[i]
        df <- read.csv(file, stringsAsFactors = FALSE)
        if (!all(c("param", "value") %in% names(df))) {
          stop(paste0("❌ Invalid parameter file format: ", file))
        }
        colnames(df)[colnames(df) == "value"] <- paste0("value_", i)
        df
      })

      # Merge and average
      merged_params <- Reduce(function(x, y) merge(x, y, by = "param", all = TRUE), param_list)
      value_cols <- grep("^value_", names(merged_params), value = TRUE)
      merged_params$avg_value <- rowMeans(merged_params[, value_cols, drop = FALSE], na.rm = TRUE)

      for (s in target_sites) {
        df <- data.frame(
          Variety = v,
          Site = s,
          param = merged_params$param,
          value = merged_params$avg_value,
          stringsAsFactors = FALSE
        )

        output_filename <- paste0("parameters_", s, "_", v, ".csv")
        output_filepath <- file.path(output_dir, output_filename)

        write.csv(df[, c("param", "value")], output_filepath, row.names = FALSE, quote = FALSE)
        #message(glue::glue("📄 Written parameter file for {v} at {s}"))

        averaged_parameters_df <- rbind(averaged_parameters_df, df)
      }
    }

    rownames(averaged_parameters_df) <- NULL

    # ✅ Use this restricted set downstream
    varieties <- valid_varieties
  }



  target_data <- janitor::clean_names(target_data)

  # 🛡️ Normalize column names in target_data and weather_data
  site_col_target <- names(target_data)[tolower(names(target_data)) == "site"]
  site_col_weather <- names(weather_data)[tolower(names(weather_data)) == "site"]

  if (length(site_col_target) == 0) {
    stop("❌ ERROR: `target_data` must have a column named 'Site' (case-insensitive).")
  }
  if (length(site_col_weather) == 0) {
    stop("❌ ERROR: `weather_data` must have a column named 'Site' (case-insensitive).")
  }

  # Rename to standard 'Site' internally
  names(target_data)[names(target_data) == site_col_target] <- "Site"
  names(weather_data)[names(weather_data) == site_col_weather] <- "Site"

  # Validate that all sites exist in target_data
  missing_sites <- setdiff(sites, unique(target_data$Site))
  if (length(missing_sites) > 0) {
    stop(glue::glue(
      "❌ ERROR: The following site(s) are not found in `target_data`: {paste(missing_sites, collapse = ', ')}"
    ))
  }

  # Validate that all sites exist in weather_data
  missing_sites_weather <- setdiff(sites, unique(weather_data$Site))
  if (length(missing_sites_weather) > 0) {
    stop(glue::glue(
      "❌ ERROR: The following site(s) are not found in `weather_data`: {paste(missing_sites_weather, collapse = ', ')}"
    ))
  }
  # CONFIGURATION----
  # Path to the EXE
  exe_path <- system.file("bin", "phenomenals.exe", package = "phenomenals")
  if (!file.exists(exe_path)) stop("❌ Executable not found.")

  # Subfolders
  input_weather_dir <- file.path(dirname(exe_path), "inputFiles", "weather", tolower(timestep))
  input_plant_dir   <- file.path(dirname(exe_path), "inputFiles", "plant")
  input_ref_dir     <- file.path(dirname(exe_path), "inputFiles", "referenceData")

  dir.create(input_weather_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(input_ref_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(input_plant_dir, recursive = TRUE, showWarnings = FALSE)

  # Ensure unique combinations of Variety and Site
  # Extract unique combinations
  unique_combos <- unique(averaged_parameters_df[, c("Variety", "Site")])

  # Get unique coordinates
  unique_coordinates <- weather_data |>
    dplyr::group_by(Site) |>
    dplyr::slice_head(n = 1)

  # Check if all Sites in unique_combos are present in unique_coordinates
  missing_sites <- setdiff(unique_combos$Site, unique_coordinates$Site)

  if (length(missing_sites) > 0) {
    stop(paste("The following Sites are missing from weather_data:",
               paste(missing_sites, collapse = ", ")))
  }

  # Generate fake reference rows
  synthetic_ref <- unique_combos  |>
    dplyr::left_join(unique_coordinates |>
                       dplyr::select(Site, Latitude, Longitude), by = "Site") |>
    dplyr::mutate(
      Date = paste0("01/04/", start_year),
      BBCH = 65
    )

  # Path to write the reference file
  ref_file <- file.path(input_ref_dir, "referenceBBCH.csv")

  # Write the synthetic file
  write.table(
    synthetic_ref,
    file = ref_file,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE
  )

  # WRITE WEATHER FILE ----
  # Handle 'all' sites logic
  # Validate required columns
  if (!"Site" %in% names(weather_data)) stop("Missing 'Site' column in weather_data")
  if (!"Site" %in% names(target_data)) stop("Missing 'Site' column in target_data")

  # Determine the list of sites to use
  if (identical(sites, "all")) {
    sites <- intersect(unique(weather_data$Site), unique(target_data$Site))
    if (length(sites) == 0) stop("❌ No overlapping sites between weather_data and target_data")
  }

  site_names <- sites  # This will be used in the JSON config

  # WRITE PARAMETERS FILE----
  params_to_df <- function(nested_list) {
    rows <- lapply(names(nested_list), function(species) {
      lapply(names(nested_list[[species]]), function(class) {
        lapply(names(nested_list[[species]][[class]]), function(par) {
          row <- nested_list[[species]][[class]][[par]]
          data.frame(
            species = species,
            class = class,
            parameter = par,
            min = row$min,
            max = row$max,
            value = row$value,
            calibration = ifelse(row$calibration, "x", ""),
            stringsAsFactors = FALSE
          )
        })
      })
    })
    flat_list <- do.call(c, do.call(c, rows))
    do.call(rbind, flat_list)
  }

  params_df <- params_to_df(phenomenalsParameters)
  param_file <- file.path(input_plant_dir, "phenologyParameters.csv")
  write.table(params_df, file = param_file, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)

  # --- WEATHER FILE WRITING ---
  for (s in sites) {
    site_weather <- weather_data[weather_data$Site == s, ]

    # Handle date/time column flexibly (Date or DateTime)
    date_col <- names(site_weather)[tolower(names(site_weather)) %in% c("datetime", "date")]
    if (length(date_col) == 0) stop("❌ 'weather_data' must have a 'DateTime' or 'Date' column.")

    names(site_weather)[names(site_weather) == date_col] <- "DateTime"

    # Parse using lubridate with fallback strategies
    parse_attempts <- list(
      lubridate::ymd_hms(site_weather$DateTime, quiet = TRUE),
      lubridate::ymd(site_weather$DateTime, quiet = TRUE),
      lubridate::dmy(site_weather$DateTime, quiet = TRUE),
      lubridate::mdy(site_weather$DateTime, quiet = TRUE)
    )

    parsed <- NULL
    for (attempt in parse_attempts) {
      if (all(!is.na(attempt))) {
        parsed <- attempt
        break
      }
    }

    if (is.null(parsed)) {
      stop("❌ Could not parse 'DateTime' values in weather_data. Please check date format.")
    }

    site_weather$DateTime <- parsed

    # Write to CSV file
    out_file <- file.path(input_weather_dir, paste0(s, ".csv"))
    write.table(
      site_weather,
      file = out_file,
      sep = ",",
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE
    )

    # message(glue::glue("🌦️  Written weather file for site '{s}' ➜ {basename(out_file)}"))
  }


  # BUILD JSON ----
  config <- list(
    settings = list(
      startYear = start_year,
      endYear = end_year,
      sites = as.list(site_names),
      isCalibration = "false",#tolower(as.character(is_calibration)),
      varieties = as.list(varieties),
      simplexes = 1,
      iterations = 1,
      weatherTimeStep = tolower(timestep)
    ),
    paths = list(
      weatherDir = normalizePath(dirname(input_weather_dir), winslash = "\\", mustWork = FALSE),
      referenceFileBBCH = normalizePath(ref_file, winslash = "\\", mustWork = FALSE),
      plantParamFile = normalizePath(param_file, winslash = "\\", mustWork = FALSE)
    )
  )

  config_path <- file.path(dirname(exe_path), "phenomenalsConfig.json")
  jsonlite::write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE)

  cmd <- paste(shQuote(exe_path), shQuote(config_path))

  # ==== Run the model ====
  # Run the command and capture success/failure
  result <- tryCatch({
    # Run the C# executable and stream output directly to the console
    system(cmd, intern = FALSE)

    # Return success
    list(success = TRUE)
  }, error = function(e) {
    # Return failure and error message
    list(success = FALSE, error = e$message)
  })

  # Check result
  if (!result$success) {
    stop("❌ Error while running executable: ", result$error)
  }


  #OUTPUTS----
  list_files_out <- list.files(paste0(dirname(exe_path),"\\outputsValidationDaily"),
                               pattern = "\\.csv$", full.names = TRUE)

  # Read and bind all files with filename as a column
  df_sim <- do.call(rbind, lapply(list_files_out, function(file) {
    df <- data.table::fread(file)
    # Extract region name from filename
    file_name <- basename(file)
    region <- sub(".*_(.*?)\\.csv", "\\1", file_name)
    df$region <- region
    return(df)
  })) |>
    dplyr::select(-c(region,AntiChillState:ForcingState,BBCHRef))

  #PREPARE THE DATASET----
  df <- df_sim |>
    dplyr::mutate(
      DiseaseF = ifelse(BBCHCode == 0, 0, DiseaseF),
      Date = as.Date(lubridate::mdy(Date)),
      Completion = DormancyCompletion + CycleCompletion,
      DOY = lubridate::yday(Date),
      hemisphere = ifelse(latitude < 0, "southern", "northern"),

      year_meteo = dplyr::case_when(
        hemisphere == "southern" ~ lubridate::year(Date) - 1,  # full season is Sep–Mar
        hemisphere == "northern" ~ lubridate::year(Date),      # full season is Jan–Aug
        TRUE ~ NA_integer_
      )
    )


  phenomenalsCandidates <- c("TempF",'HeatF','ColdF','LightF','AridityF',"VPDefF","WindF","DiseaseF")

  # Loop over candidates and apply smoothing
  df_smoothed <- df  |>  dplyr::arrange(variety, Date)

  for (signal in phenomenalsCandidates) {

    cat(glue::glue("\r🔄 Smoothing signal: {signal}"), append = FALSE)

    if (!signal %in% colnames(df_smoothed)) {
      warning(glue::glue("⚠️  Skipping '{signal}' — not found in dataset."))
      next
    }

    if (!is.numeric(df_smoothed[[signal]])) {
      warning(glue::glue("⚠️  Skipping '{signal}' — not numeric."))
      next
    }

    # Apply rolling mean
    df_smoothed <- df_smoothed |>
      dplyr::group_by(variety) |>
      dplyr::arrange(Date) |>
      dplyr::mutate("{signal}" := zoo::rollapply(.data[[signal]],
                                                 width = rolling_window,
                                                 FUN = mean,
                                                 align = "right",
                                                 fill = NA,
                                                 na.rm = TRUE)) |>
      dplyr::ungroup()
  }


  cat("✅  done\n")
  # Final column selection
  df_smoothed <- df_smoothed |>
    dplyr::select(site, hemisphere, variety, Date, DOY, year_meteo, dplyr::all_of(phenomenalsCandidates),
                  ChillState, DormancyCompletion, CycleCompletion, Completion, BBCHPhase)

  #OUT OBJECT PHENOMENALS RAW!----
  phenomenals_raw <- df_smoothed |>
    dplyr::rename(SeasonCompletion = CycleCompletion,
                  CycleCompletion = Completion) |>
    dplyr::select(site,variety,Date,DormancyCompletion,SeasonCompletion,
                  CycleCompletion,TempF:DiseaseF)

  # 1b. PREPARE TARGET DATA ----
  yieldData_raw <- target_data |>
    janitor::clean_names() |>
    dplyr::filter(site %in% site_names)|>
    dplyr::mutate(hemisphere=ifelse(latitude<0,'southern','northern'))

  # Check for missing target variables
  missing_vars <- setdiff(target_traits, unique(yieldData_raw$variable))

  if (length(missing_vars) > 0) {
    stop(glue::glue(
      "❌ ERROR: The following target variables are missing for site '{sites}': {paste(missing_vars, collapse = ', ')}\n",
      "Please check the input data and make sure all target variables are available."
    ))
  }

  # Proceed with valid data
  # If 'all', determine valid varieties from the data
  if (identical(varieties, "all")) {
    varieties <- unique(yieldData_raw$variety[
      yieldData_raw$variable %in% target_traits &
        yieldData_raw$site %in% sites
    ])
  }

  # Filter and process
  yieldData <- yieldData_raw |>
    dplyr::filter(variable %in% target_traits,
                  site %in% sites,
                  variety %in% varieties) |>
    dplyr::select(site, hemisphere, variety, variable, year, value) |>
    dplyr::group_by(site, variety, variable) |>
    dplyr::arrange(year) |>
    dplyr::filter(!all(is.na(value))) |>
    dplyr::mutate(
      min_val = min(value, na.rm = TRUE),
      max_val = max(value, na.rm = TRUE),
      target_0 = 2 * (value - min_val) / (max_val - min_val) - 1
    ) |>
    dplyr::ungroup()


  # yield table extended
  harvest_map_extended <- yieldData |>
    dplyr::mutate(
      harvest_date = dplyr::case_when(
        hemisphere == "southern" ~ as.Date(paste0(year, "-03-31")),
        hemisphere == "northern" ~ as.Date(paste0(year, "-08-31")),
        TRUE ~ as.Date(NA)
      ),
      # Meteorological season start year for year_0 and year_1
      meteo_start_0 = dplyr::case_when(
        hemisphere == "southern" ~ year - 1,  # Mar Y-1 to Mar Y
        hemisphere == "northern" ~ year       # Jan Y to Aug Y
      ),
      meteo_start_1 = meteo_start_0 - 1       # One full season earlier
    ) |>
    dplyr::select(site, variety, variable, hemisphere,
                  harvest_year = year, harvest_date, target_0,
                  meteo_start_0, meteo_start_1) |>
    tidyr::pivot_longer(cols = starts_with("meteo_start"),
                        names_to = "relative_year",
                        values_to = "year_meteo") |>
    dplyr::mutate(
      relative_year = dplyr::case_when(
        relative_year == "meteo_start_0" ~ "year_0",
        relative_year == "meteo_start_1" ~ "year_1"
      )
    )


  filter_by_completion_ranges_dt <- function(df, ranges) {
    dt <- data.table::as.data.table(df)

    # Create a logical condition for all ranges
    keep_rows <- Reduce(`|`, lapply(ranges, function(rng) {
      dt[, cyclePerc >= rng[1] & cyclePerc <= rng[2]]
    }))

    # Return filtered rows as tibble
    dplyr::as_tibble(dt[keep_rows])
  }

  ## 1c. Join weather and yield data ----
  df_signal <- df_smoothed |>
    dplyr::left_join(harvest_map_extended,
                     by = c("site", "hemisphere","variety", "year_meteo"),
                     relationship='many-to-many')  |>
    dplyr::filter(!is.na(harvest_year)) |>
    dplyr::left_join(
      yieldData  |>
        dplyr::select(site, variety, variable, year, target_0),
      by = c("site", "variety","variable", "harvest_year" = "year", 'target_0'='target_0')
    ) |>
    dplyr::mutate(
      phase = dplyr::case_when(
        DormancyCompletion > 0 & CycleCompletion == 0 ~ "dormancy",
        CycleCompletion > 0 ~ "cycle",
        TRUE ~ NA_character_)
    ) |>
    dplyr::filter(!is.na(phase), !is.na(Completion), !is.na(target_0))  |>
    dplyr::mutate(Completion = as.integer(Completion))

  #COMPUTING CORRELATIONS----
  df_long <- df_signal |>
    tidyr::pivot_longer(cols = all_of(phenomenalsCandidates),
                        names_to = "phenomenals",
                        values_to = "value")  |>
    dplyr::filter(!is.na(value)) |>
    dplyr::filter(relative_year %in% c("year_1", "year_0")) |>
    dplyr::mutate(
      bin = floor(Completion / bin_size) * bin_size,
      block = dplyr::case_when(
        relative_year == "year_1" ~ 0,
        relative_year == "year_0" ~ 2
      ),
      cyclePerc = bin + (block - 1) * 100 + 100
    )




  phenomenals<-phenomenalsCandidates
  # compute correlation function
  compute_correlations <- function(df_signal, phenomenals, target_col, bin_size = 1) {
    df_long <- df_signal |>
      tidyr::pivot_longer(cols = all_of(phenomenals),
                          names_to = "phenomenals", values_to = "value")  |>
      dplyr::filter(!is.na(value), !is.na(.data[[target_col]]))  |>
      dplyr::filter(relative_year %in% c("year_1", "year_0")) |>
      dplyr::mutate(
        bin = floor(Completion / bin_size) * bin_size  # binning completion
      )

    df_long  |>
      dplyr::group_by(site, variety, relative_year, phenomenals, bin)  |>
      dplyr::summarise(
        cor = {
          valid_cases <- sum(complete.cases(value, .data[[target_col]]))
          if (valid_cases >= 2) {
            cor(value, .data[[target_col]], use = "complete.obs", method = "pearson")
          } else {NA_real_}
        },
        p = {valid_cases <- sum(complete.cases(value, .data[[target_col]]))
        if (valid_cases >= 5) {
          tryCatch(cor.test(value, .data[[target_col]],
                            method = "pearson")$p.value, error = function(e) NA_real_)
        } else { NA_real_}
        },
        BBCHPhase = median(BBCHPhase, na.rm = TRUE),
        DormancyCompletion = median(DormancyCompletion, na.rm = TRUE),
        Completion = median(bin, na.rm = TRUE),
        ChillState = median(ChillState, na.rm = TRUE),
        .groups = "drop"
      )  |>
      dplyr::mutate(
        sign = ifelse(cor >= 0, "Positive", "Negative"),
        alpha = ifelse(p < 0.1, 1, 0.3),
        block = dplyr::case_when(
          relative_year == "year_1" ~ 0,
          relative_year == "year_0" ~ 2
        ),
        cyclePerc = Completion + (block - 1) * 100 + 100,
        cor = ifelse(is.na(cor), 0, cor),
        p = ifelse(is.na(p), 0, p),
        target = target_col
      )  |>
      filter_by_completion_ranges_dt(evaluation_range)
  }

  # --- Compute correlations for all targets ---
  correlation_results <- list()
  correlation_results <- purrr::map(target_traits, function(target) {
    cat(glue::glue("\r📊 Computing correlations ➤ {target} {strrep(' ', 5)}"), append = FALSE); flush.console()

    suppressWarnings({
      df_filtered <- df_signal |>
        dplyr::filter(variable == !!target)
      compute_correlations(
        df_filtered,
        phenomenalsCandidates,
        target_col = "target_0",
        bin_size = bin_size
      )
    })
  }) |>  purrr::set_names(target_traits)

  all_correlations <- dplyr::bind_rows(correlation_results, .id = "target_trait")

  #OUT OBJECT correlation_df!----
  correlations <- all_correlations |>
    dplyr::select(site, variety, target_trait, phenomenals, relative_year, Completion, cyclePerc, ChillState,BBCHPhase, cor, p, sign) |>
    dplyr::rename(ecoFunction = phenomenals) |>
    dplyr::arrange(ecoFunction,cyclePerc)

  cat("✅ done\n")

  # COMPUTE CLIMATOLOGY ----

  cat(glue::glue("\r📊 Computing climatology {strrep(' ', 10)}")); flush.console()
  # Aggiungi colonna bin se non già presente
  df_long <- df_long  |>
    dplyr::mutate(bin = floor(Completion / bin_size) * bin_size)

  # Climatologia per bin
  climatology <- df_long  |>
    dplyr::group_by(site, variety, bin, phenomenals) |>
    dplyr::summarise(
      min_val = quantile(value, probs=0.05, na.rm = TRUE),
      mean = median(value, na.rm = TRUE),
      max_val = quantile(value, probs=0.95, na.rm = TRUE),
      .groups = "drop"
    )

  scaled_sigmoid <- function(value, mean, min_val, max_val, scale = 10) {
    x <- (value - mean) / ((max_val - min_val) / 2)
    1 / (1 + exp(-scale * x))
  }
  cat(glue::glue("\r✅ Starting phenomenals computation {strrep(' ', 20)}\n"))

  # Normalizzazione sigmoidale per ogni bin
  df_signalized <- df_long |>
    dplyr::left_join(climatology, by = c("phenomenals", "bin", "site", "variety")) |>
    dplyr::mutate(
      signal = scaled_sigmoid(value, mean, min_val, max_val),
      signal = ifelse(is.na(signal), mean, signal)
    )

  ## 3b. Weighted signal----
  compute_weighted_signals <- function(df_signalized, cor_completion, target_col) {
    cor_mod <- cor_completion  |>
      dplyr::filter(target_trait == target_col) |>
      dplyr::mutate(
        cor = ifelse(phenomenals %in% c("chillR", "ecoCue", "endoCue") & BBCHPhase > 0, 0, cor),
        p = ifelse(phenomenals %in% c("chillR", "ecoCue", "endoCue") & BBCHPhase > 0, 0, p)
      )

    df_weighted <- df_signalized |>
      dplyr::left_join(cor_mod |>
                         dplyr::select(-BBCHPhase, -DormancyCompletion, -Completion),
                       by = c("bin", "site", "variety", "phenomenals", "relative_year"))  |>
      dplyr::mutate(
        weighted_signal = signal * cor* (1-p),
        weighted_signal_pot = cor,
        target = target_col
      )

    return(df_weighted)
  }

  weighted_signals_all <- purrr::map_dfr(target_traits, function(target) {
    compute_weighted_signals(df_signalized, all_correlations, target)
  })

  ## 3c. Cumulative signal----
  df_cumulative_signal_all <- weighted_signals_all  |>
    #filter_by_completion_ranges_dt(evaluation_range) |>
    dplyr::mutate(
      # Flip year blocks and scale consistently across hemispheres
      cyclePerc = dplyr::case_when(
        hemisphere == "southern" & relative_year == "year_1" ~ bin,               # dormancy
        hemisphere == "southern" & relative_year == "year_0" ~ bin + 200,         # growing season
        hemisphere == "northern" & relative_year == "year_1" ~ bin,               # dormancy
        hemisphere == "northern" & relative_year == "year_0" ~ bin + 200          # growing season
      )
    ) |>
    dplyr::arrange(site, variety, target_trait, harvest_year, phenomenals, cyclePerc) |>
    dplyr::group_by(site, variety, harvest_year, phenomenals, target_trait) |>
    dplyr::mutate(
      signal_cum = cumsum(weighted_signal),
      signal_cum_pot = cumsum(weighted_signal_pot)
    ) |>
    dplyr::ungroup()

  cat(glue::glue("\r✅ Phenomenals signals estimated{strrep(' ', 20)}\n"))

  ## 3d. Aggregate by x position----
  df_signal_summary_all_fixed <- df_cumulative_signal_all  |>
    dplyr::group_by(harvest_year, site, variety, cyclePerc, phenomenals, target) |>
    dplyr::summarise(
      signalSumMean = mean(signal_cum, na.rm = TRUE),
      signalSumMeanPot=mean(signal_cum_pot,na.rm=T),
      .groups = "drop"
    )

  # Pivot to wide format so each variable becomes a column
  signal_index_wide_all <- df_signal_summary_all_fixed |>
    dplyr::select(-signalSumMeanPot) |>
    tidyr::pivot_wider(names_from = phenomenals, values_from = signalSumMean)
  cat(glue::glue("\r✅ Phenomenals computed{strrep(' ', 20)}\n"))

  ## 3g. Final dataset ----
  signal_index_wide_all_with_targets <- signal_index_wide_all  |>
    dplyr::left_join(yieldData, by = c("site", "variety", "target" = 'variable',
                                       "harvest_year" = "year"),
                     relationship = "many-to-many") |>
    dplyr::filter(!is.na(target_0))  # Only complete cases

  #OUT OBJECT correlation_df!----
  phenomenals_processed <- signal_index_wide_all_with_targets |>
    dplyr::rename(cyclePercentage=cyclePerc,
                  value_normalized = target_0,
                  year=harvest_year) |>
    dplyr::select(site,variety,year,cyclePercentage,target,value,value_normalized,AridityF:WindF)


  #CORRELATION MATRIX ON CANDIDATE PHENOMENALS----
  # Step 1: Create correlation matrices and tag them with site and cyclePerc
  cor_matrices_by_site <- signal_index_wide_all_with_targets |>
    dplyr::group_by(site) |>
    dplyr::group_map(~ {
      df <- .x |>
        dplyr::select(all_of(phenomenalsCandidates)) |>
        na.omit()

      # Remove constant (zero variance) columns
      df <- df[, sapply(df, function(col) stats::sd(col, na.rm = TRUE) > 0), drop = FALSE]

      # Get correlation matrix if sufficient data and columns remain
      mat <- if (nrow(df) > 5 && ncol(df) > 1) {
        cor(df)
      } else {
        matrix(NA, length(phenomenalsCandidates), length(phenomenalsCandidates),
               dimnames = list(phenomenalsCandidates, phenomenalsCandidates))
      }

      list(
        site = .y$site,
        matrix = mat
      )
    })


  # Extract site names
  site_target_keys <- purrr::map_chr(cor_matrices_by_site, ~ .x$site)

  # Split the list into groups of (site + target)
  cor_matrices_grouped <- split(cor_matrices_by_site, site_target_keys)

  # Average correlation matrices per group
  cor_site_means <- purrr::map(cor_matrices_grouped, function(site_target_list) {
    matrices <- purrr::map(site_target_list, "matrix")
    mat_array <- simplify2array(matrices)
    apply(mat_array, 1:2, mean, na.rm = TRUE)
  })

  #OUT OBJECT autocorrelations!----
  multicollinearity_matrices <- cor_site_means

  # Convert to tidy data frame with separate site and target columns
  cor_df_all_sites <- purrr::map_dfr(names(cor_site_means), function(key) {
    mat <- cor_site_means[[key]]

    df <- as.data.frame(as.table(mat)) |>
      dplyr::rename(var1 = Var1, var2 = Var2, correlation = Freq) |>
      dplyr::filter(as.numeric(factor(var1)) > as.numeric(factor(var2)))

    # Split key into site and target
    parts <- strsplit(key, "\\|")[[1]]
    df$site <- parts[1]
    df
  })

  final_predictors_by_site_trait <- list()

  for (site_trait in names(cor_site_means)) {
    mat <- cor_site_means[[site_trait]]

    high_cor_pairs <- which(abs(mat) > multicollinearity_threshold & abs(mat) < 1, arr.ind = TRUE)

    if (length(high_cor_pairs) > 0) {
      high_cor_df <- dplyr::tibble(
        var1 = rownames(mat)[high_cor_pairs[, 1]],
        var2 = colnames(mat)[high_cor_pairs[, 2]],
        corr = mat[high_cor_pairs]
      ) |>
        dplyr::filter(var1 < var2)

      drop_vars <- c()

      for (i in seq_len(nrow(high_cor_df))) {
        pair <- high_cor_df[i, ]
        if (!(pair$var1 %in% drop_vars) && !(pair$var2 %in% drop_vars)) {
          cor1 <- mean(abs(mat[pair$var1, ]), na.rm = TRUE)
          cor2 <- mean(abs(mat[pair$var2, ]), na.rm = TRUE)
          to_drop <- ifelse(cor1 > cor2, pair$var1, pair$var2)
          drop_vars <- union(drop_vars, to_drop)
        }
      }

      final_predictors <- setdiff(phenomenalsCandidates, drop_vars)
    } else {
      final_predictors <- phenomenalsCandidates
    }

    final_predictors_by_site_trait[[site_trait]] <- final_predictors
  }
  cat(glue::glue("\r✅  Multicollinearity computed{strrep(' ', 20)}\n"))


  #stepwise phenomenals selection
  all_predictors <- final_predictors_by_site_trait
  #OUT OBJECT phenomenals_multicollinearity!----
  multicollinearity_step <- final_predictors_by_site_trait

  x_pos_list <- signal_index_wide_all_with_targets |>
    dplyr::filter(
      purrr::map_lgl(cyclePerc, function(x) {
        any(purrr::map_lgl(evaluation_range, function(rng) x >= rng[1] & x <= rng[2]))
      })
    ) |>
    dplyr::distinct(site, variety, target, cyclePerc) |>
    dplyr::arrange(site, variety, target, cyclePerc)

  stepwise_selected <- list()

  for (i in seq_len(nrow(x_pos_list))) {

    target_val  <- x_pos_list$target[i]
    site_val    <- x_pos_list$site[i]
    variety_val <- x_pos_list$variety[i]
    x_pos_val   <- x_pos_list$cyclePerc[i]

    cat(glue::glue(
      "\r🔄 Stepwise selection for: {site_val} | {variety_val} | {target_val} | Cycle% = {x_pos_val} {strrep(' ', 20)}"
    ))
    flush.console()

    df_x <- signal_index_wide_all_with_targets |>
      dplyr::filter(site == site_val, variety == variety_val,
                    target == target_val, cyclePerc == x_pos_val) |>
      dplyr::select(target_0, all_of(all_predictors[[site_val]])) |>
      na.omit()

    if (nrow(df_x) < 2) next

    full_model <- tryCatch(
      lm(target_0 ~ ., data = df_x),
      error = function(e) return(NULL)
    )

    # Skip if model creation failed
    if (is.null(full_model)) next

    # Skip if AIC is -Inf
    aic_val <- tryCatch(AIC(full_model), error = function(e) Inf)
    if (!is.finite(aic_val) || is.infinite(aic_val) || is.nan(aic_val)) {
      next
    }

    # Try stepwise safely
    step_model <- tryCatch(
      MASS::stepAIC(full_model, direction = "backward", trace = FALSE),
      error = function(e) NULL
    )

    if (is.null(step_model)) next

    stepwise_selected[[length(stepwise_selected) + 1]] <- dplyr::tibble(
      target = target_val,
      site = site_val,
      variety = variety_val,
      cyclePerc = x_pos_val,
      predictors = list(names(coef(step_model))[-1])  # exclude intercept
    )
  }

  cat(glue::glue("\r✅ Stepwise selection completed{strrep(' ', 20)}\n"))

  stepwise_selected <- dplyr::bind_rows(stepwise_selected)

  # Step 1: Compute frequencies per site + variety
  predictor_freq_site_var <- stepwise_selected |>
    tidyr::unnest(predictors) |>
    dplyr::count(site, variety, target, predictors, name = "n_selected") |>
    dplyr::group_by(site, variety, target) |>
    dplyr::mutate(freq = n_selected / max(n_selected)) |>
    dplyr::ungroup()

  # Step 1: Define fixed order of predictors
  top_predictors_by_site <- predictor_freq_site_var |>
    dplyr::group_by(site, variety, target) |>
    dplyr::slice_max(n_selected, n = max_phenomenals, with_ties = FALSE) |>
    dplyr::summarise(phenomenals = list(unique(predictors)), .groups = "drop")

  top_predictors_list <- setNames(
    top_predictors_by_site$phenomenals,
    paste(top_predictors_by_site$site, top_predictors_by_site$variety,
          top_predictors_by_site$target, sep = "|")
  )
  #OUT OBJECT phenomenals_stepwise!----
  phenomenals_stepwise <- top_predictors_list

  filtered_correlations_top_n <- all_correlations |>
    dplyr::filter(purrr::pmap_lgl(list(site, target_trait, variety, phenomenals),
                                  function(s, t, v, pheno) {
                                    key <- paste(s, v, t, sep = "|")
                                    t %in% target_traits &&                     # <- check if target is in your list
                                      key %in% names(top_predictors_list) &&
                                      pheno %in% top_predictors_list[[key]]
                                  }))

  cat("\n✅ Top phenomenals selected \n")

  signal_index_wide_all_with_targets <- signal_index_wide_all_with_targets |>
    dplyr::filter(!is.na(site) & !is.na(variety) & !is.na(target) & !is.na(cyclePerc)) |>
    dplyr::filter(site != "" & variety != "" & target != "")

  valid_keys <- names(top_predictors_list)

  signal_index_wide_all_with_targets <- signal_index_wide_all_with_targets |>
    dplyr::mutate(key = paste(site, variety, target, sep = "|")) |>
    dplyr::filter(key %in% valid_keys)

  model_results_variety_loocv <- signal_index_wide_all_with_targets |>
    dplyr::filter(
      purrr::map_lgl(cyclePerc, function(x) {
        any(purrr::map_lgl(evaluation_range, function(rng) x >= rng[1] & x <= rng[2]))
      })
    ) |>  # <- GLOBAL filter for evaluation_range
    dplyr::group_by(site, variety, target) |>
    dplyr::group_split() |>
    purrr::map(function(df_group) {

      required_cols <- c("site", "variety", "target", "cyclePerc", "target_0")
      if (!all(required_cols %in% names(df_group))) {
        warning("⚠️ Skipping group: missing required columns.")
        return(NULL)
      }

      this_variety <- unique(df_group$variety)
      this_target  <- unique(df_group$target)
      this_site    <- unique(df_group$site)

      valid_cyclePercs <- df_group |>
        dplyr::group_by(cyclePerc) |>
        dplyr::summarise(n_years = dplyr::n_distinct(harvest_year), .groups = "drop") |>
        dplyr::filter(n_years >= 3) |>
        dplyr::pull(cyclePerc)

      if (length(valid_cyclePercs) == 0) {
        cat(glue::glue("\n⚠️  Not enough data: Skipping {this_variety} | target = {this_target}\n"))
        return(NULL)
      }

      results_per_x <- df_group |>
        dplyr::filter(cyclePerc %in% valid_cyclePercs) |>
        dplyr::group_by(cyclePerc) |>
        dplyr::group_split() |>
        purrr::map(function(df_variety) {

          this_x <- unique(df_variety$cyclePerc)
          if (nrow(df_variety) < 3) return(NULL)

          tryCatch({
            key <- paste(this_site, this_variety, this_target, sep = "|")
            predictors <- top_predictors_list[[key]]
            if (is.null(predictors) || length(predictors) == 0) return(NULL)

            model_formula <- as.formula(paste("target_0 ~", paste(predictors, collapse = " + ")))
            df_clean <- df_variety

            # LOOCV training
            ctrl <- caret::trainControl(method = "LOOCV", savePredictions = "final")
            model_cv <- suppressWarnings(
              caret::train(model_formula, data = df_clean, method = "lm", trControl = ctrl)
            )
            # Full model (fit on all data)
            model_full <- suppressWarnings(lm(model_formula, data = df_clean))
            df_clean$pred_full <- suppressWarnings(predict(model_full, newdata = df_clean))

            loocv_pred <- model_cv$pred$pred
            loocv_obs  <- model_cv$pred$obs

            # Metrics for LOOCV
            r2_loocv <- cor(loocv_obs, loocv_pred, use = "complete.obs")^2
            rmse_loocv <- sqrt(mean((loocv_pred - loocv_obs)^2, na.rm = TRUE))
            mae_loocv <- mean(abs(loocv_pred - loocv_obs), na.rm = TRUE)
            range_val <- max(loocv_obs, na.rm = TRUE) - min(loocv_obs, na.rm = TRUE)
            nMBE_loocv <- if (range_val == 0) NA_real_ else mean(loocv_pred - loocv_obs, na.rm = TRUE) / range_val


            df_clean$pred_full <- suppressWarnings(predict(model_full, newdata = df_clean))

            # Metrics for full model
            r2_full <- cor(df_clean$target_0, df_clean$pred_full, use = "complete.obs")^2
            rmse_full <- sqrt(mean((df_clean$pred_full - df_clean$target_0)^2, na.rm = TRUE))
            mae_full <- mean(abs(df_clean$pred_full - df_clean$target_0), na.rm = TRUE)
            range_val_full <- max(df_clean$target_0, na.rm = TRUE) - min(df_clean$target_0, na.rm = TRUE)
            nMBE_full <- if (range_val_full == 0) NA_real_ else mean(df_clean$pred_full - df_clean$target_0, na.rm = TRUE) / range_val_full

            # Relative importance
            relimpo_result <- tryCatch({
              rel <- relaimpo::calc.relimp(model_full, type = "lmg", rela = TRUE)
              list(
                df = as.data.frame(rel@lmg) |>
                  tibble::rownames_to_column("predictor") |>
                  dplyr::mutate(
                    variety = this_variety,
                    site = this_site,
                    cyclePerc = this_x,
                    target = this_target,
                    relaimpo_note = "OK"
                  ),
                failed = FALSE
              )
            }, error = function(e) {
              list(
                df = tibble::tibble(
                  predictor = predictors,
                  lmg = 0,
                  variety = this_variety,
                  site = this_site,
                  cyclePerc = this_x,
                  target = this_target,
                  relaimpo_note = "relimpo failed"
                ),
                failed = TRUE
              )
            })

            relimpo_df <- relimpo_result$df
            relimpo_failed <- relimpo_result$failed

            msg <- glue::glue(
              "🔄 {this_variety} | {this_site} | x = {this_x} | {this_target} | R²_loocv = {round(r2_loocv, 2)} | R²_full = {round(r2_full, 2)}"
            )
            if (relimpo_failed) msg <- paste0(msg, " ⚠️ (relimpo skipped)")
            cat("\r", msg, strrep(" ", 20), sep = "")
            flush.console()

            coef_df <- broom::tidy(model_full) |>
              dplyr::mutate(
                variety = this_variety,
                site = this_site,
                cyclePerc = this_x,
                target = this_target
              )

            diagnostics <- tibble::tibble(
              site = this_site,
              variety = this_variety,
              target = this_target,
              cyclePerc = this_x,
              R2_loocv = r2_loocv,
              RMSE_loocv = rmse_loocv,
              MAE_loocv = mae_loocv,
              nMBE_loocv = nMBE_loocv,
              R2_full = r2_full,
              RMSE_full = rmse_full,
              MAE_full = mae_full,
              nMBE_full = nMBE_full
            )

            list(
              predictions = df_clean,
              coefficients = coef_df,
              relaimpo = relimpo_df,
              diagnostics = diagnostics
            )

          }, error = function(e) {
            message(glue::glue("❌ Model error for {this_variety} | {this_site} | x = {this_x}: {e$message}"))
            return(NULL)
          })
        }) |>
        purrr::compact()

      # Attach rolling stats if needed
      diagnostics_tbl <- purrr::map_dfr(results_per_x, "diagnostics")
      diagnostics_cv <- diagnostics_tbl |>
        dplyr::arrange(site, variety, target, cyclePerc) |>
        dplyr::group_by(site, variety, target) |>
        dplyr::ungroup()

      # Inject updated diagnostics
      results_per_x <- purrr::map(results_per_x, function(res) {
        res$diagnostics <- dplyr::filter(
          diagnostics_cv,
          site == unique(res$diagnostics$site),
          variety == unique(res$diagnostics$variety),
          target == unique(res$diagnostics$target),
          cyclePerc == unique(res$diagnostics$cyclePerc)
        )
        res
      })

      return(results_per_x)
    }) |>
    purrr::flatten() |>
    purrr::compact()

  all_predictions <- purrr::map_dfr(model_results_variety_loocv, "predictions")
  all_diagnostics <- purrr::map_dfr(model_results_variety_loocv, "diagnostics")
  all_relimpo <- purrr::map_dfr(model_results_variety_loocv, "relaimpo") |>
    dplyr::rename(relImpo = `rel@lmg`)
  all_coefficients <- purrr::map_dfr(model_results_variety_loocv, "coefficients") |>
    dplyr::select(site, variety, target, cyclePerc, term:p.value) |>
    dplyr::arrange()

  # ---- OUT OBJECTS ----
  # Re-transform normalized predictions and targets to original scale
  predictions <- all_predictions |>
    dplyr::left_join(
      yieldData |>
        dplyr::select(site, variety, variable, year, max_val, min_val),
      by = c("site", "variety", "target" = "variable", "harvest_year" = "year",
             'max_val'='max_val','min_val'='min_val')
    ) |>
    dplyr::mutate(
      pred_original = (pred_full + 1) / 2 * (max_val - min_val) + min_val,
      target_original = (target_0 + 1) / 2 * (max_val - min_val) + min_val
    ) |>
    dplyr::select(site,hemisphere,variety,target,harvest_year,cyclePerc,AridityF:WindF,
                  target_normalized = target_0,
                  prediction_normalized = pred_full,
                  target_original,
                  prediction_original=pred_original)

  relative_importance <- all_relimpo
  coefficients <- all_coefficients
  diagnostics <- all_diagnostics



  # Cleanup any temporary files created during averaging
  if (length(temporary_files_to_delete) > 0) {
    message("🧹 Cleaning up temporary parameter files...")
    file.remove(temporary_files_to_delete)
    message(paste0("🗑️ Deleted ", length(temporary_files_to_delete), " temporary parameter files."))
  }


  #objects returned by the function
  return(list(
    data = list(
      smoothed = phenomenals_raw,
      processed = phenomenals_processed,
      signalized = df_signalized),
    selection = list(
      correlations = correlations,
      multicollinearity_matrices = multicollinearity_matrices,
      multicollinearity_step = multicollinearity_step,
      stepwise_step = phenomenals_stepwise
    ),
    results = list(
      predictions = predictions,
      coefficients = coefficients,
      diagnostics = diagnostics,
      relative_importance = relative_importance
    )
  ))
}


