#' List Available Calibrated Varieties
#'
#' Lists the CSV files containing calibrated parameters included in the package
#' and any additional user-calibrated files in a given directory.
#'
#' @param user_dir Optional. Path to the folder containing user-calibrated parameter files.
#' @return A character vector of available calibrated variety filenames.
#' @export
listAvailableVarieties <- function(user_dir = NULL) {
  # Get pre-calibrated files from the package installation
  pkg_path <- system.file("bin\\calibratedVarieties", package = "phenomenals")
  pkg_files <- list.files(pkg_path, pattern = "^parameters_.*\\.csv$", full.names = FALSE)

  # Get user-calibrated files, if a directory is provided
  user_files <- character(0)
  if (!is.null(pkg_path) && dir.exists(pkg_path)) {
    user_files <- list.files(pkg_path, pattern = "^parameters_.*\\.csv$", full.names = FALSE)
  }

  # Combine filenames from package and user
  all_files <- unique(c(pkg_files, user_files))  # remove exact filename duplicates

  # Extract site and variety from filenames
  info <- strcapture(
    pattern = "^parameters_([^_]+)_(.+)\\.csv$",
    all_files,
    proto = list(site = character(), variety = character())
  )

  # Create a data frame
  df <- data.frame(
    Site = info$site,
    Variety = info$variety,
    stringsAsFactors = FALSE
  )

  # Remove duplicate site-variety pairs
  df <- df[!duplicated(df), ]

  # Sort alphabetically


  # Group and format
  table_df <- df |>
    dplyr::distinct(Variety, Site) |>
    dplyr::group_by(Variety) |>
    dplyr::summarise(Sites = paste(sort(Site), collapse = ", ")) |>
    dplyr::arrange(Variety)

  # Format and print as aligned console table
  # Determine column widths
  max_var_length <- max(nchar(table_df$Variety))
  max_site_length <- max(nchar(table_df$Sites))

  # Print header
  cat(sprintf("%-*s | %-*s\n", max_var_length, "Variety", max_site_length, "Sites"))
  cat(paste(rep("-", max_var_length + max_site_length + 3), collapse = ""), "\n")

  # Print each row

  # Print each row using a loop to avoid printing NULL
  for (i in seq_len(nrow(table_df))) {
    cat(sprintf("%-*s | %-*s\n",
                max_var_length,
                table_df$Variety[i],
                max_site_length,
                table_df$Sites[i]))
  }
}
