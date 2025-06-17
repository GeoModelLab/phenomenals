copy_phenomenals_exe <- function(
    cs_proj_dir = "C:/Users/simoneugomaria.brega/OneDrive - CREA/Documenti - geoModelClan/agrarsense/papers/phenomenals/src_phenomenals",
    exe_name = "phenomenals.exe",
    exe_subdir = "bin/Debug/net8.0",
    r_package_dir = "."
) {
  source_path <- file.path(cs_proj_dir, exe_subdir, exe_name)
  dest_path <- file.path(r_package_dir, "inst", "bin", exe_name)

  if (!file.exists(source_path)) {
    stop("File .exe non trovato in: ", source_path, "\nHai compilato il progetto C#?")
  }

  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
  file.copy(source_path, dest_path, overwrite = TRUE)

  message("✅ Copiato '", exe_name, "' in: ", dest_path)
}

# Esegui subito se stai lanciando lo script
copy_phenomenals_exe()
