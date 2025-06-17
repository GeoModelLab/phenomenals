# data-raw/build_parameters.R

df <- read.csv("data-raw/phenologyParameters.csv", stringsAsFactors = FALSE)

phenomenalsParameters <- split(df, df$species) |>
  lapply(function(df1) {
    split(df1, df1$class) |>
      lapply(function(df2) {
        setNames(lapply(seq_len(nrow(df2)), function(i) {
          list(
            min = df2$min[i],
            max = df2$max[i],
            value = df2$value[i],
            calibration = tolower(df2$calibration[i]) == "x"
          )
        }), df2$parameter)
      })
  })
usethis::use_data(phenomenalsParameters, overwrite = TRUE)
