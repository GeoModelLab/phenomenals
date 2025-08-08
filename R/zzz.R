.onAttach <- function(libname, pkgname) {
  green <- "\033[32m"  # Pheno
  blue  <- "\033[34m"  # Me
  red   <- "\033[31m"  # Nals
  reset <- "\033[0m"

  cat(
    paste0(
      green, "╔═╗┬ ┬┌─┐┌┐┌┌─┐",
      blue,  "╔╦╗┌─┐",
      red,   "╔╗╔┌─┐┬  ┌─┐\n",

      green, "╠═╝├─┤├┤ ││││ │",
      blue,  "║║║├┤ ",
      red,   "║║║├─┤│  └─┐\n",

      green, "╩  ┴ ┴└─┘┘└┘└─┘",
      blue,  "╩ ╩└─┘",
      red,   "╝╚╝┴ ┴┴─┘└─┘\n",
      reset
    )
  )
}
