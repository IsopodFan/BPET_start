
## Install pak
install.packages("pak")

## List the rest of the packages
packages <- c(
  "fs",
  "giscoR",
  "mapview",
  "rsi",
  "rstac",
  "sf",
  "terra",
  "tictoc",
  "tidyterra",
  "tidyverse"
)

## Install the packages
pak::pak(packages)

pkgbuild::has_build_tools(debug = TRUE)

install.packages("pak", type = "source")
install.packages("pak", type = "binary")
install.packages("terra", type = "binary")
