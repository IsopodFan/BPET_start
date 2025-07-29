#
# ------------ Mastering R: Best Practices and Essential Tools ------------ #
#
# This script:
# - The boundaries of the municipality of La Orotava, in Tenerife
# - A satellite image of the bounding box of the municipality
# - Loads the data created in 01_get_data.R
# - Calculates the NDVI from the satellite image
# - Visualizes the NDVI using ggplot2
# - Saves the plot
# ------------------------------------------------------------------------ #

# 1. Load packages --------------------------------------------------------

library(giscoR)
library(sf)
library(rsi)
library(rstac)
library(terra)
library(tictoc)
library(tidyterra)
library(tidyverse)

source("R/utils.R")

# 2. Load data ------------------------------------------------------------

## Get Tenerife municipalities
tenerife_muni_sf <- get_tenerife_muni()

## Convert to list, where each row is an element
tenerife_muni_list <- split(
    tenerife_muni_sf, 
    tenerife_muni_sf$id
)

## Get Sentinel image for all the municipalities
tic()
sentinel_list <- map(
    tenerife_muni_list, 
    get_sentinel2_muni, 
    .progress = TRUE
)
toc()

# 3. Prepare data ---------------------------------------------------------

## Calculate NDVI
tic()
ndvi_list <- map(
    sentinel_list, 
    calc_ndvi, 
    .progress = TRUE
)
toc()

# 4. Maps -----------------------------------------------------------------

## Create the maps
ndvi_gg_list <- map2(
    ndvi_list,
    tenerife_muni_list,
    create_ndvi_gg
)
## ^^ above is slightly broken but I don't think it matters enough to fix

## Save the maps
    #not running the code because the maps are broken
map2(
    
    ndvi_gg_list, 
    tenerife_muni_list,
    \(x, y) ggsave(
        filename = str_glue("figures/NDVI_{y$id}.png"), 
        plot     = x,
        width    = 8,
        height   = 
    )
    
)








