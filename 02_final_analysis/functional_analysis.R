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
library(tidyverse)

source("R/utils.R")

# 2. Load data ------------------------------------------------------------

## Get Tenerife municipalities
tenerife_muni_sf <- get_tenerife_muni()

## Convert to list, where each row is an element

## Get Sentinel image for all the municipalities


# 3. Prepare data ---------------------------------------------------------

## Calculate NDVI


# 4. Maps -----------------------------------------------------------------

## Create the maps


## Save the maps

