#
# ------------ Mastering R: Best Practices and Essential Tools ------------ #
#
# This script:
# - Loads the data created in 01_get_data.R
# - Calculates the NDVI from the satellite image
# - Visualizes the NDVI using ggplot2
# - Saves the plot
# ------------------------------------------------------------------------ #

# 1. Load packages --------------------------------------------------------

library(sf)
library(terra)
library(tictoc)
library(tidyterra)
library(tidyverse)

# 2. Load data ------------------------------------------------------------

## Study area
selected_muni_sf <- read_sf("data/municipality.geojson")

## Satellite image
sentinel_sr <- rast("data/sentinel/ES6538026.tif")

# 3. Data analysis --------------------------------------------------------

## Calculate NDVI
## Formula: NDVI = (N - R) / (N + R)
ndvi_sr <- (sentinel_sr$N - sentinel_sr$R) / (sentinel_sr$N + sentinel_sr$R)

## Rename band
names(ndvi_sr) <- "NDVI"

# 4. Results --------------------------------------------------------------

## Visualize
ndvi_gg <- ggplot() + 
    geom_spatraster( 
        data = ndvi_sr
    ) +  
    geom_sf(
        data  = selected_muni_sf, 
        color = "darkblue", 
        fill  = "transparent", 
        lwd   = 1
    ) +
    scale_fill_gradientn( 
        colours = hcl.colors(20, "RdYlGn")
    ) + 
    labs( 
        title = str_glue("NDVI in La Orotava, Tenerife"),
        fill  = "NDVI"
    ) + 
    theme_void() + 
    theme( 
        plot.title = element_text( 
            face   = "bold", 
            size   = 14, 
            family = "Roboto", 
            hjust  = 0.5
            )
    )

## Save plot 
ggsave( 
   filename = str_glue("figures/NDVI_{selected_muni_sf$id}.png"), 
   plot     = ndvi_gg,
   width    = 8, 
   height   = 9
)

