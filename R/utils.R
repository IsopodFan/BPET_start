

## FUNCTION: download Tenerife municipalities 
get_tenerife_muni <- function(sel_crs = "EPSG:25828") {
    
    ## Get Spain municipalities
    spanish_muni_sf   <-   gisco_get_communes(
        country = "Spain"
    ) |> 
        #transform coordinates to meters
        st_transform(sel_crs)
    
    tenerife_sf <- gisco_get_nuts( 
        country    = "Spain", 
        resolution = "01",  
        #nuts = what level of specificity you want within country 
        #ex: 3 is the municipalities
        nuts_level = 3
    ) |>  
        filter( 
            NAME_LATN == "Tenerife"
        ) |> 
        st_transform(sel_crs)
    
    ## Filter municipalities intersecting Tenerife Island
    tenerife_muni_sf <- st_filter(
        x = spanish_muni_sf, 
        y = tenerife_sf
    )
}


## FUNCTION: download satellite image for each municipality 
get_sentinel2_muni <- function(data) {
    
    # select only the bands we'll use later (makes it run faster/smaller files)
    bands <- rsi::sentinel2_band_mapping$planetary_computer_v1[c("B04", "B08")]
    
    ## download Sentinel-2 image
    sentinel_path <- get_sentinel2_imagery(
        aoi             = data, 
        start_date      = "2024-05-04", 
        end_date        = "2024-05-05",  
        asset_names     = bands,
        output_filename = str_glue("data/sentinel/{data$id}.tif")
    )
    
    ## scale 
    rast(sentinel_path) / 10000
    
}

#FUNCTION: Calculate NDVI 
calc_ndvi <- function(image) {
    
    ## Calculate NDVI
    ## Formula: NDVI = (N - R) / (N + R)
    ndvi_sr <- (image$N - image$R) / (image$N + image$R)
    
    ## Rename band
    names(ndvi_sr) <- "NDVI"
    
    ## return NDVI
    ndvi_sr
    
}


























