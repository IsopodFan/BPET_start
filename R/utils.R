
## FUNCTION: fix muni names
fix_muni_names <- function(name) {
    
    muni_split <- name |> 
        str_split(",") |> 
        pluck(1)
    
    paste(muni_split[2], muni_split[1]) |> 
        str_trim()
} #to get this function to apply to each element in turn, we need to vectorize it



## FUNCTION: vectorize the fixed muni names
fix_muni_names_vec <- Vectorize(fix_muni_names)



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
    
    #fix muni names 
    tenerife_muni_sf |> 
        mutate(
            change = if_else(
                str_detect(COMM_NAME, ","), TRUE, FALSE
            )
        ) |> 
        mutate(
            fixed_names = if_else(
                change, 
                fix_muni_names_vec(COMM_NAME),
                COMM_NAME
            )
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
calc_ndvi <- function(data) {
    
    ## Calculate NDVI
    ## Formula: NDVI = (N - R) / (N + R)
    ndvi_sr <- (data$N - data$R) / (data$N + data$R)
    
    ## Rename band
    names(ndvi_sr) <- "NDVI"
    
    ## return NDVI
    ndvi_sr
    
}



#FUNCTION: create map
create_ndvi_gg <- function(ndvi_sr, muni_sf) {
    
    ggplot() + 
        geom_spatraster( 
            data = ndvi_sr
        ) +  
        geom_sf(
            data  = muni_sf, 
            color = "darkblue", 
            fill  = "transparent", 
            lwd   = 1
        ) +
        scale_fill_gradientn( 
            colours = hcl.colors(20, "RdYlGn")
        ) + 
        labs( 
            title = str_glue("NDVI in {muni_sf$fixed_names}, Tenerife"),
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
    
}

























