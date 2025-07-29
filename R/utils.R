

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
