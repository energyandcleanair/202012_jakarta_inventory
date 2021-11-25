#' Build support required for Commercial & residential
#'
#' @return support sf
comres.build_support <- function(){

  lu <- data.land_use(type="comres") %>%
    mutate(weight=1) %>%
    sf::st_make_valid()

  # Add the kabupaten map
  g <- data.bps_map() %>%
    sf::st_make_valid()

  intersection <- sf::st_intersection(lu, g)
  intersection$weight <- 1
  intersection <- intersection %>%
    filter(sf::st_geometry_type(geometry) %in% c("MULTIPOLYGON","POLYGON"))

  sf::write_sf(intersection, "sectors/comres/comres_support.gpkg")

  return(intersection)
}


#' Get support for Commercial & Residentisl
#'
#' @return support sf
comres.get_support <- function(){
  sf::read_sf("sectors/comres/comres_support.gpkg") %>% rename(geometry=geom)
}


#' Read Commercial & Residential emission from excel
#'
#' @return emission tibble
comres.get_emission <- function(){
  data.sheet_to_emissions(sheet_name="Residential and commercial ")
}
