#' Build support required for Commercial & residential
#'
#' @return
#' @export
#'
#' @examples
data.build_comres_support <- function(){

  lu <- data.land_use(type="comres") %>%
    mutate(weight=1) %>%
    sf::st_make_valid()

  # Add the kabupaten map
  g <- data.bps_map() %>%
    sf::st_make_valid()

  intersection <- sf::st_intersection(lu, g)
  intersection$weight <- 1

  sf::write_sf(intersection, "data/comres/support.shp")

  return(intersection)
}


data.comres_support <- function(){
  sf::read_sf("data/comres/support.shp")
}


#' Read Commercial & Residential emission from excel
#'
#' @return
#' @export
#'
#' @examples
data.comres_emission <- function(){
  data.sheet_to_emissions(sheet_name="Residential and commercial ")
}
