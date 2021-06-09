#' Build support required for Commercial & residential
#'
#' @return
#' @export
#'
#' @examples
data.build_agroob_support <- function(){

  # Get land use with agriculture on it
  lu <- data.land_use(type="agroob") %>%
    mutate(weight=1) %>%
    sf::st_make_valid()

  # Add the kabupaten map
  g <- data.bps_map() %>%
    sf::st_make_valid()

  intersection <- sf::st_intersection(lu, g)

  # Get fires over that region
  library(creatrajs)

  creatrajs::fire.download(date_from="2019-01-01",
                           date_to="2019-12-31"
                           )


  sf::write_sf(intersection, "data/agroob/support.shp")

  return(intersection)
}


data.agroob_support <- function(){
  sf::read_sf("data/agroob/support.shp")
}
q

#' Read Commercial & Residential emission from excel
#'
#' @return
#' @export
#'
#' @examples
data.agroob_emission <- function(){
  data.sheet_to_emissions(sheet_name="Agro-residual-OB")
}
