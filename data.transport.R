
#' Build simplified road network, keeping only required levels
#'
#' @return
#' @export
#'
#' @examples
data.build_roads <- function(){

  osm.roads <- sf::read_sf("data/transport/osm/gis_osm_roads_free_1.shp")

  weights <- list(
    "motorway"=0.7,
    "trunk"=0.5,
    "primary"=0.4,
    "secondary"=0.25,
    "tertiary"=0.15)

  osm.roads.lite <- osm.roads %>%
    dplyr::filter(fclass %in% names(weights))

  # Add gadm 1 and 2 ids
  g <- data.gadm()

  # Attaching gadm id to roads
  roads <- osm.roads.lite %>%
    sf::st_join(g, left=F)

  roads$weight <- recode(roads$fclass,
                         !!!weights)

  sf::st_as_sf(roads)%>%
    sf::write_sf("data/transport/transport_spatial.shp")

  return(roads)
}

data.roads <- function(){
  sf::read_sf("data/transport/transport_spatial.shp")
}
