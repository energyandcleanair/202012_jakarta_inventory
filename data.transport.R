
#' Build simplified road network, keeping only required levels, and adding the bps id to it
#' so that it can be merged with emission data
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


  # Add the kabupaten map
  g <- data.bps_map()

  # Attaching region_id (from BPS) to roads
  roads <- osm.roads.lite %>%
    sf::st_join(g, left=F)

  roads$weight <- recode(roads$fclass,
                         !!!weights)

  sf::st_as_sf(roads)%>%
    sf::write_sf("data/transport/transport_spatial.shp")

  return(roads)
}

data.transport_support <- function(){
  sf::read_sf("data/transport/transport_spatial.shp")
}

data.transport_emission <- function(){
  data.sheet_to_emissions(sheet_name="On-road-transportation")
}
