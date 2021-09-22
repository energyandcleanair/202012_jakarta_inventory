
#' Build simplified road network, keeping only required levels, and adding the bps id to it
#' so that it can be merged with emission data
#'
#' @return support.sf
transport.build_support <- function(){

  f <- "sectors/transport/osm/gis_osm_roads_free_1.shp"

  if(!file.exists(f)){
    download.file(
      url="http://download.geofabrik.de/asia/indonesia-latest-free.shp.zip",
      destfile="sectors/transport/osm.zip")

    unzip("sectors/transport/osm.zip")
  }

  osm.roads <- sf::read_sf("sectors/transport/osm/gis_osm_roads_free_1.shp")

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
    dplyr::select(osm_id, weight) %>%
    sf::write_sf("sectors/transport/transport_support.gpkg")

  return(roads)
}

#' Get support for transportation sector
#'
#' @return support sf
transport.get_support <- function(){
  sf::read_sf("sectors/transport/transport_support.gpkg") %>%
    rename(geometry=geom)
}

#' Get emission data for transportation sector
#'
#' @return emission tibble
transport.get_emission <- function(){
  data.sheet_to_emissions(sheet_name="On-road-transportation")
}
