
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
  s <- readxl::read_xlsx("data/Emission-2019-compilation-send.xlsx",
                         sheet='On-road-transportation',
                         skip = 1)
  s <- s %>% rename(location=`Province/Cities`)
  s <- s %>% filter(!str_detect(location, "total"),
                    !is.na(location))



  s <- s %>%
    tidyr::pivot_longer(names_to="poll",
                        values_to="emission",
                        -location) %>%
    filter(!is.na(emission))

  s$unit <- "tonnes"
  s$year <- 2019

  s$id <- utils.location_name_to_bps_id(s$location)

  if(nrow(s[is.na(s$id),])>0){
    stop("Missing ids for regions ", s[is.na(s$bps_id),] %>% distinct(location))
  }


}
