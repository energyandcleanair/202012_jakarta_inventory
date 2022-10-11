
#' Build simplified road network, keeping only required levels, and adding the bps id to it
#' so that it can be merged with emission data
#'
#' @return support.sf
transport.build_support <- function(){

  # f <- "sectors/transport/osm/gis_osm_roads_free_1.shp"
  f <- 'sectors/transport/indonesia-latest.osm.pbf'

  if(!file.exists(f)){
    download.file(
      url="https://download.geofabrik.de/asia/indonesia-latest.osm.pbf",
      destfile="sectors/transport/indonesia-latest.osm.pbf")
  }

  # osm.roads <- sf::read_sf("sectors/transport/osm/gis_osm_roads_free_1.shp")
  osm.roads <- rgdal::readOGR(f, layer = 'lines') %>%
    sf::st_as_sf() %>%
    mutate(fclass = highway)

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
    dplyr::select(osm_id, weight, id) %>%
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

#' Get date-time weight for transportation sector
#'
#' @return
transport.get_datetime_weight <- function(){
  weight_data <- read.csv('sectors/transport/hourly_weight.csv', fileEncoding='UTF-8-BOM') %>%
    pivot_longer(2:8, names_to='day', values_to='weight')
  weight <- tibble(date=seq(as.POSIXct("2019-01-01 00:00:00"),
                            as.POSIXct("2019-12-31 23:00:00"), by="hour")) %>%
    mutate(day=lubridate::wday(date, label=T, abbr=F),
           hour=lubridate::hour(date)) %>%
    left_join(weight_data, by=c('day', 'hour')) %>%
    select(-day, -hour)
}
