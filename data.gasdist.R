
#' Build simplified road network, keeping only required levels, and adding the bps id to it
#' so that it can be merged with emission data
#'
#' @return
#' @export
#'
#' @examples
data.build_gasdist_support <- function(){

  library(osmdata)

  # Get bps map
  g <- data.bps_map()
  as.vector(sf::st_bbox(g))


  # amenity=fuel: stations
  stations.q.1 <- opq(bbox=as.vector(sf::st_bbox(g))) %>%
    add_osm_feature(key = "amenity", value = "fuel")
  stations.xml <- osmdata_xml(stations.q.1, filename = 'data/gasdistrib/gasdistrib.osm')
  stations.sf <- osmdata_sf(stations.q.1, stations.xml)$osm_points

  # name=gas station:
  stations.q.2 <- opq(bbox=as.vector(sf::st_bbox(g)),
                      timeout = 300) %>%
    # add_osm_feature(key = "building", value = "yes") %>%
    add_osm_feature(key = "name", value = "Gas Station", value_exact=F, match_case=F)
  stations.xml.2 <- osmdata_xml(stations.q.2, filename = 'data/gasdistrib/gasdistrib.2.osm')
  stations.sf.2 <- osmdata_sf(stations.q.2, stations.xml.2)$osm_points

  stations.q.3 <- opq(bbox=as.vector(sf::st_bbox(g)),
                      timeout = 300) %>%
    add_osm_feature(key = "name:en", value = "Gas Station", value_exact=F, match_case=F)
  stations.xml.3 <- osmdata_xml(stations.q.3, filename = 'data/gasdistrib/gasdistrib.3.osm')
  stations.sf.3 <- osmdata_sf(stations.q.3, stations.xml.3)$osm_points

  stations.sf <- bind_rows(stations.sf,
                           stations.sf.2,
                           stations.sf.3,
                           ) %>%
    dplyr::distinct(osm_id, .keep_all=T)
  # Google places
  # Limited to 60 nearest results...
  # library(googleway)
  # readRenviron(".Renviron")
  #
  # page <- 0
  # stations.query <- google_places(search_string="Indonesia",
  #               place_type="gas_station",
  #               key=Sys.getenv("GOOGLE_PLACES_API_KEY"))
  # stations <- stations.query$results
  #
  # while(!is.null(stations.query$next_page_token)){
  #   page <- page + 1
  #   nexttoken <- stations.query$next_page_token
  #   stations.query <- google_places(search_string="Indonesia",
  #                                   place_type="gas_station",
  #                                   page_token=nexttoken,
  #                                   key=Sys.getenv("GOOGLE_PLACES_API_KEY"))
  #   stations <- bind_rows(stations.query$results,
  #                         stations)
  # }

  # Attaching region_id (from BPS) to roads
  stations <- stations.sf %>%
    sf::st_join(g, left=F) %>%
    select(osm_id, id, geometry)

  stations$weight <- 1

  sf::st_as_sf(stations) %>%
    sf::write_sf("data/gasdist/gasdist.shp")

  return(stations)
}

data.gasdist_support <- function(){
  sf::read_sf("data/gasdist/gasdist.shp")
}

data.gasdist_emission <- function(){
  data.sheet_to_emissions(sheet_name="Gasoline distribution")
}
