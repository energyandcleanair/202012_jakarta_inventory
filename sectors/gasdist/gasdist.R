
#' Build gasdist support (gas stations) using osm data
#'
#' @return support sf
gasdist.build_support_osm <- function(){

  library(osmdata)

  # Get bps map
  g <- data.bps_map()
  bbox <- as.vector(sf::st_bbox(g))

  # Combine several OS queries
  # amenity=fuel
  stations.sf.1 <- opq(bbox=bbox, timeout=300) %>%
    add_osm_feature(key = "amenity", value = "fuel", value_exact=F) %>%
    osmdata_sf()

  # name=gas station
  stations.sf.2 <- opq(bbox=bbox, timeout=300) %>%
    add_osm_feature(key = "name", value = "Gas Station", value_exact=F, match_case=F) %>%
    osmdata_sf()

  # name:en=gas station
  stations.sf.3 <- opq(bbox=bbox, timeout=300) %>%
    add_osm_feature(key = "name:en", value = "Gas Station", value_exact=F, match_case=F) %>%
    osmdata_sf()

  # name=spbu
  stations.sf.4 <- opq(bbox=bbox, timeout=300) %>%
    add_osm_feature(key = "name", value = "SPBU", value_exact=F, match_case=F) %>%
    osmdata_sf()

  stations.sf <- bind_rows(stations.sf.1$osm_points,
                           stations.sf.2$osm_points,
                           stations.sf.3$osm_points,
                           stations.sf.4$osm_points
                           ) %>%
    dplyr::distinct(osm_id, .keep_all=T)


  # Attaching region_id (from BPS) to roads
  stations <- stations.sf %>%
    sf::st_set_crs(4326) %>%
    sf::st_join(g, left=F) %>%
    select(osm_id, id, geometry)

  stations$weight <- 1

  sf::st_as_sf(stations) %>%
    sf::write_sf("sectors/gasdist/support_osm.gpkg")

  return(stations)
}


#' Build gasdist support (gas stations) using Google Places
#'
#' @return support sf
gasdist.build_support_google <- function(){

  # Google places
  # Limited to 60 nearest results...
  library(googleway)
  library(pbapply)
  readRenviron(".Renviron")

  g <- data.bps_map()

  centers <- sf::st_make_grid(g,
                   cellsize = 0.05,
                   what = "centers") %>%
    sf::st_intersection(sf::st_make_valid(g))

  # sf::write_sf(centers, "data/gasdist/centers_01.shp")

  get_stations <- function(pt){
    tryCatch({
      coords <- c(pt[2], pt[1])
      page <- 0
      stations.query <- google_places(
        # search_string =
        location = coords,
        radius = 50000,
        place_type="gas_station",
        key=Sys.getenv("GOOGLE_PLACES_API_KEY"))
      stations <- stations.query$results

      while(!is.null(stations.query$next_page_token)){
        page <- page + 1
        nexttoken <- stations.query$next_page_token
        Sys.sleep(3)
        stations.query <- google_places(
          location = coords,
          radius = 50000,
          place_type="gas_station",
          page_token=nexttoken,
          key=Sys.getenv("GOOGLE_PLACES_API_KEY")
        )
        stations <- bind_rows(stations.query$results,
                              stations)
      }
      return(stations)
    }, error=function(e){
      return(NULL)
    })
  }

  stations <- pblapply(centers, get_stations)

  stations.clean <- do.call(bind_rows, stations) %>%
    distinct(place_id, .keep_all=T)

  stations.clean$lat <- stations.clean$geometry[[1]]$lat
  stations.clean$lng <- stations.clean$geometry[[1]]$lng

  stations.clean.sf <- stations.clean %>%
    dplyr::select(place_id, lat, lng) %>%
    sf::st_as_sf(coords=c("lng","lat")) %>%
    mutate(weight=1) %>%
    # Attaching region_id (from BPS)
    sf::st_set_crs(4326) %>%
    sf::st_join(g, left=F)

  stations.clean.sf %>%
    sf::write_sf("sectors/gasdist/support_google.gpkg")

  return(stations)
}


#' Build gasdist support (gas stations) from Google Places
#'
#' @return support sf
gasdist.build_support <- function(){
  gasdist.build_support_google()
}


#' Return google support
#'
#' @return support sf
gasdist.get_support <- function(){
  sf::read_sf("sectors/gasdist/support_google.gpkg") %>%
    rename(geometry=geom)
}


#' Get emission data for gasdist sector
#'
#' @return emissino tibble
gasdist.get_emission <- function(){
  data.sheet_to_emissions(sheet_name="Gasoline distribution")
}
