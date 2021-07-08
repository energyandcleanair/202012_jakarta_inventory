
#' Build support for landfill sector
#' #TODO: not found relevant dataset yet
#'
landfill.build_support <- function(){

  library(osmdata)

  # Get bps map
  g <- data.bps_map()
  as.vector(sf::st_bbox(g))

  # Use osm data
  landfills <- opq(bbox=as.vector(sf::st_bbox(g))) %>%
    add_osm_feature(key = "landuse", value = "landfill") %>%
    osmdata_sf()

  landfills.2 <- opq(bbox=as.vector(sf::st_bbox(g)),
                     timeout = 300) %>%
    add_osm_feature(key = "name", value = "TPA", value_exact = F) %>%
    osmdata_sf()

  landfills.3 <- opq(bbox=as.vector(sf::st_bbox(g)),
                     timeout = 300) %>%
    add_osm_feature(key = "name", value = "Tempat Pemrosesan Akhir", value_exact = F) %>%
    osmdata_sf()

  # Attaching region_id (from BPS) to roads
  landfills <- bind_rows(
    landfills$osm_points,
    landfills.2$osm_points,
    landfills.3$osm_points
    ) %>%
    dplyr::distinct(osm_id, .keep_all=T) %>%
    dplyr::select(osm_id) %>%
    sf::st_join(g, left=F) %>%
    select(osm_id, id, geometry)

  landfills$weight <- 1

  sf::st_as_sf(landfills) %>%
    sf::write_sf("data/landfill/landfill_support.shp")

  return(stations)
}


landfill.get_support <- function(){
  sf::read_sf("data/landfill/landfill_support.shp")
}


landfill.get_emission <- function(){
  data.sheet_to_emissions(sheet_name="Methane-landfill")
}
