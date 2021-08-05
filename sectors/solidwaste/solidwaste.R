#' We use population density as support for solid waste open burning
#'
#' @return
#' @export
#'
#' @examples
solidwaste.build_support <- function(){

  # Get bps map
  g <- data.bps_map()

  # Get population density
  # gis_dir <- "/Volumes/ext1/gis/"
  creahelpers::get_gis_dir()
  gpw.global <- creahelpers::get_population_path("gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif") %>%
    raster::raster()
  gpw <- gpw.global %>% raster::crop(sf::st_bbox(g))

  sf::st_as_sf(as(gpw, "SpatialPolygonsDataFrame")) %>%
    rename(weight=gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec) %>%
    filter(weight>0) %>%
    sf::st_intersection(g %>% dplyr::select(id)) %>%
    filter(!is.na(id)) %>%
    sf::write_sf("sectors/solidwaste/solidwaste_support.shp")

  return(stations)
}


solidwaste.get_support <- function(){
  sf::read_sf("sectors/solidwaste/solidwaste_support.shp")
}


solidwaste.get_emission <- function(){
  data.sheet_to_emissions(sheet_name="Solid Waste OB")
}
