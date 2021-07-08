
#' We use population density as support for solid waste open burning
#'
#' @return
#' @export
#'
#' @examples
data.build_solidwaste_support <- function(){

  # Get bps map
  g <- data.bps_map()

  # Get population density
  gis_dir <- "/Volumes/ext1/gis/"
  gpw.global <- creahelpers::get_population_path("gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif") %>%
    raster::raster()
  gpw <- gpw.global %>% raster::crop(sf::st_bbox(g))

  sf::st_as_sf(as(gpw, "SpatialPointsDataFrame")) %>%
    rename(weight=gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec) %>%
    filter(weight>0) %>%
    sf::st_join(g %>% dplyr::select(id)) %>%
    filter(!is.na(id)) %>%
    sf::write_sf("data/solidwaste/solidwaste_support.shp")

  return(stations)
}

data.solidwaste_support <- function(){
  sf::read_sf("data/solidwaste/solidwaste_support.shp")
}

data.solidwaste_emission <- function(){
  data.sheet_to_emissions(sheet_name="Solid Waste OB")
}
