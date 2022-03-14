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
  names(gpw) <- "gpw"

  # Version 2
  # breaks <- seq(0,1,0.05)
  # gpw_quantiles <- raster::quantile(gpw, breaks)
  gpw_cut <- raster::cut(gpw, breaks=20)
  gpw_cut_pol <- raster::rasterToPolygons(gpw_cut, dissolve=T)
  gpw_cut_pol$weight <- raster::extract(gpw, gpw_cut_pol, fun=mean)


  s <- sf::st_as_sf(gpw_cut_pol) %>%
    dplyr::select(weight) %>%
    filter(weight>0) %>%
    sf::st_make_valid() %>%
    sf::st_intersection(g %>% sf::st_make_valid() %>% dplyr::select(id)) %>%
    filter(!is.na(id)) %>%
    sf::st_collection_extract() %>%
    filter(grepl("POLYGON", sf::st_geometry_type(geometry))) %>%
    sf::st_cast("POLYGON")

  sf::write_sf(s, "sectors/solidwaste/solidwaste_support.gpkg")

}


solidwaste.get_support <- function(){
  sf::read_sf("sectors/solidwaste/solidwaste_support.gpkg") %>%
    rename(geometry=geom)
}


solidwaste.get_emission <- function(){
  data.sheet_to_emissions(sheet_name="Solid Waste OB")
}
