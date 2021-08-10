

#' @return support.sf
livestock.build_support <- function(){

  # https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/GIVQ75/I5CUJS&version=3.0
  download.file(url="https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/GIVQ75/I5CUJS",
                destfile = "sectors/livestock/glw.tif")

  glw <- raster::raster("sectors/livestock/glw.tif")

  # Add the kabupaten map
  g <- data.bps_map() %>% sf::st_make_valid()
  glw <- glw %>% raster::crop(sf::st_bbox(g))

  sf::st_as_sf(as(glw, "SpatialPolygonsDataFrame")) %>%
    sf::st_make_valid() %>%
    rename(weight=glw) %>%
    filter(weight>0) %>%
    sf::st_intersection(g %>% dplyr::select(id)) %>%
    filter(!is.na(id)) %>%
    sf::write_sf("sectors/livestock/livestock_support.gpkg")

  return(roads)
}

#' Get support for livestock sector
#'
#' @return support sf
livestock.get_support <- function(){
  sf::read_sf("sectors/livestock/livestock_support.gpkg") %>%
    rename(geometry=geom)
}



#' Get emission data for livestock sector
#'
#' @return emission tibble
livestock.get_emission <- function(){
  data.sheet_to_emissions(sheet_name="Ammonia Livestock")
}
