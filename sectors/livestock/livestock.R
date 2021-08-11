

#' @return support.sf
livestock.build_support <- function(){

  # https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/GIVQ75/I5CUJS&version=3.0

  glws <- list(
    c(url="https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/GIVQ75/I5CUJS",
      weight=(11+38.2)/2, # not sure what proportion is dairy vs beef cow. Using average
      file = "sectors/livestock/glw_cattle.tif"),
    c(url="https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/BLWPZN/XDIRM4",
      weight=3.4,
      file = "sectors/livestock/glw_sheeps.tif"),
    c(url="https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/SUFASB/AP1LHN",
      weight=0.27,
      file = "sectors/livestock/glw_chicken.tif"),
    c(url="https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/33N0JG/9LGGBS",
      weight=6.5,
      file = "sectors/livestock/glw_pigs.tif"),
    c(url="https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/7Q52MV/UAWH3Z",
      weight=12.3,
      file = "sectors/livestock/glw_horses.tif"),
    c(url="https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/OCPH42/ZMVOOW",
      weight=6.4,
      file = "sectors/livestock/glw_goats.tif")
  )


  rs <- lapply(glws, function(glw){
    if(!file.exists(glw[["file"]])) download.file(url=glw[["url"]],destfile=glw[["file"]])
    raster::raster(glw[["file"]]) * as.numeric(glw[["weight"]])
  })

  glw <- raster::stack(rs) %>% raster::calc(sum)

  # Add the kabupaten map
  g <- data.bps_map() %>% sf::st_make_valid()
  glw <- glw %>% raster::crop(sf::st_bbox(g))

  sf::st_as_sf(as(glw, "SpatialPolygonsDataFrame")) %>%
    sf::st_make_valid() %>%
    rename(weight=layer) %>%
    filter(weight>0) %>%
    sf::st_intersection(g %>% dplyr::select(id)) %>%
    filter(!is.na(id)) %>%
    sf::write_sf("sectors/livestock/livestock_support.gpkg")

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
