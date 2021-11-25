#' Build support required for Agriculture open burning
#' using land type intersection with VIIRS fires
#'
#' @return support sf
agroob.build_support <- function(){

  # Taking more than one year to get more
  # representative distribution
  date_from = "2019-01-01"
  date_to = "2019-12-31"

  # Get land use with agriculture on it
  lu <- data.land_use(type="agroob") %>%
  # lu <- data.land_use(type=NULL) %>%
    mutate(weight=1) %>%
    sf::st_make_valid()

  # Add the kabupaten / province map
  g <- data.bps_map() %>%
    sf::st_make_valid()

  intersection <- sf::st_intersection(lu, g)
  intersection <- intersection %>%
    filter(sf::st_geometry_type(geometry) %in% c("MULTIPOLYGON","POLYGON"))

  extent.sp <- sf::as_Spatial(intersection$geometry[!sf::st_is_empty(intersection$geometry)])

  # Get fires over that region
  creatrajs::fire.download(date_from=date_from,
                           date_to=date_to)

  fires <- creatrajs::fire.read(date_from=date_from,
                       date_to=date_to,
                       extent.sp=extent.sp)

  fires_w_id <- fires %>%
    sf::st_join(intersection %>% select(id=province)) %>%
    rename(weight=frp)

  fires_w_id$acq_date <- NULL

  fires_w_id %>%
    sf::st_write("sectors/agroob/agroob_support.gpkg")

  return(fires_w_id)
}


agroob.get_support <- function(){
  sf::read_sf("sectors/agroob/agroob_support.gpkg") %>%
    rename(geometry=geom)
}


#' Read Agriculture burning emission from excel
#' and sum it by province
#'
#' @return emission tibble
agroob.get_emission <- function(){
  e <- data.sheet_to_emissions(sheet_name="Agro-residual-OB")
  g <- data.bps_map() %>%
    sf::st_make_valid()

  # Using province as id
  e %>%
    left_join(g %>% select(id, province)) %>%
    group_by(id=province, poll, unit, year) %>%
    summarise_at("emission", sum, na.rm=T)
}
