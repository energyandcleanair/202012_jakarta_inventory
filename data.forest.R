# Forest Fires
# Hubert: The original data used in the report is at a provincial level: http://sipongi.menlhk.go.id/hotspot/luas_kebakaran
# Prof. Didin probably scaled it down to kota and kabupaten based on some area coverage.
# However, when looking at VIIRS data (on which is based original data), not all kota/kabupaten have detected fires in 2019.
# Solution: we bring back the data to the original regional scale, and attribute emissions down to each pixel based on detected FRPs.



#' Build support at province level
#'
#' @return
#' @export
#'
#' @examples
data.build_forest_support <- function(){

  # Taking more than one year to get more
  # representative distribution
  date_from = "2019-01-01"
  date_to = "2019-12-31"

  # Get land use with agriculture on it
  lu <- data.land_use(type="forest") %>%
    mutate(weight=1) %>%
    sf::st_make_valid()

  # Add the kabupaten map
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
    select(weight=frp) %>%
    sf::st_join(intersection %>% select(id=province))

  fires_w_id$acq_date <- NULL
  # sf::write tooo slow
  library(rgdal)
  lapply(list.files("data/forest","support.*", full.names = T), file.remove)
  writeOGR(as(fires_w_id,"Spatial"), "data/forest/","support",driver = "ESRI Shapefile")
}


data.forest_support <- function(){
  sf::read_sf("data/forest/support.shp")
}


#' Read forest fire emission from excel
#'
#' @return
#' @export
#'
#' @examples
data.forest_emission <- function(){
  e <- data.sheet_to_emissions(sheet_name="Forest Fire")
  g <- data.bps_map() %>%
    sf::st_make_valid()

  # Using province as id
  e %>%
    left_join(g %>% select(id, province)) %>%
    group_by(id=province, poll, unit, year) %>%
    summarise_at("emission", sum, na.rm=T)
}
