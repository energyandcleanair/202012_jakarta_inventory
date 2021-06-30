# Forest Fires
# Hubert: The original data used in the report is at a provincial level.
# Prof. Didin probably scaled it down to kota and kabupaten based on some area coverage.
# However, when looking at VIIRS data (on which is based original data), not all kota/kabupaten have detected fires in 2019.
# Solution: we bring back the data to the original regional scale, and attribute emissions down to each pixel based on detected FRPs.
# Comment: ideally, we would have land cover by crop, but haven't found a relevant dataset yet.

#' Building support for Agriculture burning
#'
#' @return
#' @export
#'
#' @examples
data.build_agroob_support <- function(){

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

  # sf::write tooo slow
  library(rgdal)
  lapply(list.files("data/agroob","support.*", full.names = T), file.remove)
  writeOGR(as(fires_w_id,"Spatial"), "data/agroob/","support",driver = "ESRI Shapefile")
}


data.agroob_support <- function(){
  sf::read_sf("data/agroob/support.shp")
}


#' Read Agriculture burning emission from excel
#' and sum it by province
#'
#' @return
#' @export
#'
#' @examples
data.agroob_emission <- function(){
  e <- data.sheet_to_emissions(sheet_name="Agro-residual-OB")
  g <- data.bps_map() %>%
    sf::st_make_valid()

  # Using province as id
  e %>%
    left_join(g %>% select(id, province)) %>%
    group_by(id=province, poll, unit, year) %>%
    summarise_at("emission", sum, na.rm=T)
}
