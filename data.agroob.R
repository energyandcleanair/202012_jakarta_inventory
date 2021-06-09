#' Build support required for Commercial & residential
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
  # lu <- data.land_use(type="agroob") %>%
  lu <- data.land_use(type=NULL) %>%
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
    sf::st_join(intersection %>% select(id)) %>%
    rename(weight=frp)

  # sf::write tooo slow
  library(rgdal)
  lapply(list.files("data/agroob","support.*", full.names = T), file.remove)
  writeOGR(as(fires_w_id,"Spatial"), "data/agroob/","support",driver = "ESRI Shapefile")
}


data.agroob_support <- function(){
  sf::read_sf("data/agroob/support.shp")
}


#' Read Commercial & Residential emission from excel
#'
#' @return
#' @export
#'
#' @examples
data.agroob_emission <- function(){
  data.sheet_to_emissions(sheet_name="Agro-residual-OB")
}
