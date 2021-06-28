#' Build support required for Shipping
#'
#' @return
#' @export
#'
#' @examples
data.build_shipping_support <- function(){

  # Global ship activity
  # https://datacatalog.worldbank.org/dataset/global-shipping-traffic-density

  # Get bbox
  g <- data.bps_map() %>%
    sf::st_make_valid()

  gsd <- raster::raster("data/shipping/shipdensity_global.tif") %>%
    raster::crop(as(g,"Spatial"))

  gsd[gsd==0] <- NA

  gsd.sf <- as(gsd, "SpatialPointsDataFrame") %>%
    sf::st_as_sf()

  intersection <- sf::st_intersection(gsd.sf, g)
  intersection$weight <- intersection$shipdensity_global

  # sf::write tooo slow
  library(rgdal)
  lapply(list.files("data/shipping","support.*", full.names = T), file.remove)
  writeOGR(as(intersection,"Spatial"), "data/shipping/","support",driver = "ESRI Shapefile")
}


data.shipping_support <- function(){
  sf::read_sf("data/shipping/support.shp")
}


#' Read Shipping emissions from Excel
#'
#' @return
#' @export
#'
#' @examples
data.agroob_emission <- function(){
  data.sheet_to_emissions(sheet_name="Shipping")
}
