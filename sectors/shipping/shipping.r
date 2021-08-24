#' Build support for Shipping using manual list of harbours used in report
#'
#' @return support sf
shipping.build_support <- function(){

  library(tidygeocoder)

  # Use manual data
  harbours <- c(
    "Pelabuhan Tanjung Priok",
    "Bakauheni Port, Lampung",
    "Merak, Cilegon, Banten"
  )

  # Another option could have been Global ship activity
  # https://datacatalog.worldbank.org/dataset/global-shipping-traffic-density

  locs <- tidygeocoder::geocode(.tbl=tibble(address=harbours), address="address")

  # Get bps map
  g <- data.bps_map()

  locs.sf <- locs %>%
    sf::st_as_sf(coords=c("long","lat"), crs=4326) %>%
    sf::st_join(g, left=F) %>%
    dplyr::select(airport=address, id, geometry) %>%
    mutate(weight=1)

  locs.sf %>% sf::write_sf("sectors/shipping/shipping_support.gpkg")

  return(locs.sf)
}


shipping.get_support <- function(){
  sf::read_sf("sectors/shipping/shipping_support.gpkg") %>%
    rename(geometry=geom)
}


shipping.get_emission <- function(){
  data.sheet_to_emissions(sheet_name="Shipping")
}
