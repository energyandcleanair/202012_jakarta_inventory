#' Build support required for Shipping
#'
#' @return
#' @export
#'
#' @examples
data.build_shipping_support <- function(){

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

  locs.sf %>% sf::write_sf("data/shipping/harbours.shp")

  return(locs.sf)
}

data.shipping_support <- function(){
  sf::read_sf("data/shipping/harbours.shp")
}

data.shipping_emission <- function(){
  data.sheet_to_emissions(sheet_name="Shipping")
}
