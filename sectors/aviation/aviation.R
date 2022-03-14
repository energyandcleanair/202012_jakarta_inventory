
#' Build support for aviation sector from manual list of airports used in report
#'
#' @return support .sf
aviation.build_support <- function(){

  library(tidygeocoder)

  # Use manual data
  airports <- c(
    "Halim Perdanakusuma Airport",
    "Husein Sastranegara Airport",
    "Kertajati International Airport",
    "Radin Inten II International Airport",
    "Soekarno Hatta International Airport",
    "Ahmad Yani Airport, Semarang",
    "Adi Sumarmo airport"
    )

  locs <- tidygeocoder::geocode(.tbl=tibble(address=airports), address="address")

  # Get bps map
  g <- data.bps_map()

  locs.sf <- locs %>%
    sf::st_as_sf(coords=c("long","lat"), crs=4326) %>%
    sf::st_join(g, left=F) %>%
    dplyr::select(airport=address, id, geometry) %>%
    mutate(weight=1)

  locs.sf %>% sf::write_sf("sectors/aviation/aviation_support.gpkg")

  return(locs.sf)
}


aviation.get_support <- function(){
  sf::read_sf("sectors/aviation/aviation_support.gpkg") %>%
    rename(geometry=geom)
}


aviation.get_emission <- function(){
  e <- data.sheet_to_emissions(sheet_name="Air-transportation")

  # "Adi Sumarmo airport" Airport region attribution is wrong
  e <- e %>%
    filter(emission>0) %>%
    mutate(
      location=recode(location, "Kota Surakarta"="Boyolali"),
      id=recode(id, "ID3372"="ID3309"),
           )
  return(e)
}

#
# aviation.get_date_weight <- function(){
#   s <- aviation.get_support
#
#   t <- readxl::read_xlsx("sectors/aviation/timeseries.xlsx",
#                          sheet='Sheet2')
#
# }
