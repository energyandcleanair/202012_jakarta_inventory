#' Build support for power plants
#'
#' @return
#' @export
#'
#' @examples
data.power_support <- function(){

  s <- readxl::read_xlsx("data/power/January 2021 Global Coal Plant Tracker.xlsx",
                    sheet='Units')  %>% filter(Country %in% c("Indonesia"),
               Status %in% "Operating") %>%
    sf::st_as_sf(coords=c("Longitude","Latitude"), crs=4326) %>%
    select(id.plant=`Tracker ID`,
           weight=`Capacity (MW)`)

  s %>% sf::write_sf("data/power/January 2021 Global Coal Plant Tracker.shp")

  # Add BPS id
  g <- data.bps_map()

  # Attaching gadm id to roads
  s.rich <- s %>%
    sf::st_join(g, left=F)

  return(s.rich)
}


#' Read power emission from excel
#'
#' @return
#' @export
#'
#' @examples
data.power_emission <- function(){

  s <- readxl::read_xlsx("data/Emission-2019-compilation-send.xlsx",
                         sheet='Power-generation',
                         skip = 1)
  s <- s %>% rename(location=`Province/Cities`)
  s <- s %>% filter(!str_detect(location, "total"),
               !is.na(location))



  s <- s %>%
    tidyr::pivot_longer(names_to="poll",
                        values_to="emission",
                        -location) %>%
    filter(!is.na(emission))

  s$unit <- "tonnes"
  s$year <- 2019

  s$id <- utils.location_name_to_bps_id(s$location)

  if(nrow(s[is.na(s$id),])>0){
    stop("Missing ids for regions ", s[is.na(s$bps_id),] %>% distinct(location))
  }


  return(s)
}
