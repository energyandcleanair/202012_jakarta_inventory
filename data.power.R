#' Build support for power plants
#'
#' @return
#' @export
#'
#' @examples
data.power.support <- function(){

  s <- readxl::read_xlsx("data/power/January 2021 Global Coal Plant Tracker.xlsx",
                    sheet='Units')  %>% filter(Country %in% c("Indonesia"),
               Status %in% "Operating") %>%
    sf::st_as_sf(coords=c("Longitude","Latitude"), crs=4326) %>%
    select(id.plant=`Tracker ID`,
           weight=`Capacity (MW)`)

  # Add gadm 1 and 2 ids
  g <- data.gadm()

  # Attaching gadm id to roads
  s.rich <- s %>%
    sf::st_join(g, left=F)

  return(s.rich)
}
