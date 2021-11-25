#' Build support for power plants
#' Nothing to be done, using GCPT xls file
#' @return support sf
power.build_support <- function(){

}

#' Get support for power plants
#'
#' @return support sf
power.get_support <- function(){

  s <- readxl::read_xlsx("sectors/power/January 2021 Global Coal Plant Tracker.xlsx",
                    sheet='Units')  %>% filter(Country %in% c("Indonesia"),
               Status %in% "Operating") %>%
    sf::st_as_sf(coords=c("Longitude","Latitude"), crs=4326) %>%
    select(id.plant=`Tracker ID`,
           weight=`Capacity (MW)`)

  s %>% sf::write_sf("sectors/power/January 2021 Global Coal Plant Tracker.gpkg")

  # Add BPS id
  g <- data.bps_map()

  # Attaching gadm id to roads
  s.rich <- s %>%
    sf::st_join(g, left=F)

  return(s.rich)
}


#' Read power emission from excel
#' [FIX] # Prof. Didin: ... you are right Semarang PGU is located in Kota Semarang not Kabupaten Semarang.
# Switching Kota <> Kab.
#'
#' @return emission tibble
power.get_emission <- function(){

  e <- data.sheet_to_emissions(sheet_name='Power-generation')

  if(sum(e[e$location=="Kota Semarang","emission"])==0 & sum(e[e$location=="Kab. Semarang","emission"])>0){
    e[e$location=="Kota Semarang","location"] <- "tmp"
    e[e$location=="Kab. Semarang","location"] <- "Kota Semarang"
    e[e$location=="tmp","location"] <- "Kab. Semarang"
  }

  return(e)
}
