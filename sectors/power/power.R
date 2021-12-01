#' Build support for power plants
#' Nothing to be done, using GCPT xls file
#' @return support sf
power.build_support <- function(){

  s <- readr::read_csv("sectors/power/global_power_plant_database_sea.csv") %>%
    filter(is.na(commissioning_year) | commissioning_year <= 2019,
           primary_fuel %in% c("Coal","Gas")) %>%
    sf::st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
    select(id.plant=gppd_idnr,
           weight=capacity_mw)

  # Add BPS id
  g <- data.bps_map() %>% sf::st_make_valid()

  # Move power plants inside provinces

  # Identify points outside the polygons
  # s$outside <- sapply(st_intersects(s, g), function(x){length(x)==0})
  # ggplot() + geom_sf(data=g) + geom_sf(data=s, aes(col=outside))

  s.rich <- s %>%
    sf::st_join(g, left=F)
#
#   s.rich.additional <- s.additional %>%
#     sf::st_join(g, left=F)

  sf::write_sf(s.rich, "sectors/power/power_support.gpkg")
}

#' Get support for power plants
#'
#' @return support sf
power.get_support <- function(){
  sf::read_sf("sectors/power/power_support.gpkg") %>%
    rename(geometry=geom)
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
