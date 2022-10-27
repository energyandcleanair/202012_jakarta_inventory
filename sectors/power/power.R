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
  # Add a buffer cause some locations are closed to the coast
  g <- data.bps_map(buffer_km=10) %>% st_make_valid()

  s_wid <- s %>%
    sf::st_join(g, left=F)

  # Adding power plant in missing regions
  emission <- power.get_emission()
  ids_with_emissions <- unique(emission$id[emission$emission>0])
  missing_ids <- setdiff(ids_with_emissions, unique(s_wid$id))
  cat("Missing power plants in regions: ", paste(missing_ids, collapse=", "))

  additional_points <- sf::st_as_sf(sf::st_centroid(g %>% sf::st_make_grid(cellsize=0.05))) %>%
    sf::st_join(
      g %>% filter(id %in% missing_ids),
      left=F
    ) %>%
    rename(geometry='...1') %>%
    mutate(weight=1)

  s_wid <- bind_rows(
    s_wid,
    additional_points)

  sf::write_sf(s_wid, "sectors/power/power_support.gpkg")
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
