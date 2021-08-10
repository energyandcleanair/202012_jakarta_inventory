#' Build support for industry
#' using PROPER Industry database
#' TODO: improve weighting
#' @return support sf
industry.build_support <- function(){
  s <- readxl::read_xlsx("sectors/industry/PROPER_Industry List_Work.xlsx",
                         sheet='Full list') %>%
    filter(`General Industri Group` != "Power & Energy") %>%
    sf::st_as_sf(coords=c("lon","lat"), crs=4326) %>%
    dplyr::select(facility=No, type=`General Industri Group`) %>%
    mutate(weight=1)

  # Add BPS id
  g <- data.bps_map()

  # Attaching gadm id to roads
  s.rich <- s %>%
    sf::st_join(g, left=F)

  sf::write_sf(s.rich, "sectors/industry/support.shp")
  return(s.rich)
}

#' Get support for industry
#'
#' @return support sf
industry.get_support <- function(){
  sf::read_sf("sectors/industry/support.shp")
}


#' Read industry emission from excel
#'
#' @return emission tibble
industry.get_emission <- function(){

  e <- data.sheet_to_emissions(sheet_name='Power-generation')

  if(sum(e[e$location=="Kota Semarang","emission"])==0 & sum(e[e$location=="Kab. Semarang","emission"])>0){
    e[e$location=="Kota Semarang","location"] <- "tmp"
    e[e$location=="Kab. Semarang","location"] <- "Kota Semarang"
    e[e$location=="tmp","location"] <- "Kab. Semarang"
  }

  return(e)
}
