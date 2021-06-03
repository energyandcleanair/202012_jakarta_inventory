#' Build support required for Commercial & residential
#'
#' @return
#' @export
#'
#' @examples
data.build_comres_support <- function(){

  lu <- sf::read_sf("data/landuse/land_cover_2019.geojson") %>%
    filter(Legenda=="Pemukiman") %>%
    mutate(weight=1) %>%
    st_make_valid()

  lu <- lu %>%
    filter(sf::st_geometry_type(geometry) %in% c("MULTIPOLYGON","POLYGON"))

  # Add the kabupaten map
  g <- data.bps_map() %>%
    sf::st_make_valid()

  intersection <- sf::st_intersection(lu, g)
  intersection$weight <- 1

  sf::write_sf(intersection, "data/comres/support.shp")

  return(intersection)
}


data.comres_support <- function(){
  sf::read_sf("data/comres/support.shp")
}


#' Read Commercial & Residential emission from excel
#'
#' @return
#' @export
#'
#' @examples
data.comres_emission <- function(){

  s <- readxl::read_xlsx("data/Emission-2019-compilation-send.xlsx",
                         sheet='Residential and commercial ',
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
