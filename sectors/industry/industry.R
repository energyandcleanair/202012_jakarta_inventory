#' Build support for industry
#' using other_industries tab from Google Sheet
#' https://docs.google.com/spreadsheets/d/1gbjo1ogvc32Dnd4MZOlQ_W5epzPDyOHbbt7JRJbr6z0/edit#gid=2051056520
#' @return support sf
industry.build_support <- function(){
  s <- read_csv("sectors/industry/other_industries.csv") %>%
    dplyr::select(facility_name=`Nama Perusahaan`, lon, lat, industry=`General Industri Group`) %>%
    mutate(category=recode(industry,
                           "Heavy industry"	= "Cement",
                           "Heavy industry (construction)" = "Cement",
                           "Glass" = "Cement",
                           "Metal & non-metallic minerals" = "Cement",
                           "Petrochemicals & Plastics" = "Petrochemicals",
                           "Rubber" = "Manufacturing",
                           "Petrochemicals" = "Petrochemicals",
                           "Paint" = "Petrochemicals",
                           "Waste treatment" = "Excluded",
                           "Power & Energy" = "Excluded",
                           "Waste" = "Waste",
                           "Steel" = "Metals",
                           "Oil & Gas" = "Petrochemicals",
                           "Steel processing" = "Metals",
                           "Industrial Manufacturing" = "Manufacturing",
                           "Metals & Minerals" = "Metals",
                           "Chemical Processing (CPI)" = "Chemicals",
                           "Pulp, Paper & Wood" = "Pulp&Paper"
                      )) %>%
    filter(category != "Excluded")

  weight_fuel <- tibble(
    category = c("Cement", "Petrochemicals", "Manufacturing", "Metals", "Pulp&Paper", "Chemicals"),
    coal     = c( 13     ,  0              ,  3             ,  5      ,  1          ,  11),
    oil     =  c( 0      ,  1              ,  0             ,  0      ,  0          ,  0),
    gas     =  c( 0      ,  1              ,  1             ,  0      ,  0          ,  1)
  )

  #TODO replace with future EF sent by Prof. Didin
  weight_emission <- read_csv("sectors/industry/emission_factors.csv")
  weight_emission$PM <- weight_emission$PM10

  s <- weight_fuel %>%
    tidyr::pivot_longer(-category, names_to="fuel", values_to="weight_fuel") %>%
    left_join(weight_emission) %>%
    tidyr::pivot_longer(cols=-c(category, fuel, weight_fuel),
                        names_to="poll",
                        values_to="weight_emission") %>%
    mutate(weight=weight_fuel*weight_emission) %>%
    group_by(category, poll) %>%
    summarise(weight=sum(weight)) %>%
    # tidyr::pivot_wider(names_from="poll", values_from="weight") %>%
    right_join(s) %>%
    filter(!is.na(lon)) %>%
    sf::st_as_sf(coords=c("lon","lat"))


  # Add BPS id
  g <- data.bps_map() %>%
    sf::st_make_valid()

  # Attaching gadm id to facilities
  s.rich <- s %>%
    sf::st_set_crs(sf::st_crs(g)) %>%
    sf::st_join(g, left=F)


  # There are some regions (3) without facilities, but with emissions...
  # We distribute evenly across these provinces
  emission <- industry.get_emission()
  ids_with_emissions <- unique(emission$id[emission$emission>0])
  missing_ids <- setdiff(ids_with_emissions, unique(s.rich$id))
  cat("Missing facilities in regions: ", paste(missing_ids, collapse=", "))


  additional_points <- sf::st_as_sf(sf::st_centroid(g %>% sf::st_make_grid(cellsize=0.05))) %>%
    sf::st_join(
      g %>% filter(id %in% missing_ids),
      left=F
    ) %>%
    mutate(category="Province",
           weight=1) %>%
    as.data.frame()

  additional_points_w_poll <- lapply(unique(s.rich$poll), function(poll) additional_points %>% mutate(poll=!!poll)) %>%
    do.call(bind_rows, .)

  s.rich <- bind_rows(
    s.rich,
    additional_points_w_poll)



  # read_csv("sectors/industry/large_industries.csv") %>%
  #   sf::st_as_sf(coords=c("LONGITUDE","LATITUDE")) %>%
  #   sf::st_set_crs(sf::st_crs(g)) %>%
  #   sf::st_join(g) %>%
  #   as.data.frame() %>%
  #   distinct(id) -> a
  #
  # missing_ids %in% a$id

  sf::write_sf(s.rich, "sectors/industry/industry_support.gpkg")
}

#' Get support for industry
#'
#' @return support sf
industry.get_support <- function(){
  sf::read_sf("sectors/industry/industry_support.gpkg") %>%
    rename(geometry=geom)
}


#' Read industry emission from excel
#'
#' @return emission tibble
industry.get_emission <- function(){

  e <- data.sheet_to_emissions(sheet_name='Manufacturing Industry')

  if(sum(e[e$location=="Kota Semarang","emission"])==0 & sum(e[e$location=="Kab. Semarang","emission"])>0){
    e[e$location=="Kota Semarang","location"] <- "tmp"
    e[e$location=="Kab. Semarang","location"] <- "Kota Semarang"
    e[e$location=="tmp","location"] <- "Kab. Semarang"
  }

  return(e)
}
