# Forest Fires
# Hubert: The original data used in the report is at a provincial level: http://sipongi.menlhk.go.id/hotspot/luas_kebakaran
# Prof. Didin probably scaled it down to kota and kabupaten based on some area coverage.
# However, when looking at VIIRS data (on which is based original data), not all kota/kabupaten had detected fires in 2019.
# Solution: we bring back the data to the original regional scale, and attribute emissions down to kota / kabupaten based on detected FRPs.


#' Build support
#'
#' @return support sf
forest.build_support <- function(){

  # Taking more than one year to get more
  # representative distribution
  date_from = "2016-01-01"
  date_to = "2020-12-31"

  # Get land use with agriculture on it
  lu <- data.land_use(type="forest") %>%
    mutate(weight=1) %>%
    sf::st_make_valid()

  # Add the kabupaten / kota map
  g <- data.bps_map()

  intersection <- sf::st_intersection(lu %>% filter(sf::st_is_valid(geometry)), g)
  intersection <- intersection %>%
    filter(sf::st_geometry_type(geometry) %in% c("MULTIPOLYGON","POLYGON"))

  extent.sp <- sf::as_Spatial(intersection$geometry[!sf::st_is_empty(intersection$geometry)])

  # Get fires over that region
  creatrajs::fire.download(date_from=date_from,
                           date_to=date_to)

  dates <- seq(as.Date(date_from), as.Date(date_to), by="day")
  fires <- lapply(split(dates, lubridate::floor_date(dates, 'month')), function(year_dates){
    creatrajs::fire.read(date_from=min(year_dates),
                         date_to=max(year_dates),
                         extent.sp=extent.sp,
                         parallel = F,
                         show.progress = F)
  }) %>% do.call(bind_rows, .)

  fires_w_id <- fires %>%
    sf::st_join(intersection %>%
                  filter(sf::st_is_valid(geometry)) %>%
                  select(id=province)) %>%
    rename(weight=frp)

  # Use dates to save monthly patterns
  date_weight = fires_w_id %>%
    as.data.frame() %>%
    group_by(id, date=lubridate::floor_date(acq_date, 'day')) %>%
    summarise(weight=sum(weight))
  saveRDS(date_weight, "sectors/forest/forest_date_weight.RDS")

  fires_w_id$acq_date <- NULL

  f <- "sectors/forest/forest_support.gpkg"
  if(file.exists(f)) file.remove(f)
  fires_w_id %>%
    sf::st_write(f)

  return(fires_w_id)
}


forest.get_support <- function(){
  sf::read_sf("sectors/forest/forest_support.gpkg") %>%
    rename(geometry=geom)
}


#' Read forest fire emission from excel
#' and transform it (required)
#'
#' @return emission tibble
forest.get_emission <- function(){

  e <- data.sheet_to_emissions(sheet_name="Forest Fire")
  g <- data.bps_map() %>%
    sf::st_make_valid()

  # Using province as id
  e %>%
    left_join(g %>% select(id, province)) %>%
    group_by(id=province, poll, unit, year) %>%
    summarise_at("emission", sum, na.rm=T)
}



forest.get_date_weight <- function(){
  readRDS("sectors/forest/forest_date_weight.RDS")
}
