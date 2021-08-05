
#' Build support for landfill sector
#' UNSUCCESFUL: Using OSM data. Not enough points
landfill.build_support_osm <- function(){

  library(osmdata)

  # Get bps map
  g <- data.bps_map()
  as.vector(sf::st_bbox(g))

  # Use osm data
  landfills <- opq(bbox=as.vector(sf::st_bbox(g))) %>%
    add_osm_feature(key = "landuse", value = "landfill") %>%
    osmdata_sf()

  landfills.2 <- opq(bbox=as.vector(sf::st_bbox(g)),
                     timeout = 300) %>%
    add_osm_feature(key = "name", value = "TPA", value_exact = F) %>%
    osmdata_sf()

  landfills.3 <- opq(bbox=as.vector(sf::st_bbox(g)),
                     timeout = 300) %>%
    add_osm_feature(key = "name", value = "Tempat Pemrosesan Akhir", value_exact = F) %>%
    osmdata_sf()

  # Attaching region_id (from BPS) to roads
  landfills <- bind_rows(
    landfills$osm_points,
    landfills.2$osm_points,
    landfills.3$osm_points
    ) %>%
    dplyr::distinct(osm_id, .keep_all=T) %>%
    dplyr::select(osm_id) %>%
    sf::st_join(g, left=F) %>%
    select(osm_id, id, geometry)

  landfills$weight <- 1

  sf::st_as_sf(landfills) %>%
    sf::write_sf("data/landfill/landfill_support.shp")

  return(stations)
}

#' Build support for landfill sector
#' Using SIPSN. Probably ok, but we received
#' Prof. Didin data in the meantime.
landfill.build_support_sipsn <- function(){
  # https://sipsn.menlhk.go.id/sipsn/public/home/peta

  library(jsonlite)
  d <- jsonlite::fromJSON("sectors/landfill/tpa.json")$markers %>%
    as.data.frame()

  names(d) <- c("name", "latitude", "longitude", "url", "type", "id")

  f <- "sectors/landfill/tpa.gpkg"
  if(file.exists(f)){
    file.remove(f)
  }

  sf::st_as_sf(d, coords=c("longitude","latitude")) %>%
    sf::st_set_crs(4326) %>%
    sf::st_write(f)
}


landfill.get_support <- function(){
  # Directly use Excelsheet by Porf. Didin
  s <- readxl::read_xlsx("sectors/landfill/landfill-inventory-2021.xlsx",
                         sheet=1,
                         skip = 8)

  names(s) <- c("province","city","landfill","lat","lon","emission")
  s <- s %>% tidyr::fill(province,city)

  # Spread equally across landfills
  s %>% left_join(
    s %>% group_by(province, city) %>%
      summarise(city_emission=sum(emission, na.rm=T),
                city_nlandfill=n())
  ) %>%
    mutate(emission=city_emission/city_nlandfill) %>%
    select(-c(city_emission, city_nlandfill))

  # Remove subtotals
  s <- s %>% filter(!grepl("total",province))

  # Format to be similar to other
  s$weight <- s$emission
  s$id <- paste(s$province, s$city, s$landfill, sep="_")
  s <- s %>% sf::st_as_sf(coords=c("lon","lat"))
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


landfill.get_emission <- function(){
  data.sheet_to_emissions(sheet_name="Methane-landfill")
}
