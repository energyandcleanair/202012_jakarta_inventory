data.region_ids <- function(gadm=T, bps=T){
  ids <- c()
  if(gadm) ids <- c(ids, c("IDN.4_1", "IDN.7_1", "IDN.9_1", "IDN.10_1", "IDN.17_1"))
  if(bps) ids <- c(ids,
                   read_csv("data/region_lookup.csv") %>% pull(ADM2_PCODE) %>% unique()
                   )

  return(unique(ids))
}

#' Build region shapefiles to be used later on.
#'
#' @return
#' @export
#'
#' @examples
data.gadm <- function(){
  rbind(
    sf::read_sf(file.path("data","boundaries","gadm","gadm36_IDN_1.shp")) %>%
      filter(GID_1 %in% data.region_ids()) %>%
      select(id=GID_1, name=NAME_1, geometry),
    sf::read_sf(file.path("data","boundaries","gadm","gadm36_IDN_2.shp")) %>%
      filter(GID_1 %in% data.region_ids()) %>%
      select(id=GID_2, name=NAME_2, geometry)
  )
}

data.bps_map <- function(){
  sf::read_sf(file.path("data","boundaries","bps","idn_admbnda_adm2_bps_20200401.shp")) %>%
    dplyr::select(id=ADM2_PCODE, name=ADM2_EN, province=ADM1_EN, geometry) %>%
    filter(id %in% data.region_ids())
}


data.grid.edgar <- function(){
  g <- data.bps_map()
  extent <- sf::st_bbox(g)

  raster::raster("data/edgar/v50_NOx_2015_ENE.0.1x0.1.nc") %>%
    raster::raster() %>%
    raster::crop(extent)
}


data.grid.d04 <- function(){
  g <- data.gadm()
  grid <- raster::raster("data/d04.grid.tif") %>% raster::raster()
  extent <- g %>%
    sf::st_transform(raster::projection(grid)) %>%
    sf::st_bbox()

  grid %>%
    raster::crop(extent)
}

#' If res_deg is specified -> EPSG:4326,
#' if res_m is specified, we use projection from MeteoSim sample data (units=m)
#'
#' @param res_deg
#' @param res_m
#' @param extent
#'
#' @return
#' @export
#'
#' @examples
data.grid <- function(res_deg=NULL, res_m=NULL, extent=NULL){

  if(is.null(extent)){
    extent <- data.bps_map()
  }

  if(!is.null(res_m)){
    crs <- sf::st_crs(data.grid.d04())
    extent <- sf::st_transform(extent, crs)
    grid <- raster::raster(raster::extent(extent),
                           resolution=res_m,
                           crs=raster::crs(extent)) %>%
      raster::raster()
  }

  if(!is.null(res_deg)){
    grid <- raster::raster(raster::extent(extent),
                           resolution=res_deg,
                           crs=raster::crs(extent)) %>%
      raster::raster()
  }

  return(grid)
}

data.sheet_to_emissions <- function(sheet_name){

  s <- readxl::read_xlsx("data/Emission-2019-compilation-send.xlsx",
                         sheet=sheet_name,
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

data.land_use <- function(type){

  # Available types are
  en_to_org <- list(
    "Primary Dryland Forest"= "Hutan Lahan Kering Primer",
    "Secondary Dryland Forest"= "Hutan Lahan Kering Sekunder",
    "Open Land"= "Tanah Terbuka",
    "Plant Forest"= "Hutan Tanaman",
    "Scrub"= "Belukar",
    "Plantation"= "Perkebunan",
    "Settlement"= "Pemukiman",
    "Secondary Mangrove Forest"= "Hutan Mangrove Sekunder",
    "Swamp Scrub"= "Belukar Rawa",
    "Dryland Farming"= "Pertanian Lahan Kering",
    "Mixed Dry Land Farming"= "Pertanian Lahan Kering Campur",
    "rice field"= "Sawah",
    "Pond"= "Tambak",
    "Airport / Port"= "Bandara / Pelabuhan",
    "Mining"= "Pertambangan",
    "Swamp"= "Rawa",
    "Water body"= "Badan Air",
    "Primary Mangrove Forest"= "Hutan Mangrove Primer",
    "Savanna / Grasslands"= "Savana / Padang rumput",
    "Secondary Swamp Forest"= "Hutan Rawa Sekunder",
    "Transmigration"= "Transmigrasi"
  )



  sector_to_type_en <- list(
    comres=c("Settlement"),
    agroob=c("Plantation", "Dryland Farming", "Mixed Dry Land Farming", "rice field"),
    forest=names(en_to_org)[grepl("Forest", names(en_to_org))]
  )

  lu <- sf::read_sf("data/landuse/land_cover_2019.geojson")

  if(!is.null(type)){
    legendas <- unlist(en_to_org[sector_to_type_en[[type]]], use.names = F)
    lu <- lu %>% filter(Legenda %in% legendas)
  }

  return( lu %>% filter(sf::st_geometry_type(geometry) %in% c("MULTIPOLYGON","POLYGON")))
}
