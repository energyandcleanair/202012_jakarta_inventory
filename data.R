data.region_ids <- function(gadm=T, bps=T){
  ids <- c()
  if(gadm) ids <- c(ids, c("IDN.4_1", "IDN.7_1", "IDN.9_1", "IDN.10_1", "IDN.17_1"))
  if(bps) ids <- c(ids,
                   read_csv("data/region_lookup.csv") %>% pull(ADM2_PCODE) %>% unique()
                   )

  return(unique(ids))
}

data.sector_name <- function(sector=NULL){
  corr <-  list(
    power="Power generation",
    transport="Road transportation",
    agroob="Agro-residual open burning",
    shipping="Harbour",
    aviation="Air transportation (LTO)",
    forest="Forest fire",
    comres="Residential and commercial",
    industry="Manufacturing industry",
    landfill="Landfill (methane)",
    solidwaste="Solid waste open burning",
    gasdist="Fugitive emissions from fuels",
    livestock="Livestock"
  )

  if(is.null(sector)){
    return(corr)
  }else{
    return(corr[sector] %>% unlist(use.names=F))
  }
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
      dplyr::select(id=GID_1, name=NAME_1, geometry) %>%
      mutate(level=1),
    sf::read_sf(file.path("data","boundaries","gadm","gadm36_IDN_2.shp")) %>%
      filter(GID_1 %in% data.region_ids()) %>%
      dplyr::select(id=GID_2, name=NAME_2, geometry)  %>%
      mutate(level=2)
  )
}

data.bps_map <- function(buffer_km=0){

  file_cache <- sprintf("data/boundaries/bps/geom_%skm.RDS",buffer_km)
  if(file.exists(file_cache)) return(readRDS(file_cache))

  g <- sf::read_sf(file.path("data","boundaries","bps","idn_admbnda_adm2_bps_20200401.shp")) %>%
    dplyr::select(id=ADM2_PCODE, name=ADM2_EN, province=ADM1_EN, geometry) %>%
    sf::st_make_valid()

  # We want neighbour regions as well so that the buffering
  # only goes into the sea, and does not extend on other regions
  # So we can't filter yet with data.region_ids()
  bbox <- sf::st_bbox(data.grid.d02() %>% raster::projectExtent(4326))
  g <- g %>% sf::st_crop(bbox + c(-1,-1,1,1))

  if(buffer_km>0){
    g_coast <- cartomisc::regional_seas(g %>% sf::st_transform(3857),
                                        group="id",
                                        dist=buffer_km*1000) %>%
      left_join(g %>% as.data.frame() %>% dplyr::select(id, name, province))

    g <-  bind_rows(g %>% sf::st_transform(3857),
                    g_coast) %>%
      # st_snap(x = ., y = ., tolerance = 0.0001) %>% # for sliver polygons but too slow
      group_by(id, name, province) %>%
      summarise() %>%
      sf::st_transform(sf::st_crs(g)) %>%
      sf::st_make_valid()
  }

  # Now we can limit to region of interest
  ids <- data.region_ids()
  g <- g %>%
    filter(id %in% ids)

  saveRDS(g, file_cache)
  return(g)
}


data.createedgar <- function(){
  g <- data.bps_map()
  extent <- sf::st_bbox(g)

  r <- raster::raster("data/edgar/v50_NOx_2015_ENE.0.1x0.1.nc") %>%
    raster::raster() %>%
    raster::crop(extent)

  raster::writeRaster(r, "data/edgar.grid.tif", overwrite=T)
}

data.grid.edgar <- function(){
  raster::raster("data/edgar.grid.tif")
}


data.created02 <- function(){
  f <- "/Volumes/ext1/studies/202012_jakarta_emissions/meteosim/topdown_d02.nc"
  nc <- ncdf4::nc_open(f)
  x <- ncvar_get(nc, "x")
  y <- ncvar_get(nc, "y")
  r <- raster::rasterFromXYZ(tidyr::crossing(x,y) %>% mutate(z=1),
                             crs="+proj=merc +a=6370000.0 +b=6370000.0 +lat_ts=-4.0 +lon_0=108.35 +units=m")
  raster::writeRaster(r, "data/d02.grid.tif", overwrite=T)
}


data.grid.d02 <- function(){
  # g <- data.gadm()
  raster::raster("data/d02.grid.tif") %>% raster::raster()
  # extent <- g %>%
  #   sf::st_transform(raster::projection(grid)) %>%
  #   sf::st_bbox()

  # grid %>%
  #   raster::crop(extent)
}


data.created03 <- function(){
  # Any file from METEOSIM d03 dataset
  f <- "/Volumes/ext1/studies/202012_jakarta_emissions/meteosim/topdown_d03.nc"
  nc <- ncdf4::nc_open(f)
  x <- ncvar_get(nc, "x")
  y <- ncvar_get(nc, "y")
  # crs <- utils.proj4string_from_nc(f)
  # crs <- "+proj=merc +a=6370000.0 +b=6370000.0 +lat_ts=-4.0 +lon_0=108.35 +units=m"
  r <- raster::rasterFromXYZ(tidyr::crossing(x,y) %>% mutate(z=1),
                        crs="+proj=merc +a=6370000.0 +b=6370000.0 +lat_ts=-4.0 +lon_0=108.35 +units=m")
  # raster::crs(r) <- crs
  raster::writeRaster(r, "data/d03.grid.tif", overwrite=T)
}

data.created02 <- function(){
  f <- "/Volumes/ext1/studies/202012_jakarta_emissions/meteosim/CCTM_d03_CMAQv521_jakarta_disp-1km_EXP2_2019010112.nc"
  nc <- ncdf4::nc_open(f)
  x <- ncvar_get(nc, "X")
  y <- ncvar_get(nc, "Y")
  # crs <- utils.proj4string_from_nc(f)
  # crs <- "+proj=merc +a=6370000.0 +b=6370000.0 +lat_ts=-4.0 +lon_0=108.35 +units=m"
  r <- raster::rasterFromXYZ(tidyr::crossing(x,y) %>% mutate(z=1),
                             crs="+proj=merc +a=6370000.0 +b=6370000.0 +lat_ts=-4.0 +lon_0=108.35 +units=m")
  # raster::crs(r) <- crs
  raster::writeRaster(r, "data/d03.grid.tif", overwrite=T)
}

data.grid.d03 <- function(){
  # g <- data.gadm()
  raster::raster("data/d03.grid.tif") %>% raster::raster()
  # extent <- g %>%
  #   sf::st_transform(raster::projection(grid)) %>%
  #   sf::st_bbox()

  # grid %>%
  #   raster::crop(extent)
}


data.grid.d04 <- function(){
  # g <- data.gadm()
  raster::raster("data/d04.grid.tif") %>% raster::raster()
  # extent <- g %>%
  #   sf::st_transform(raster::projection(grid)) %>%
  #   sf::st_bbox()
  #
  # grid %>%
  #   raster::crop(extent)
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

data.build_urban_rural <- function(){

  g <- data.bps_map() %>% sf::st_make_valid()
  # Data shared by Danny
  f.org <- "data/landuse/Indonesia boundary - Desa kelurahan.gpkg"

  sf::read_sf(f.org) %>%
    mutate(type=ifelse(STATUS_KOT %in% c("Kota","kota","kot"), "urban", "rural")) %>%
    dplyr::select(type) %>%
    sf::st_make_valid() %>%
    st_zm() %>%
    sf::st_crop(sf::st_bbox(g)) %>%
    sf::write_sf("data/landuse/urban_rural.gpkg")
}

data.urban_rural <- function(){
  sf::read_sf("data/landuse/urban_rural.gpkg")
}
