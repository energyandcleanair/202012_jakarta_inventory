data.region_ids <- function(gadm=T, bps=T){
  ids <- c()
  if(gadm) ids <- c(ids, c("IDN.4_1", "IDN.7_1", "IDN.9_1", "IDN.10_1", "IDN.17_1")
  if(bps) ids <- c(ids, data.emission_transport() %>% .$region_id)

  return(ids)
}

#' Build region shapefiles to be used later on.
#'
#' @return
#' @export
#'
#' @examples
data.gadm <- function(){
  rbind(
    sf::read_sf(file.path("data","boundaries","gadm36_IDN_1.shp")) %>%
      filter(GID_1 %in% data.region_ids()) %>%
      select(id=GID_1, name=NAME_1, geometry),
    sf::read_sf(file.path("data","boundaries","gadm36_IDN_2.shp")) %>%
      filter(GID_1 %in% data.region_ids()) %>%
      select(id=GID_2, name=NAME_2, geometry)
  )
}

data.bps_map <- function(){
  sf::read_sf(file.path("data","boundaries","bps","idn_admbnda_adm2_bps_20200401.shp")) %>%
    select(id=ADM2_PCODE, name=ADM2_EN, geometry) %>%
    filter(id %in% data.region_ids())
}


data.grid.edgar <- function(){
  g <- data.gadm()
  extent <- sf::st_bbox(g)

  raster::raster("data/edgar/ENE/v50_NOx_2015_1_ENE.0.1x0.1.nc") %>%
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

data.grid <- function(res_deg, extent=NULL){

  if(is.null(extent)){
    extent <- data.gadm()
  }

  raster::raster(raster::extent(extent),
                         resolution=res_deg,
                         crs=raster::crs(extent)) %>%
    raster::raster()
}




