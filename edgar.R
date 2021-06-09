#' Convert sector from Jakarta emission inventory to EDGAR sector(s)
#'
#' @param sector
#'
#' @return
#' @export
#'
#' @examples
edgar.local_to_edgar_sectors <- function(sector){
  # Table available here:
  # https://www.nature.com/articles/s41597-020-0462-2/tables/6

  list(
    power=c("ENE"),
    transport=c("TRO_noRES"),
    agroob=c("AWB"),
    shipping=c("TNR_Ship"),
    air=c("TNR_Aviation_LTO"),
    forest=NULL,
    comres=c("RCO")
  )[[sector]]
}

edgar.download_emissions <- function(edgar_sector, poll){

  # edgar_sectors <- edgar.local_to_edgar_sectors(sector)
  eixport::get_edgar(dataset = "v50_AP",
            pol = poll,
            sector = edgar_sector,
            year = 2015,
            destpath = "data/edgar",
            type = "nc",
            ask = F)

  fs <- list.files("data/edgar", "*.zip", full.names=T)
  lapply(fs, unzip, exdir="data/edgar")
  file.remove(fs)
}

edgar.emission <- function(sector, poll){

  edgar_sectors <- edgar.local_to_edgar_sectors(sector)
  grid <- data.grid.edgar()
  rs <- lapply(edgar_sectors, function(s){
    f <- file.path("data/edgar", sprintf("v50_%s_2015_%s.0.1x0.1.nc", poll, s))
    if(!file.exists(f)){
      edgar.download_emissions(edgar_sector=s, poll)
    }
    r <- raster::raster(f) %>%
      raster::crop(grid)
  })

  rs <- do.call("sum",rs)
  # Convert
  # Edgar: kg substance /m2 /s
  # Ours: tonnes / year (/ km2)
  rs <- rs * 1e6 / 1000 *3600*24*365
  names(rs) <- sprintf("EDGAR: %s", paste(edgar_sectors, sep="", collapse=", "))


  names(rs) <- sprintf("EDGAR: %s", paste(edgar_sectors, sep="", collapse=", "))
  return(rs)
}
