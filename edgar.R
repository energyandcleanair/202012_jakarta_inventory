#' Convert sector from Jakarta emission inventory to EDGAR sector(s)
#'
#' @param sector
#'
#' @return
#' @export
#'
#' @examples
edgar.local_to_edgar_sectors <- function(sector){
  list(
    power=c("ENE"),
    transport=c("TRO_noRES")
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
  # Ours: tonnes / m2 / year
  rs <- rs * raster::area(rs) *1e6/1000*3600*24*365
  names(rs) <- sprintf("EDGAR: %s", paste(edgar_sectors, sep="", collapse=", "))

  return(rs)
}
