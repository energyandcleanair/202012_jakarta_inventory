#' Convert sector from Jakarta emission inventory to EDGAR sector(s)
#'
#' @param sector
#'
#' @return
#' @export
#'
#' @examples
edgar.local_to_edgar_sectors <- function(sector=NULL){
  # Table available here:
  # https://www.nature.com/articles/s41597-020-0462-2/tables/6

  corr <-  list(
    power=c("ENE"),
    transport=c("TRO_noRES"),
    agroob=c("AWB"),
    shipping=c("TNR_Ship"),
    aviation=c("TNR_Aviation_LTO"),
    comres=c("RCO"),
    industry=c("IND"),
    landfill=c("SWD_LDF"),
    solidwaste=c("SWD_INC"),
    forest=NULL,
    gasdist=NULL,
    livestock=NULL
  )

  if(is.null(sector)){
    return(corr)
  }else{
    return(corr[sector] %>% unlist(use.names=F))
  }
}

edgar.download_emissions <- function(edgar_sector, poll){

  # edgar_sectors <- edgar.local_to_edgar_sectors(sector)
  eixport::get_edgar(dataset = ifelse(tolower(poll)=="ch4", "v50_GHG", "v50_AP"),
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
    # GHG (i.e. CH4) is only available by month
    f_months <- file.path("data/edgar", sprintf("v50_%s_2015_%d_%s.0.1x0.1.nc", poll, seq(1,12), s))

    if(!file.exists(f) & !all(file.exists(f_months))){
      edgar.download_emissions(edgar_sector=s, poll)
    }

    if(all(file.exists(f_months))){
      r <- lapply(f_months,
                  function(f){raster::raster(f) %>% raster::crop(grid)}) %>%
        do.call("mean", .)
    }else{
      r <- raster::raster(f) %>%
        raster::crop(grid)
    }
  })

  rs <- do.call("sum",rs)
  # Convert
  # Edgar: kg substance /m2 /s
  # Ours: tonnes / year (/ km2)
  rs <- rs * 1e6 / 1000 *3600*24*365 * raster::area(rs)
  names(rs) <- sprintf("EDGAR: %s", paste(edgar_sectors, sep="", collapse=", "))

  return(rs)
}
