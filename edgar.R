#' Convert sector from Jakarta emission inventory to EDGAR sector(s)
#'
#' @param sector
#'
#' @return
#' @export
#'
#' @examples
edgar.local_to_edgar_sectors <- function(sector=NULL, year = 2015){
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
    # ,
    # # other_ind=c('NMM', 'CHE', 'IRO', 'PRU_SOL', 'FOO_PAP'),
    # other_ind_nmm=c('NMM'),
    # other_ind_che=c('CHE'),
    # other_ind_iro=c('IRO'),
    # other_ind_prusol=c('PRU_SOL'),
    # other_ind_foopap=c('FOO_PAP'),
    # other_ind_nfe=c('NFE')
  )

  if(is.null(sector)){
    return(corr)
  } else if(sector == 'transport' & year > 2015){
    return('TRO')
  }else{
    return(corr[sector] %>% unlist(use.names=F))
  }
}

edgar.download_emissions <- function(edgar_sector, poll, year = 2015){

  if(year > 2015){
    edgar_url <- 'https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v81_FT2022_AP/{poll}/{edgar_sector}/flx_nc/v8.1_FT2022_AP_{poll}_{year}_{edgar_sector}_flx_nc.zip'

    if(poll == 'CH4'){
      edgar_url <- 'https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/CH4/{edgar_sector}/flx_nc/v8.0_FT2022_GHG_CH4_{year}_{edgar_sector}_flx_nc.zip'
    }

    download.file(glue(edgar_url),
                  destfile = file.path("data/edgar", sprintf("%s.zip", edgar_sector)),
                  mode = "wb")
  } else {
    edgar_sectors <- edgar.local_to_edgar_sectors(sector)
    eixport::get_edgar(dataset = ifelse(tolower(poll)=="ch4", "v80ghg", "v61_AP"),
              pol = poll,
              sector = edgar_sector,
              year = 2015,
              destpath = "data/edgar",
              type = "nc",
              ask = F)
  }

  fs <- list.files("data/edgar", "*.zip", full.names=T)
  lapply(fs, unzip, exdir="data/edgar")
  file.remove(fs)
}

edgar.emission <- function(sector, poll, year = 2015){

  edgar_sectors <- edgar.local_to_edgar_sectors(sector, year = year)
  grid <- data.grid.edgar()
  rs <- lapply(edgar_sectors, function(s){
    if(year == 2015){
      f <- file.path("data/edgar", sprintf("v50_%s_2015_%s.0.1x0.1.nc", poll, s))
      # GHG (i.e. CH4) is only available by month
      f_months <- file.path("data/edgar", sprintf("v50_%s_2015_%d_%s.0.1x0.1.nc", poll, seq(1,12), s))

      if(!file.exists(f) & !all(file.exists(f_months))){
        edgar.download_emissions(edgar_sector=s, poll, year = year)
      }

      if(all(file.exists(f_months))){
        r <- lapply(f_months,
                    function(f){raster::raster(f) %>% raster::crop(grid)}) %>%
          do.call("mean", .)
      }else{
        r <- raster::raster(f) %>%
          raster::crop(grid)
      }

    } else if(year > 2015){
      if(poll == 'CH4'){
        f <- file.path('data/edgar', sprintf("v8.0_FT2022_GHG_CH4_2019_%s_flx.nc", s))
      } else {
        f <- file.path('data/edgar', sprintf("v8.1_FT2022_AP_%s_2019_%s_flx.nc", poll, s))
      }

      if(!file.exists(f)){
        edgar.download_emissions(edgar_sector=s, poll, year = year)
      }

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
