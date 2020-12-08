require(tidyverse)
require(sf)
require(ggplot2)

source('./utils.R')



#' Generate emission data
#'
#' @param regulation: either 2008 or 2019
#' @param type: either old or new
#'
#' @return dataframe with coordinates, time, poll, and emission_mg_hour columns
#' @export
#'
#' @examples
build_emission_power <- function(regulation, type, date_from="2018-01-01", date_to="2018-12-31 23:00"){

  # Reading data ------------------------------------------------------------

  # Power plants
  plants <- read.csv("data/power/global_power_plant_database_sea.csv") %>%
    filter(country=="IDN") %>%#CHECK: what about other SEA countries?
    mutate(fuel=tolower(primary_fuel)) %>%
    filter(fuel %in% c("coal","gas"))

  # Emission factors
  power_ef <- read.csv("data/power/power_ef.csv") %>%
    filter(Unit=="mg/Nm3") %>%
    mutate(Hg=as.numeric(Hg)) %>%
    tidyr::pivot_longer(cols=c(SO2, NOx, PM, Hg),
                        names_to="poll",
                        values_to="emission_mg_nm3") %>%
    rename(unit=Unit)

  power_ef[grepl("Coal",power_ef$Variable),"fuel"] <- "coal"
  power_ef[grepl("Gas",power_ef$Variable),"fuel"] <- "gas"
  power_ef[grepl("2008",power_ef$Note),"regulation"] <- 2008
  power_ef[grepl("2019",power_ef$Note),"regulation"] <- 2019
  power_ef[grepl("Old",power_ef$Note),"type"] <- "old"
  power_ef[grepl("New",power_ef$Note),"type"] <- "new"
  power_ef$scenario <- paste(power_ef$type,power_ef$regulation,sep="_")

  # Filter according to parameters
  power_ef <- power_ef %>%
    filter(type==!!type,
           regulation==!!regulation)

  # Attribute emission factors to plants
  plants.ef <- plants %>%
    dplyr::select(name, capacity_mw, latitude, longitude, fuel, estimated_generation_gwh, country=country_long) %>%
    left_join(power_ef %>%
                select(Activity, country=Geographical.area, fuel, regulation,
                       poll, emission_mg_nm3, unit, scenario))


  # Attribute power generation to plants
  # in proportion of their capacity
  # TODO Attribute per province instead
  power_activity <- read.csv("data/power/power_activity.csv")
  power_generation_gwh <- sum(as.numeric(gsub(",","",power_activity$Value)))

  plants.ef$generation_gwh_year <- plants.ef$capacity_mw / sum(plants.ef$capacity_mw) * power_generation_gwh
  plants.ef$year <- 2018

  # Spread power generation within year
  t <- seq(as.POSIXct(date_from, tz="Asia/Jakarta"), as.POSIXct(date_to, tz="Asia/Jakarta"), by="hour")
  t.joiner <- tibble(date=t, frac=1/length(t))
  plants.ef.d <- plants.ef %>% tidyr::crossing(t.joiner)
  plants.ef.d <- plants.ef.d %>% mutate(generation_gwh_hour=generation_gwh_year*frac)


  # GWh to NM3
  # Using: https://www.vgb.org/vgbmultimedia/rp338_flue_gas-p-5560.pdf
  # "For solid fuels, the fixed factor is acceptable for commercially traded hard coal and biomass
  # with a moisture content below 25% by mass"
  mj_per_gwh <- 3600000
  thermal_efficiency <- tibble(fuel=c("gas","coal"),
                               efficiency=c(0.6, 0.4), #TODO get decent numbers
                               nm3_per_mj=c(0.240, 0.2566)) #TODO This is volume at 0C, check if same definition in EF regulation
  plants.ef.d <- plants.ef.d %>% left_join(thermal_efficiency) %>%
    mutate(emission_mg_year=generation_gwh_year*mj_per_gwh*nm3_per_mj*emission_mg_nm3,
           emission_mg_hour=generation_gwh_hour*mj_per_gwh*nm3_per_mj*emission_mg_nm3,
           emission_g_s=emission_mg_hour/1000/3600)

  return(plants.ef.d %>%
           select(longitude,latitude,poll,date, emission_g_s))
}
