require(dplyr)
require(tidyr)
require(stringr)
require(magrittr)

source('bps.data.R')

d.industri <- bps.industri()
d.susunas <- bps.susunas()
d.susenas <- bps.susenas()

# Testing some combinations for validation --------------------------------


d.industri %>%
  group_by(code, province_en) %>%
  summarise(value=sum(value)) %>%
  filter(code %in% c("ECLKGU17","ECLKGE17", "ECBKGU17")) %>%
  spread(code, value) %>%
  rename(total_ton=ECLKGU17, power_ton=ECLKGE17, briquette_kg=ECBKGU17) %>%
  write.csv(file.path("data/bps/industri/result/coal.csv"))

d.industri %>%
  mutate(name_full=paste(item,sector,unit,sep="_")) %>%
  group_by(name_full, province_en) %>%
  summarise(value=sum(value)) %>%
  spread(name_full, value) %>%
  write.csv(file.path("data/bps/industri/result/industri.csv"))

d.industri %>%
  filter(code=="ECLKGU17", value>0) %>%
  pull(value) %>%
  hist()

# Indonesia: 100M tonne = 1e8
d.industri %>%
  filter(code=="ECLKGU17", value>0, value<10)


# Number of cars
d.susunas %>%
  group_by(province, kab_id, kab) %>%
  summarise(car=sum((car==1)*weight, na.rm=T),
            motorbike=sum((motorbike==1)*weight, na.rm=T)) %>%
  write.csv("data/bps/susunas/cars.csv", row.names = F)



# Fuel consumption for cars
d.susenas %>%
  filter(usage %in% c("motor_vehicle")) %>%
  group_by(province, commodity_en, unit, timespan) %>%
  summarise(
    # value=weighted.mean(multiple_uses, weighing_for_population, na.rm=T),
    value=weighted.mean(multiple_uses, weighing_for_household, na.rm=T)
  ) %>%
  tidyr::pivot_wider(id_cols=c(province, unit, timespan),
                     names_from=commodity_en,
                     values_from=value) %>%
  write.csv("data/bps/susenas/car_consumption.csv", row.names = F)



# Fuel consumption for others
d.susenas %>%
  filter(usage %in% c("generator","other")) %>%
  group_by(province, commodity=paste(commodity_en, unit, sep="_"), timespan) %>%
  summarise(
    # value=weighted.mean(multiple_uses, weighing_for_population, na.rm=T),
    value=weighted.mean(multiple_uses, weighing_for_household, na.rm=T)
  ) %>%
  tidyr::pivot_wider(id_cols=c(province, timespan),
                     names_from=commodity,
                     values_from=value,
                     values_fill=0) %>%
  write.csv("data/bps/susenas/other_consumption.csv", row.names = F)
