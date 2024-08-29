library(glue)
library(terra)
library(sf)
library(lubridate)
library(terra)
library(pbapply)

source('data.R')
source('edgar.R')


# EDGAR -------------------------------------------------------------------

polls <- c("NOx","SO2","BC","OC","PM","NMVOC","NH3","CH4","CO")
polls.edgar <- gsub("PM", "PM10", polls)
sectors <- names(edgar.local_to_edgar_sectors())
g <- data.bps_map(buffer_km=50) %>%
  group_by(province) %>%
  summarise()

# download EDGAR emissions
lapply(edgar_sectors, function(sector){
  edgar_sector <- edgar.local_to_edgar_sectors(sector, year = 2019)
  download_edgar <- lapply(polls.edgar, function(poll){
    tryCatch({
      # browser()
      edgar.download_emissions(edgar_sector, poll, year = 2019)},
      error=function(e){
        return(data.grid.edgar() %>% setValues(0))
      })
  })
})

edgar_2019 <- purrr::map(polls.edgar, function(poll){
  purrr::map(sectors, function(sector){
    tryCatch({edgar.emission(sector = sector, poll = poll, year = 2019)},
             error = function(e){
               return(data.grid.edgar() %>% setValues(0))
             })
  }) %>%
    do.call(raster::stack, .) %>%
    `names<-`(sectors) %>%
    raster::extract(g, fun = sum, na.rm = T) %>%
    as.data.frame() %>%
    mutate(province = g$province,
           poll = poll) %>%
    tidyr::pivot_longer(cols = -c("poll", "province"),
                        names_to = "sector",
                        values_to = "emission_tonne")
}) %>%
  do.call(bind_rows, .) %>%
  mutate(source = 'EDGAR_2019')

edgar_2015 <- purrr::map(polls.edgar, function(poll){
  purrr::map(sectors, function(sector){
    tryCatch({edgar.emission(sector = sector, poll = poll)},
             error = function(e){
               return(data.grid.edgar() %>% setValues(0))
             })
  }) %>%
    do.call(raster::stack, .) %>%
    `names<-`(sectors) %>%
    raster::extract(g, fun = sum, na.rm = T) %>%
    as.data.frame() %>%
    mutate(province = g$province,
           poll = poll) %>%
    tidyr::pivot_longer(cols = -c("poll","province"),
                        names_to = "sector",
                        values_to = "emission_tonne")
}) %>%
  do.call(bind_rows, .) %>%
  mutate(source = 'EDGAR_2015')


# Prof's ------------------------------------------------------------------

prof_data <- read.csv('data/prof_emission.csv', encoding = 'UTF-8') %>%
  # rename(province = X.U.FEFF.province) %>%
  pivot_longer(cols = 3:11, names_to = 'poll', values_to = 'emission_tonne') %>%
  mutate(source = 'CREA_didin')

prof_data_rec <- read.csv('data/prof_emission_rec.csv', encoding = 'UTF-8') %>%
  rename(province = X.U.FEFF.province) %>%
  pivot_longer(cols = 3:11, names_to = 'poll', values_to = 'emission_tonne') %>%
  mutate(source = 'CREA_didin_rec')

prof_data2 <- read.csv('data/JKTSA_manualcompile.csv') %>%
  select(province = Province, poll = Pollutant, sector = Sector_benchmark,
         emission_tonne = Emission_2019_ton) %>%
  group_by(province, poll, sector) %>%
  summarise(emission_tonne = sum(emission_tonne, na.rm = T)) %>%
  mutate(source = 'manual_compile',
         province = case_when(province == 'DKI Jakarta' ~ 'Dki Jakarta',
                              T ~ province)) %>%
  ungroup


# CEDS --------------------------------------------------------------------

g <- data.bps_map(buffer_km = 50) %>%
  group_by(province) %>%
  summarise() %>%
  rowid_to_column('ID')

ceds_files <- list.files('data/ceds/', full.names = TRUE, pattern = '\\.nc$')

ceds_data <- lapply(ceds_files, function(file){
  print(file)
  pollutant <- basename(file) %>% str_split_i(pattern = '-', 1)

  multiplier <- sapply(1:12, function(x) days_in_month(x) * 24 * 3600) %>%
    rep(each = 8) # 8 sectors, 12 months to seconds per month

  r <- rast(file)
  r <- r * multiplier * cellSize(r) # convert kg m-2 s-1 to km month-1

  v <- extract(r, vect(g), fun = 'sum', na.rm = TRUE) %>% mutate(poll = pollutant)
}) %>% bind_rows()

ceds <- ceds_data %>% pivot_longer(cols = -c(ID, poll),
                                   names_to = 'file',
                                   values_to = 'emission_tonne') %>%
  filter(!is.na(emission_tonne),
         poll != 'CO2') %>%
  mutate(sector_code = sub(".*=(\\d+)_.*", "\\1", file),
         sector = case_when(sector_code == 0 ~ 'agroob',
                            sector_code == 1 ~ 'power',
                            sector_code == 2 ~ 'industry',
                            sector_code == 3 ~ 'transport',
                            sector_code == 4 ~ 'comres',
                            sector_code == 5 ~ 'other',
                            sector_code == 6 ~ 'solidwaste',
                            sector_code == 7 ~ 'shipping')) %>%
  group_by(ID, poll, sector) %>%
  summarise(emission_tonne = sum(emission_tonne)/1000,
            source = 'CEDS') %>%
  ungroup %>%
  left_join(g %>% st_drop_geometry(), by = 'ID') %>%
  filter(sector != 'other') %>%
  select(-ID)


# Plot --------------------------------------------------------------------

e_all <- bind_rows(edgar_2019, ceds,
                   prof_data2 %>% filter(!poll %in% c('CO2', 'NMHC', 'N2O'),
                                         sector != '#N/A'))

e_all %>%
  as.data.frame() %>%
  group_by(poll, source, sector) %>%
  summarise_at("emission_tonne", sum, na.rm = T) %>%
  ggplot() +
  geom_bar(stat = "identity",
           aes(poll, emission_tonne, fill = source),
           position="dodge") +
  facet_wrap(~sector, scales="free_y") +
  rcrea::theme_crea() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_manual(name = "",
                    values = unname(rcrea::pal_crea[c("Dark.blue", "Blue", "Orange", "Red", "Green")])) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(subtitle = "All sectors - Comparison between CREA and EDGAR total emissions",
       y = "Tonne",
       x = NULL)
ggsave(sprintf("comparison/all_sectors.png"), width = 14, height = 10)


e_all %>%
  mutate(source=gsub(" \\(","\n\\(", source)) %>%
  as.data.frame() %>%
  ggplot() +
  geom_bar(stat = "identity",
           aes(x = source, y = emission_tonne, fill = sector)) +
  facet_wrap(~poll, scales = "free_y", ncol = 2) +
  rcrea::theme_crea() +
  scale_fill_manual(name = "", values = rev(c(unname(rcrea::pal_crea[seq_len(11)]), "darkorchid4"))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(subtitle = "All sectors - Comparison between CREA and EDGAR total emissions",
       y = "Tonne",
       x = NULL)
ggsave(sprintf("comparison/all_sectors_poll.png"), width = 10, height = 14)


e_all %>%
  mutate(source = gsub(" \\(","\n\\(", source)) %>%
  as.data.frame() %>%
  ggplot() +
  geom_bar(stat = "identity",
           aes(x = source, y = emission_tonne, fill = poll)) +
  facet_wrap(~sector, scales = "free_y", ncol = 2) +
  rcrea::theme_crea() +
  scale_fill_manual(name =" ", values = rev(c(unname(rcrea::pal_crea[seq_len(13)])))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(subtitle = "All sectors - Comparison between CREA and EDGAR total emissions",
       y = "Tonne",
       x = NULL)
ggsave(sprintf("comparison/all_poll_sectors.png"), width = 10, height = 14)


e_all %>%
  as.data.frame() %>%
  filter(poll != 'CO') %>%
  group_by(poll, source) %>%
  summarise_at("emission_tonne", sum, na.rm = T) %>%
  ggplot() +
  geom_bar(stat = "identity",
           aes(poll, emission_tonne, fill = source),
           position = "dodge") +
  rcrea::theme_crea() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_manual(name = "",
                    values = unname(rcrea::pal_crea[c("Dark.blue", "Blue", "Orange", "Red", "Green")])) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(subtitle = "All sectors - Comparison between CREA and EDGAR total emissions",
       y = "Tonne",
       x = NULL)
ggsave(sprintf("comparison/all_total.png"), width = 10, height = 5)


e_all %>%
  ggplot() +
  geom_bar(stat = "identity",
           aes(province, emission_tonne, fill = source),
           position = "dodge") +
  facet_wrap(~poll, scales = "free", ncol = 2) +
  rcrea::theme_crea() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_manual(name = "",
                    values = unname(rcrea::pal_crea[c("Dark.blue", "Blue", "Orange", "Red", "Green")])) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                     labels = scales::comma) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(subtitle = "All sectors - Comparison between CREA and EDGAR provincial emissions",
       y = "Tonne",
       x = NULL)
ggsave(sprintf("comparison/all_province.png"), width = 10, height = 10)


# Jakarta only ------------------------------------------------------------

vitals <- tibble(
  sector = c('power', 'industry', 'transport', 'comres'),
  'SO2' = c(1071, 2637, 493, 55),
  'NOx' = c(12244, 12183, 76793, 4848),
  'CO' = c(5252, 3738, 287317, 1864),
  'PM10' = c(660, 2989, 5113, 55),
  'PM25' = c(447, 2102, 5257, 36),
  'BC' = c(157, 799, 5048, 2),
  'NMVOC' = c(352, 1212, 198936, 1471)
) %>% pivot_longer(-sector, names_to = 'poll', values_to = 'emission_tonne') %>%
  mutate(source = 'vital_strategies_2018', province = 'Dki Jakarta')

e_all_jak <- bind_rows(e_edgar_2019, prof_data, e_edgar_2015, prof_data_rec, ceds, vitals) %>%
  filter(province == 'Dki Jakarta',
         sector %in% c('power', 'industry', 'transport', 'comres'))

e_all_jak %>%
  as.data.frame() %>%
  group_by(poll, source, sector) %>%
  summarise_at("emission_tonne", sum, na.rm = T) %>%
  ggplot() +
  geom_bar(stat = "identity",
           aes(poll, emission_tonne, fill = source),
           position = "dodge") +
  facet_wrap(~sector, scales = "free_y") +
  rcrea::theme_crea() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_manual(name = "",
                    values = unname(rcrea::pal_crea[c("Dark.blue", "Blue", "Orange", "Red", "Green", "#351c75")])) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(subtitle = "All sectors - Comparison between CREA and EDGAR total emissions, Jakarta",
       y = "Tonne",
       x = NULL)
ggsave(sprintf("comparison/all_sectors_jak.png"), width = 14, height = 10)


e_all_jak %>%
  mutate(source = gsub(" \\(","\n\\(", source)) %>%
  as.data.frame() %>%
  ggplot() +
  geom_bar(stat = "identity",
           aes(x = source, y = emission_tonne, fill = sector)) +
  facet_wrap(~poll, scales = "free_y", ncol = 2) +
  rcrea::theme_crea() +
  scale_fill_manual(name = "", values = rev(c(unname(rcrea::pal_crea[seq_len(11)]), "darkorchid4"))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(subtitle = "All sectors - Comparison between CREA and EDGAR total emissions, Jakarta",
       y = "Tonne",
       x = NULL)
ggsave(sprintf("comparison/all_sectors_poll_jak.png"), width = 10, height = 14)


e_all_jak %>%
  mutate(source=gsub(" \\(","\n\\(", source)) %>%
  as.data.frame() %>%
  ggplot() +
  geom_bar(stat = "identity",
           aes(x = source, y = emission_tonne, fill = poll)) +
  facet_wrap(~sector, scales = "free_y", ncol = 2) +
  rcrea::theme_crea() +
  scale_fill_manual(name = "", values = rev(c(unname(rcrea::pal_crea[seq_len(11)]), "darkorchid4"))) +
  scale_y_continuous(expand=expansion(mult = c(0, 0.1))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(subtitle = "All sectors - Comparison between CREA and EDGAR total emissions, Jakarta",
       y ="Tonne",
       x = NULL)
ggsave(sprintf("comparison/all_poll_sectors_jak.png"), width = 10, height = 14)


e_all_jak %>%
  as.data.frame() %>%
  filter(poll != 'CO') %>%
  group_by(poll, source) %>%
  summarise_at("emission_tonne", sum, na.rm = T) %>%
  ggplot() +
  geom_bar(stat = "identity",
           aes(poll, emission_tonne, fill = source),
           position = "dodge") +
  rcrea::theme_crea() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_manual(name = "",
                    values = unname(rcrea::pal_crea[c("Dark.blue", "Blue", "Orange", "Red", "Green", "#351c75")])) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(subtitle = "All sectors - Comparison between CREA and EDGAR total emissions, Jakarta",
       y =" Tonne",
       x = NULL)
ggsave(sprintf("comparison/all_total_jak.png"), width = 10, height = 5)


e_all_jak %>%
  ggplot() +
  geom_bar(stat = "identity",
           aes(province, emission_tonne, fill = source),
           position = "dodge") +
  facet_wrap(~poll, scales = "free", ncol = 2) +
  rcrea::theme_crea() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_manual(name = "",
                    values = unname(rcrea::pal_crea[c("Dark.blue", "Blue", "Orange", "Red", "Green", "#351c75")])) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                     labels = scales::comma) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(subtitle = "All sectors - Comparison between CREA and EDGAR provincial emissions, Jakarta",
       y = "Tonne",
       x = NULL)
ggsave(sprintf("comparison/all_province_jak.png"), width = 10, height = 10)



# iqair -------------------------------------------------------------------

iqair_files <- list.files("data/iqair/aq", full.names = TRUE)

# only iqair data in 2019
iqair_data <- lapply(iqair_files, function(file){
  data <- read_csv(file, show_col_types = FALSE) %>%
    mutate(location = basename(file) %>% str_replace('.csv', ''),
           ts = ymd_hms(ts))
}) %>% bind_rows() %>%
  filter(ts >= ymd("2019-01-01"), ts < ymd("2020-01-01")) %>%
  pivot_longer(-c(location, ts), names_to = 'poll', values_to = 'value') %>%
  filter(!is.na(value)) %>%
  arrange(ts)

stations <- read.csv('data/iqair/station coordinates.csv') %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  mutate(name = tolower(station.name)) %>%
  select(name) %>%
  filter(name %in% unique(data$location))

# meteosim data
met_files <- list.files('data/meteosim_tif/daily_tifs/', pattern = '.tif', full.names = TRUE)

iqair_comp <- pblapply(met_files, function(file,
                                           date_from = ymd('2019-01-01'),
                                           date_to = ymd('2020-01-01')){
  print(paste('processing', file))

  str <- str_split(basename(file) %>% tools::file_path_sans_ext(), '_|\\.') %>% unlist()
  domain <- str[2]
  date <- str[3] %>% ymd()
  poll <- str[4] %>% tolower()
  sector <- str[6]

  if(domain != 'd02'){
    print('not d02, skipping...')
    return(NULL)
  }

  if(!(date >= date_from & date < date_to)){
    print('date out of range, skipping...')
    return(NULL)
  }

  avail_loc <- data %>% filter(date(ts) == date) %>% distinct(location) %>% pull
  avail_poll <- data %>% filter(date(ts) == date) %>% distinct(poll) %>% pull

  if(!poll %in% avail_poll){
    print('poll not in data, skipping...')
    return(NULL)
  }

  if(length(avail_loc) > 0){
    r <- rast(file)
    v <- extract(r, vect(stations %>% filter(name %in% avail_loc))) %>%
      pivot_longer(-c("ID"), names_to = 'time', values_to = 'value') %>%
      mutate(sector = sector, poll = poll, domain = domain, date = date)
  } else {
    print('no station data, skipping...')
    return(NULL)
  }
}) %>% bind_rows()

iqair <- iqair_comp %>% mutate(date = ymd_h(paste(date, str_split_i(time, 'time', 2)))) %>%
  group_by(ID, date, poll) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup %>%
  left_join(stations %>% mutate(ID = seq(1, nrow(.))) %>%
              select(ID, name) %>%
              st_drop_geometry(),
            by = 'ID') %>%
  left_join(iqair_data, by = c('poll', 'name' = 'location', 'date' = 'ts')) %>%
  rename('meteosim' = value.x, 'iqair' = value.y) %>%
  pivot_longer(c(meteosim, iqair), names_to = 'source', values_to = 'value')

test2 %>% filter(poll == 'pm25') %>%
  ggplot(aes(date, value, color = source)) +
  geom_line() +
  facet_wrap(~name, scales = 'free', ncol = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(subtitle = "Comparison between Meteosim and IQAir data, PM2.5",
       y = "Time",
       x = "Concentration (ug/m3)")

iqair %>% filter(poll == 'pm10') %>%
  ggplot(aes(date, value, color = source)) +
  geom_line(na.rm = F) +
  facet_wrap(~name, scales = 'free', ncol = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(subtitle = "Comparison between Meteosim and IQAir data, PM10",
       y = "Time",
       x = "Concentration (ug/m3)")


