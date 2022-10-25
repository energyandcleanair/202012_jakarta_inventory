sectors <- c(
  "agroob",
  "aviation",
  "comres",
  "forest",
  "gasdist",
  "industry",
  "landfill",
  "livestock",
  "power",
  "shipping",
  "solidwaste",
  "transport"
)

polls <- c("SO2", "NOx", "CO", "NMVOC",
           "NH3", "PM", "CH4", "BC", "OC")

all_emissions <- lapply(sectors, function(sector){
  get(paste0(sector, '.get_emission'))() %>%
    mutate(sector = sector)
}) %>% bind_rows()

all_sector <- all_emissions %>% group_by(sector, poll) %>%
  summarise(value = sum(emission, na.rm = F)) %>%
  ungroup()

all_poll <- all_emissions %>% group_by(poll) %>%
  summarise(value = sum(emission, na.rm = T)) %>%
  ungroup()

nc_table <- lapply(sectors, function(sector){
  nc <- terra::rast(glue::glue('results/{sector}.d02.nc'))
  summary <- lapply(polls, function(poll){
    subset <- nc[paste0('^', poll, '_datetime')]
    terra::global(sum(subset, na.rm = T), fun = 'sum', na.rm = T)
  })
  unlist(summary) %>% as_tibble() %>%
    mutate(sector = sector, poll = polls)
}) %>% bind_rows()

# compare nc and emission table
comp <- nc_table %>% left_join(all_sector, by = c('poll', 'sector'),
                               suffix = c('_nc', '_tab')) %>%
  mutate(diff_ratio = abs(value_nc - value_tab)/value_tab,
         status = case_when(diff_ratio < 0.05 | value_nc == 0 ~ 'PASS',
                            T ~ 'FAIL')) %>%
  select(sector, poll, value_tab, value_nc, diff_ratio, status)

write.csv(comp, 'diagnosis/nc_diagnosis.csv')


wo_table <- lapply(sectors, function(sector){
  nc <- terra::rast(glue::glue('results/scenario_wo_{sector}.d02.nc'))
  summary <- lapply(polls, function(poll){
    subset <- nc[paste0('^', poll, '_datetime')]
    terra::global(sum(subset, na.rm = T), fun = 'sum', na.rm = T)
  })
  unlist(summary) %>% as_tibble() %>%
    mutate(sector_wo = sector, poll = polls)
}) %>% bind_rows()

wo_table %>% left_join(all_poll, by = c('poll'), suffix = c('_wo', '_all')) %>%
  left_join(all_sector, by = c('sector_wo' = 'sector', 'poll'))


# lapply(polls, function(poll){
#   rast <- terra::rast(glue::glue('results/transport.{poll}.d01.tif'))
#   terra::global(rast, fun = 'sum', na.rm = T)
# })










