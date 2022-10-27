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

grids <- c('d02', 'd03', 'd04')

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

nc_table <- lapply(grids, function(grid){
  lapply(sectors, function(sector){
    nc <- terra::rast(glue::glue('results/{sector}.{grid}.nc'))
    summary <- lapply(polls, function(poll){
      subset <- nc[paste0('^', poll, '_datetime')]
      terra::global(sum(subset, na.rm = T), fun = 'sum', na.rm = T)
    })
    unlist(summary) %>% as_tibble() %>%
      mutate(sector = sector, poll = polls, grid = grid)
  })
}) %>% bind_rows()

# compare nc and emission table
comp <- nc_table %>% left_join(all_sector, by = c('poll', 'sector'),
                               suffix = c('_nc', '_tab')) %>%
  mutate(diff_ratio = abs(value_nc - value_tab)/value_tab,
         status = case_when(diff_ratio < 0.05 | value_nc == 0 ~ 'PASS',
                            T ~ 'FAIL')) %>%
  select(sector, poll, value_tab, value_nc, diff_ratio, status)

write.csv(comp, 'diagnosis/nc_diagnosis.csv')


nc_poll <- nc_table %>% group_by(poll, grid) %>%
  summarise(value = sum(value))
wo_table <- lapply(grids, function(grid){
  lapply(sectors, function(sector){
    nc <- terra::rast(glue::glue('results/scenario_wo_{sector}.{grid}.nc'))
    summary <- lapply(polls, function(poll){
      subset <- nc[paste0('^', poll, '_datetime')]
      terra::global(sum(subset, na.rm = T), fun = 'sum', na.rm = T)
    })
    unlist(summary) %>% as_tibble() %>%
      mutate(sector_wo = sector, poll = polls, grid = grid)
  })
}) %>% bind_rows()

# compare scenario and original nc
scen_check <- wo_table %>%
  left_join(nc_poll %>% rename(value_all_sector = value), by = c('poll', 'grid')) %>%
  left_join(nc_table %>% rename(value_om_sector = value),
            by = c('sector_wo' = 'sector', 'poll', 'grid')) %>%
  mutate(diff = abs(value_all_sector - value - value_om_sector),
         status = case_when(diff < 1 ~ 'PASS',
                            T ~ 'FAIL')) %>%
  select(sector_wo, poll, grid, value, value_all_sector,
         value_om_sector, diff, status)

write.csv(scen_check, 'diagnosis/scenario_diagnosis.csv')

# lapply(polls, function(poll){
#   rast <- terra::rast(glue::glue('results/transport.{poll}.d01.tif'))
#   terra::global(rast, fun = 'sum', na.rm = T)
# })










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

grids <- c('d02', 'd03', 'd04')

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

nc_table <- lapply(grids, function(grid){
  lapply(sectors, function(sector){
    nc <- terra::rast(glue::glue('results/{sector}.{grid}.nc'))
    summary <- lapply(polls, function(poll){
      subset <- nc[paste0('^', poll, '_datetime')]
      terra::global(sum(subset, na.rm = T), fun = 'sum', na.rm = T)
    })
    unlist(summary) %>% as_tibble() %>%
      mutate(sector = sector, poll = polls, grid = grid)
  })
}) %>% bind_rows()

# compare nc and emission table
comp <- nc_table %>% filter(grid == 'd02') %>%
  left_join(all_sector, by = c('poll', 'sector'),
            suffix = c('_nc', '_tab')) %>%
  mutate(diff_ratio = abs(value_nc - value_tab)/value_tab,
         status = case_when(diff_ratio < 0.05 | value_nc == 0 ~ 'PASS',
                            T ~ 'FAIL')) %>%
  select(sector, poll, value_tab, value_nc, diff_ratio, status)

write.csv(comp, 'diagnosis/nc_diagnosis.csv')


nc_poll <- nc_table %>% group_by(poll, grid) %>%
  summarise(value = sum(value))
wo_table <- lapply(grids, function(grid){
  lapply(sectors, function(sector){
    nc <- terra::rast(glue::glue('results/scenario_wo_{sector}.{grid}.nc'))
    summary <- lapply(polls, function(poll){
      subset <- nc[paste0('^', poll, '_datetime')]
      terra::global(sum(subset, na.rm = T), fun = 'sum', na.rm = T)
    })
    unlist(summary) %>% as_tibble() %>%
      mutate(sector_wo = sector, poll = polls, grid = grid)
  })
}) %>% bind_rows()

# compare scenario and original nc
scen_check <- wo_table %>%
  left_join(nc_poll %>% rename(value_all_sector = value), by = c('poll', 'grid')) %>%
  left_join(nc_table %>% rename(value_om_sector = value),
            by = c('sector_wo' = 'sector', 'poll', 'grid')) %>%
  mutate(diff = abs(value_all_sector - value - value_om_sector),
         status = case_when(diff < 1 ~ 'PASS',
                            T ~ 'FAIL')) %>%
  select(sector_wo, poll, grid, value, value_all_sector,
         value_om_sector, diff, status)

write.csv(scen_check, 'diagnosis/scenario_diagnosis.csv')

# lapply(polls, function(poll){
#   rast <- terra::rast(glue::glue('results/transport.{poll}.d01.tif'))
#   terra::global(rast, fun = 'sum', na.rm = T)
# })










