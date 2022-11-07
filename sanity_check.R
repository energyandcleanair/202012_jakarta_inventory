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

nc_table <- lapply(c('d02', 'd03', 'd04'), function(grid){
  lapply(sectors, function(sector){
    if(file.exists(glue::glue('results/{sector}.{grid}.nc'))){
      nc <- terra::rast(glue::glue('results/{sector}.{grid}.nc'))
      summary <- lapply(polls, function(poll){
        subset <- nc[paste0('^', poll, '_datetime')]
        terra::global(sum(subset, na.rm = T), fun = 'sum', na.rm = T)
      })
      unlist(summary) %>% as_tibble() %>%
        mutate(sector = sector, poll = polls, grid = grid)
    } else {
      message(glue::glue('File {sector}.{grid}.nc not found. Continuing...'))
    }
  })
}) %>% bind_rows()

write.csv(nc_table,  'nc_table.csv')

# compare nc and emission table
comp <- nc_table %>% filter(grid == 'd02') %>%
  left_join(all_sector, by = c('poll', 'sector'), suffix = c('_nc', '_tab')) %>%
  mutate(diff_ratio = abs(value_nc - value_tab)/value_tab,
         status = case_when(diff_ratio < 0.05 | value_nc == 0 ~ 'PASS',
                            T ~ 'FAIL')) %>%
  select(sector, poll, value_tab, value_nc, diff_ratio, status)

write.csv(comp, 'diagnosis/nc_diagnosis.csv')


nc_table_temporal <- lapply(c('d02', 'd03', 'd04'), function(grid){
  lapply(sectors, function(sector){
    tryCatch({
      nc <- terra::rast(glue::glue('results/{sector}.{grid}.nc'))
      summary <- lapply(polls, function(poll){
        subset <- nc[paste0('^', poll, '_datetime')]
        lapply(1:365, function(day){
          temp_subset <- subset[[((day-1)*24 + 1):(day*24)]]
          terra::global(sum(temp_subset, na.rm = T), fun = 'sum', na.rm = T)
        }) %>% unlist() %>%
          as_tibble() %>%
          mutate(sector = sector, poll = poll, grid = grid, day = 1:365)
      })
    }, error = function(e){
      message('File not found. Continuing...')
    })
  }) %>% bind_rows()
}) %>% bind_rows()

write.csv(nc_table_temporal, 'diagnosis/nc_table_temporal.csv')


# plotting temporal variation
lapply(c('d02', 'd03', 'd04'), function(grid_){
  lapply(sectors, function(sect){
    ggplot(nc_table_temporal %>% filter(sector == sect, grid == grid_)) +
      geom_line(aes(day, value, colour = poll)) +
      labs(title = glue::glue('{sect}_{grid_}')) +
      rcrea::theme_crea()
    ggsave(glue::glue('{sect}_{grid_}.png'), width = 8, height = 6)
  })
})


nc_poll <- nc_table %>% group_by(poll, grid) %>%
  summarise(value = sum(value))
wo_table <- lapply(c('d02', 'd03', 'd04'), function(grid){
  lapply(sectors, function(sector){
    if(file.exists(glue::glue('results/scenario_wo_{sector}.{grid}.nc'))){
      nc <- terra::rast(glue::glue('results/scenario_wo_{sector}.{grid}.nc'))
      summary <- lapply(polls, function(poll){
        subset <- nc[paste0('^', poll, '_datetime')]
        terra::global(sum(subset, na.rm = T), fun = 'sum', na.rm = T)
      })
      unlist(summary) %>% as_tibble() %>%
        mutate(sector_wo = sector, poll = polls, grid = grid)
    } else {
      message(glue::glue('File scenario_wo_{sector}.{grid}.nc not found. Continuing...'))
    }
  })
}) %>% bind_rows()

write.csv(wo_table, 'diagnosis/wo_table.csv')

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










