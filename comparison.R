library(creainventory)
library(raster)
library(sf)
library(tidyverse)
library(creatrajs)
library(eixport)

lapply(list.files(".", "data.*.R"),source)
source('utils.R')
source('edgar.R')
source('data.R')


sectors <- c("power","industry","comres","agroob","shipping","aviation","landfill")
# names(edgar.local_to_edgar_sectors())

compare_sector <- function(sector){
  print(paste0("=================", sector, "================="))

  polls <- c("NOx","SO2","BC","OC","PM","NMVOC","NH3","CH4")
  polls.edgar <- gsub("PM", "PM10", polls)

  # Get CREA results
  r.crea <- purrr::map(polls, function(poll){
    f <- sprintf("results/%s.%s.edgar.tif", sector, poll)
    if(file.exists(f)) raster::raster(f) else data.grid.edgar() %>% setValues(0)
  })

  names(r.crea) <- polls
  r.crea.tonne <- lapply(r.crea, function(r) raster::cellStats(r, sum))

  # Get EDGAR results
  r.edgar <- lapply(polls.edgar, function(poll){
    tryCatch({edgar.emission(sector=sector, poll=poll)},
             error=function(e){
               return(data.grid.edgar() %>% setValues(0))
             })
  })

  names(r.edgar) <- polls
  r.edgar.tonne <- lapply(r.crea, function(r) raster::cellStats(r, sum))

  # Negative to NA
  r.edgar <- lapply(r.edgar, function(r) reclassify(r, cbind(-Inf, 0, NA), right=T))
  r.crea <- lapply(r.crea, function(r) reclassify(r, cbind(-Inf, 0, NA), right=T))

  # Maps
  png(sprintf("results/comparison/%s_rasters.png", sector), width=1024, height=800)
  raster::stack(c(r.crea %>% `names<-`(paste("CREA", sector, polls)),
                  r.edgar %>% `names<-`(paste("EDGAR", sector, polls)))) %>%
    raster::plot(colNA="black")
  dev.off()

  # Province comparison
  g <- data.gadm() %>% filter(level==1)

  # Add a buffer cause some locations are closed to the coast
  g.buffered <- g %>%
    sf::st_buffer(20000) %>% # For some unknown reason, this is in meter even though we're using 4326
    sf::st_difference(sf::st_union(g)) %>%
    bind_rows(g) %>%
    group_by(id, name) %>%
    summarise(geometry=sf::st_union(geometry))


  e.crea <- lapply(polls,
                   function(p) cbind(g.buffered,
                                     emission_tonne=raster::extract(r.crea[[p]], g.buffered, fun=sum, na.rm=T),
                                     poll=p,
                                     source="CREA"))
  e.edgar <- lapply(polls,
                    function(p) cbind(g.buffered,
                                      emission_tonne=raster::extract(r.edgar[[p]], g.buffered, fun=sum, na.rm=T),
                                      poll=p,
                                      source="EDGAR"))


  (plt2 <- bind_rows(
    e.crea,
    e.edgar
  ) %>%
    as.data.frame() %>%
    ggplot() +
    geom_bar(stat="identity",
             aes(name, emission_tonne, fill=source),
             position="dodge") +
    facet_wrap(~poll, scales="free_y") +
    rcrea::theme_crea() +
    rcrea::scale_fill_crea_d() +
    scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
    labs(subtitle=sprintf("%s - Comparison between CREA and EDGAR provincial emissions", sector),
         y="Tonne",
         x=NULL))

  ggsave(sprintf("results/comparison/%s_province.png",sector), plt2, width=12, height=6)

  # Total comparison
  (plt3 <- bind_rows(
    e.crea,
    e.edgar
  ) %>%
    as.data.frame() %>%
    group_by(poll, source) %>%
    summarise_at("emission_tonne", sum, na.rm=T) %>%
    ggplot() +
    geom_bar(stat="identity",
             aes(poll, emission_tonne, fill=source),
             position="dodge") +
    rcrea::theme_crea() +
    rcrea::scale_fill_crea_d() +
    scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
    labs(subtitle=sprintf("%s - Comparison between CREA and EDGAR total emissions", sector),
         y="Tonne",
         x=NULL))

  ggsave(sprintf("results/comparison/%s_total.png",sector), plt3, width=12, height=6)

}

purrr::map(sectors, purrr::safely(compare_sector))


