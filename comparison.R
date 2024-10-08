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
dir.create("comparison", showWarnings=F)


sectors <- names(edgar.local_to_edgar_sectors())

compare_sector <- function(sector){
  print(paste0("=================", sector, "================="))

  polls <- c("NOx","SO2","BC","OC","PM","NMVOC","NH3","CH4","CO")
  polls.edgar <- gsub("PM", "PM10", polls)
  sector_name <- data.sector_name(sector)

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
  png(sprintf("comparison/%s_rasters.png", sector), width=1024, height=800)
  raster::stack(c(r.crea %>% `names<-`(paste("CREA", sector, polls)),
                  r.edgar %>% `names<-`(paste("EDGAR", sector, polls)))) %>%
    raster::plot(colNA="black")
  dev.off()

  # Province comparison (we buffer to be sure coastal emitters are included)
  # Inland buffering is prevented
  g <- data.bps_map(buffer_km=50) %>%
    group_by(province) %>%
    summarise()

  e.crea <- lapply(polls,
                   function(p) cbind(g,
                                     emission_tonne=raster::extract(r.crea[[p]], g, fun=sum, na.rm=T),
                                     poll=p,
                                     source="CREA (2019)"))
  e.edgar <- lapply(polls,
                    function(p) cbind(g,
                                      emission_tonne=raster::extract(r.edgar[[p]], g, fun=sum, na.rm=T),
                                      poll=p,
                                      source="EDGAR (2015)"))


  (plt2 <- bind_rows(
    e.crea,
    e.edgar
  ) %>%
      as.data.frame() %>%
      ggplot() +
      geom_bar(stat="identity",
               aes(province, emission_tonne, fill=source),
               position="dodge") +
      facet_wrap(~poll, scales="free_y") +
      rcrea::theme_crea() +
      theme(legend.position = "top") +
      rcrea::scale_fill_crea_d(name="") +
      guides(fill = guide_legend(nrow = 1)) +
      scale_y_continuous(expand=expansion(mult=c(0,0.1)),
                         labels=scales::comma) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      labs(subtitle=sprintf("%s - Comparison between CREA and EDGAR provincial emissions", sector_name),
           y="Tonne",
           x=NULL,
           caption=sprintf("EDGAR sector(s) considered: %s.",
                           paste(edgar.local_to_edgar_sectors(sector), collapse=", "))))

  ggsave(sprintf("comparison/%s_province.png",sector), plt2, width=10, height=5)

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
      theme(legend.position = "top") +
      rcrea::scale_fill_crea_d(name="") +
      guides(fill = guide_legend(nrow = 1)) +
      scale_y_continuous(expand=expansion(mult=c(0,0.1)),
                         labels=scales::comma) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      labs(subtitle=sprintf("%s - Comparison between CREA and EDGAR total emissions", sector_name),
           y="Tonne",
           x=NULL,
           caption=sprintf("EDGAR sector(s) considered: %s.",
                           paste(edgar.local_to_edgar_sectors(sector), collapse=", "))))

  ggsave(sprintf("comparison/%s_total.png",sector), plt3, width=10, height=5)

}

purrr::map(sectors, purrr::safely(compare_sector))


compare_sectors <- function(sectors){

  polls <- c("NOx","SO2","BC","OC","PM","NMVOC","NH3","CH4")
  polls.edgar <- gsub("PM", "PM10", polls)

  # Get CREA results
  g <- data.bps_map(buffer_km=20) %>%
    group_by(province) %>%
    summarise()

  e_crea <- purrr::map(polls, function(poll){
    purrr::map(sectors, function(sector){
      f <- sprintf("results/%s.%s.edgar.tif", sector, poll)
      if(file.exists(f)) raster::raster(f) else data.grid.edgar() %>% setValues(0)
      }) %>%
      raster::stack() %>%
      `names<-`(sectors) %>%
      raster::extract(g, fun=sum, na.rm=T) %>%
      as.data.frame() %>%
      mutate(province=g$province,
             poll=poll) %>%
      tidyr::pivot_longer(cols=-c("poll","province"),
                          names_to = "sector",
                          values_to = "emission_tonne")
  }) %>%
    do.call(bind_rows, .) %>%
    mutate(source='CREA')

  # Get EDGAR results
  e_edgar <- purrr::map(polls.edgar, function(poll){
    purrr::map(sectors, function(sector){
      tryCatch({edgar.emission(sector=sector, poll=poll)},
               error=function(e){
                 return(data.grid.edgar() %>% setValues(0))
               })
    }) %>%
      do.call(raster::stack, .) %>%
      `names<-`(sectors) %>%
      raster::extract(g, fun=sum, na.rm=T) %>%
      as.data.frame() %>%
      mutate(province=g$province,
             poll=poll) %>%
      tidyr::pivot_longer(cols=-c("poll","province"),
                          names_to = "sector",
                          values_to = "emission_tonne")
  }) %>%
    do.call(bind_rows, .) %>%
    mutate(source='EDGAR')

  # Some CREA sectors are not covered by EDGAR
  shared_sectors <- e_edgar %>%
    group_by(sector) %>%
    summarise(total=sum(emission_tonne)) %>%
    filter(total>0) %>%
    pull(sector)

  e_crea_covered <- e_crea %>%
    filter(sector %in% shared_sectors) %>%
    mutate(source="CREA (covered by EDGAR)")

  sector_levels <- rev(names(edgar.local_to_edgar_sectors()))
  sector_labels <- data.sector_name(sector_levels)

  e_all <- bind_rows(
    e_crea,
    e_crea_covered,
    e_edgar %>% mutate(poll=recode(poll,PM10="PM"))
  ) %>%
    mutate(sector=factor(sector,
                         levels=sector_levels,
                         labels=sector_labels))


  saveRDS(e_all, "comparison/emission_all.RDS")

  (plt2 <- e_all %>%
      ggplot() +
      geom_bar(stat="identity",
               aes(province, emission_tonne, fill=source),
               position="dodge") +
      facet_wrap(~poll, scales="free", ncol = 2) +
      rcrea::theme_crea() +
      theme(legend.position = "top") +
      guides(fill = guide_legend(nrow = 1)) +
      scale_fill_manual(name="",
                        values=unname(rcrea::pal_crea[c("Dark.blue","Blue","Orange","Red")])) +
      scale_y_continuous(expand=expansion(mult=c(0,0.1)),
                         labels=scales::comma) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      labs(subtitle="All sectors - Comparison between CREA and EDGAR provincial emissions",
           y="Tonne",
           x=NULL))

  ggsave(sprintf("comparison/all_province.png"), plt2, width=10, height=10)



  # Total comparison
  (plt3 <- e_all %>%
      as.data.frame() %>%
      group_by(poll, source) %>%
      summarise_at("emission_tonne", sum, na.rm=T) %>%
      ggplot() +
      geom_bar(stat="identity",
               aes(poll, emission_tonne, fill=source),
               position="dodge") +
      rcrea::theme_crea() +
      theme(legend.position = "top") +
      guides(fill = guide_legend(nrow = 1)) +
      scale_fill_manual(name="",
                        values=unname(rcrea::pal_crea[c("Dark.blue","Blue","Orange","Red")])) +
      scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
      labs(subtitle="All sectors - Comparison between CREA and EDGAR total emissions",
           y="Tonne",
           x=NULL))

  ggsave(sprintf("comparison/all_total.png"), plt3, width=10, height=5)

  # Total comparison by sector
  (plt4 <- e_all %>%
      mutate(source=gsub(" \\(","\n\\(",source)) %>%
      as.data.frame() %>%
      ggplot() +
      geom_bar(stat="identity",
               aes(x=source, y=emission_tonne, fill=sector)) +
      facet_wrap(~poll, scales="free_y", ncol = 2) +
      rcrea::theme_crea() +
      scale_fill_manual(name="", values=rev(c(unname(rcrea::pal_crea[seq_len(11)]),"darkorchid4"))) +
      scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
      labs(subtitle="All sectors - Comparison between CREA and EDGAR total emissions",
           y="Tonne",
           x=NULL))
           # caption=sprintf("Sectors covered: %s", paste(sectors, collapse=", "))))

  ggsave(sprintf("comparison/all_total_sectors.png"), plt4, width=10, height=10)

}

compare_sectors(sectors)

