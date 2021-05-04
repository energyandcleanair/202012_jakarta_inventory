bps.industri <- function(){
  library(stringr)
  d <- read.csv("data/bps/industri/mfg17e_prov.csv")
  names(d) <- str_extract(names(d), "[^.*]*")

  n <- read.csv("data/bps/industri/nomenclature.csv")
  g <- read.csv("data/bps/geographic_metadata.csv") %>%
    select(DPROVI17=value_prov,
           province_en=nama_prov_other) %>%
    distinct()

  d %>%
    tidyr::pivot_longer(-c(DPROVI17, LOCATI17, RENUM),
                        names_to="code") %>%
    left_join(n) %>%
    left_join(g)


}


bps.susunas <- function(){

  d1 <- read.csv("data/bps/susunas/kor19rt_revisi1_diseminasi.csv")
  d2 <- read.csv("data/bps/susunas/kor19ind_2_diseminasi.csv")

  # names(d1) %<>% str_extract("[^.*]*")
  # names(d2) %<>% str_extract("[^.*]*")

  n <- read.csv("data/bps/susunas/nomenclature.csv") %>%
    select(code, id)

  g <- read.csv("data/bps/geographic_metadata.csv") %>%
    select(province_id=value_prov,
           province=nama_prov_other,
           kab_id=value_kab,
           kab=nama_kab) %>%
    distinct()

  d <- bind_rows(d1,d2) %>%
    rename(!!!setNames(n$code, n$id)) %>%
    mutate(kab_id=province_id*100+kab_id) %>%
    left_join(g)

  d
}


bps.susenas <- function(){

  d1 <- read.csv("data/bps/susenas/blok42_diseminasi_r.csv")
  d2 <- read.csv("data/bps/susenas/blok43_diiseminasi.csv")

  names(d1) %<>% str_extract("[^.*]*")
  names(d2) %<>% str_extract("[^.*]*")

  n <- read.csv("data/bps/susenas/nomenclature.csv") %>%
    select(code, id)

  kodes <- read.csv("data/bps/susenas/b42kodes.csv") %>%
    select(kode, commodity_en, unit, timespan, usage)

  g <- read.csv("data/bps/geographic_metadata.csv") %>%
    select(province=value_prov,
           province_en=nama_prov_other) %>%
    distinct()

  d <- bind_rows(d1,d2) %>%
    rename(!!!setNames(n$code, n$id)) %>%
    left_join(g) %>%
    left_join(kodes, by="kode") %>%
    select(-c(province)) %>%
    rename(province=province_en)

  d
}
