library(magrittr)

data("milk_dat")
data("technico_dat")

technico_dat %>% is.na %>% any()
milk_dat %>% is.na %>% any()

naniar::gg_miss_upset(milk_dat)

milk_dat %<>% 
  tidyr::drop_na()
  

save(milk_dat, file = "data/milk_dat.rda")


naniar::gg_miss_upset(technico_dat)

technico_dat %<>% 
  tidyr::drop_na()

save(technico_dat, file = "data/technico_dat.rda")