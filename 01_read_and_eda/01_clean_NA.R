library(magrittr)

data("milk_dat")
data("technico_dat")
data("milk_quarter_dat")


# -------------------------------------------------------------------------------------- #
# Milk  
# -------------------------------------------------------------------------------------- #

# technico_dat %>% is.na %>% any()
milk_dat %>% is.na %>% any()

naniar::gg_miss_upset(milk_dat)

milk_dat %<>% 
  dplyr::select(-lactose)

milk_dat %<>% 
  tidyr::drop_na()
  

save(milk_dat, file = "data/milk_dat.rda")


# -------------------------------------------------------------------------------------- #
# Milk quarter 
# -------------------------------------------------------------------------------------- #

naniar::gg_miss_upset(milk_quarter_dat)

milk_quarter_dat %<>% 
  dplyr::select(-lactose)

naniar::gg_miss_upset(milk_quarter_dat)
naniar::prop_miss(milk_quarter_dat)
naniar::miss_var_summary(milk_quarter_dat)

milk_quarter_dat %>% 
  dplyr::filter(is.na(pFA_iso)) %>% 
  dplyr::pull(year) %>% table

milk_quarter_dat %>% 
  dplyr::filter(year == 2018)

milk_quarter_dat %<>% 
  tidyr::drop_na()

naniar::gg_miss_upset(milk_quarter_dat)
naniar::prop_miss(milk_quarter_dat)
naniar::miss_var_summary(milk_quarter_dat)

save(milk_quarter_dat, 
     file = "data/milk_quarter_dat.rda",
     compress = "xz")


# -------------------------------------------------------------------------------------- #
# technico  
# -------------------------------------------------------------------------------------- #

naniar::gg_miss_upset(technico_dat)

technico_dat %<>% 
  tidyr::drop_na()

save(technico_dat, file = "data/technico_dat.rda")

