library(magrittr)

source("globals/global_variables.R")
 
data("milk_year_dat")
data("milk_season_dat")

milk_year_dat$fat %>% hist(breaks = 100)
milk_year_dat$protein %>% hist(breaks = 100)
milk_year_dat$lactose %>% hist(breaks = 100)


FAs <- c(
  grep(pattern = "^pC[[:digit:]]", milk_col_names, value = T),
  grep(pattern = "^pFA", milk_col_names, value = T)
)


# -------------------------------------------------------------------------------------- #
# Convert FA to FA in fat for yearly data ####
# -------------------------------------------------------------------------------------- #

milk_year_02_dat <- milk_year_dat %>% 
dplyr::ungroup() %>% 
  # 3. Express the FA in percentage in fat %>% 
  dplyr::mutate(dplyr::across( .cols = dplyr::all_of(FAs), ~ .x / fat ))

cor(milk_year_dat$pC10, milk_year_02_dat$pC10)

a <- milk_year_02_dat %>% 
  dplyr::select(pFA_shrt, pFA_med, pFA_long) %>% 
  rowSums() 
a %>% hist(breaks = 200)
range(a)

save(milk_year_02_dat, 
     file = "data/milk_year_02_dat.rda", 
     compress = "xz")

# -------------------------------------------------------------------------------------- #
# Convert FA to FA in fat for season data ####
# -------------------------------------------------------------------------------------- #

milk_season_02_dat <- milk_season_dat %>% 
dplyr::ungroup() %>% 
  # 3. Express the FA in percentage in fat %>% 
  dplyr::mutate(dplyr::across( .cols = dplyr::all_of(FAs), ~ .x / fat ))

cor(milk_season_02_dat$pC10, milk_season_dat$pC10)
 

save(milk_season_02_dat, 
     file = "data/milk_season_02_dat.rda", 
     compress = "xz")