library(magrittr)

data("train_season_partial")
 
data("milk_season_clean_dat")
data("milk_year_clean_dat")

season_var <- names(train_season_partial) %>% 
  grep(pattern = "(Winter|Spring|Autumn|Summer)$", value = T) 


year_var <- names(train_season_partial) %>% 
  setdiff(., c(season_var, "gradient_axis1"))

season_var %<>% 
  sub(pattern = "\\_(Winter|Spring|Autumn|Summer)$", replacement = "") 

# --------------------- #
# Create full milk data
# --------------------- #

milk_season_partial <- milk_season_clean_dat %>% 
  dplyr::select(farmerID, year, year_quarter, dplyr::all_of(season_var)) %>% 
  tidyr::pivot_wider(id_cols = c("farmerID", "year"), 
                     names_from = "year_quarter",
                     values_from = dplyr::all_of(season_var)) %>% 
  dplyr::inner_join(
    milk_year_clean_dat %>% 
      dplyr::select(dplyr::all_of(year_var))
  )

milk_season_partial %<>% 
  tidyr::drop_na()

save(milk_season_partial, file = "data/milk_season_partial.rda", compress = "xz")
  
  
