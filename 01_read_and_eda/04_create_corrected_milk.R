library(magrittr)

data("milk_year_clean_dat")
data("milk_season_clean_dat")

# -------------------------------------------------------------------------------------- #
# year and year-season correction ####
# -------------------------------------------------------------------------------------- #

milk_year_corrected_clean_dat <- milk_year_clean_dat %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(dplyr::across(-c("farmerID"), ~{.x - mean(.x)})) %>% 
  dplyr::ungroup()

milk_season_corrected_clean_dat <- milk_season_clean_dat %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(dplyr::across(-c("farmerID", "year_quarter"), ~{.x - mean(.x)})) %>% 
  dplyr::ungroup()


# -------------------------------------------------------------------------------------- #
# Save ####
# -------------------------------------------------------------------------------------- #

save(milk_year_corrected_clean_dat, 
     file = "data/milk_year_corrected_clean_dat.rda",
     compress = "xz")
save(milk_season_corrected_clean_dat, 
     file = "data/milk_season_corrected_clean_dat.rda",
     compress = "xz")