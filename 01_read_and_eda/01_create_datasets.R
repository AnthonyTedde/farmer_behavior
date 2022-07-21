library(magrittr)

source("globals/global_variables.R")

# -------------------------------------------------------------------------------------- #
# Load data
# -------------------------------------------------------------------------------------- #

data("milk_month_dat")


dim(milk_month_dat)

naniar::miss_var_summary(milk_month_dat) %>% 
  dplyr::filter(n_miss > 0)

# -------------------------------------------------------------------------------------- #
#  Season milk data
# -------------------------------------------------------------------------------------- #

milk_month_dat %<>%  
  dplyr::mutate( 
    year_quarter = lubridate::quarter(lubridate::my(paste(month, year, sep = "-"))),
    .after = month
  ) %>% 
  dplyr::mutate( year_quarter = dplyr::case_when(
    year_quarter == 1 ~ "Winter",
    year_quarter == 2 ~ "Spring",
    year_quarter == 3 ~ "Summer",
    year_quarter == 4 ~ "Autumn"
  ) ) %>% 
  tibble::as_tibble()
 

tictoc::tic()
milk_season_dat <- milk_month_dat %>% 
  dplyr::group_by(farmerID, year, year_quarter) %>% 
  dplyr::summarise(
  dplyr::across(dplyr::all_of(c(labo, predictions)), 
                .fns = function(x){
    weight <- dplyr::cur_data() %>% 
      dplyr::pull(paste("n", dplyr::cur_column(), sep = "_"))
    weighted.mean(x, weight, na.rm = T)
  })
)
tictoc::toc()

milk_season_dat %<>% dplyr::ungroup()
  
save(milk_season_dat, file = "data/milk_season_dat.rda", compress = "xz")


# -------------------------------------------------------------------------------------- #
#  Yearly milk data
# -------------------------------------------------------------------------------------- #

tictoc::tic()
milk_year_dat <- milk_month_dat %>% 
  dplyr::group_by(farmerID, year) %>% 
  dplyr::summarise(
  dplyr::across(dplyr::all_of(c(labo, predictions)), 
                .fns = function(x){
    weight <- dplyr::cur_data() %>% 
      dplyr::pull(paste("n", dplyr::cur_column(), sep = "_"))
    weighted.mean(x, weight, na.rm = T)
  })
)
tictoc::toc()

milk_year_dat %<>% dplyr::ungroup()
  
save(milk_year_dat, file = "data/milk_year_dat.rda", compress = "xz")
