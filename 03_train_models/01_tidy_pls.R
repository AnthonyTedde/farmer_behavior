library(magrittr)
library(plsmod)

if(file.exists("data/dat_vect.rda")){
  data("dat_vect")
}else{
  # dat_vect <- c("train_year_full", "train_season_full", "train_season_partial", 
  #               "train_winsum_full", "train_winsum_partial")  
  # dat_vect <- c("train_year_corrected_full", "train_season_corrected_full", 
  #               "train_season_corrected_partial", "train_winsum_corrected_full", 
  #               "train_winsum_corrected_partial")
  dat_vect <- c("train_season_corrected_full", "train_season_corrected_partial", 
                "train_winsum_corrected_full", "train_winsum_corrected_partial")
}

# CECI Load data
# Training data
data(list = dat_vect)
# Cross-validation data
data(list = paste(dat_vect, "cv", sep = "_"))

# CECI: change ncpu
source("globals/global_variables.R")  
source("helper_functions/misc.R")


# -------------------------------------------------------------------------------------- #
# data ####
# -------------------------------------------------------------------------------------- #
# train_year_working <- train_year_full %>% 
#   dplyr::select(-c("farmerID", "year"))
# train_year_working_cv <- train_year_full_cv
# train_season_working <- train_season_full %>% 
#   dplyr::select(-c("farmerID", "year"))
# train_season_working_cv <- train_season_full_cv

# -------------------------------------------------------------------------------------- #
# Create recipe specification ####
# -------------------------------------------------------------------------------------- #

get_recipe <- function(dat){
  
  
  recipes::recipe(
    x = dat, 
    var = names(dat),
    roles = c("outcome", rep("predictor", length(names(dat)) -1))
  ) %>% 
    # recipes::step_ns(recipes::all_numeric_predictors(), deg_free = ) %>% 
    bestNormalize::step_orderNorm(recipes::all_numeric_predictors())
  
}


# -------------------------------------------------------------------------------------- #
# Create model specification ####
# -------------------------------------------------------------------------------------- #

get_model <- function(){
  
  parsnip::pls(
    num_comp = tune::tune(),
    predictor_prop = tune::tune()
  ) %>% 
    parsnip::set_engine("mixOmics") %>% 
    parsnip::set_mode("regression")
  
}


# -------------------------------------------------------------------------------------- #
# Create workflow specification ####
# -------------------------------------------------------------------------------------- #

get_workflow <- function(mod, rec){ 
  
  workflows::workflow() %>% 
    workflows::add_recipe(rec) %>% 
    workflows::add_model(mod)
  
}

# pls_mod_wfl %>% tune::tunable()
# pls_mod_wfl %>% hardhat::extract_parameter_set_dials() %>%  dplyr::pull(object)

get_param <- function(wfl){
  
  wfl %>% 
    hardhat::extract_parameter_set_dials() %>% 
    update(
      num_comp = dials::num_comp(range = c(1L, 99L))
    )
  
}


# -------------------------------------------------------------------------------------- #
# Create workflow specification ####
# -------------------------------------------------------------------------------------- #

# CECI parameters configuration (initial, iter, no_improve)
run_mdl <- function(wfl, dat_cv, param, iter = 500){ 
  pls_mod_wfl_rslt <- tune::tune_bayes(
    object = wfl,
    resamples = dat_cv,
    iter = iter,
    initial = initial_set_n, # CECI globals
    metrics = yardstick::metric_set(yardstick::rmse),
    param_info = param,
    control = tune::control_bayes(
      verbose = T, 
      seed = 42, 
      uncertain = 5, 
      no_improve = early_stop # CECI globals
    )
  )
  return(pls_mod_wfl_rslt)
}

# For test purpose
# initial_set_n <- 4
# early_stop <- 1

doParallel::registerDoParallel(cores = ncpu) 
options(tidymodels.dark = T)


tictoc::tic()
for(d in dat_vect){
  print(glue::glue("#-------------------     {d}     -------------------#"))
  # data
  dat <- get(d) %>%  dplyr::select(-c("farmerID", "year"))
  dat_cv <- get(paste(d, "cv", sep = "_"))
  # Get specifications
  recipe <- get_recipe(dat)
  mod <- get_model()
  wfl <- get_workflow(mod, recipe) 
  param <- get_param(wfl)
  # Run models 
  mdl <- run_mdl( wfl,  dat_cv,  param,  iter = niter ) # niter -> CECI globals 
  # Save result
  save_model(obj = mdl, type = "rslt")
  save_model(obj = wfl, type = "wfl")
  # Save progress
  dat_vect <- setdiff(dat_vect, d)
  save(dat_vect, file = here::here("data", "dat_vect.rda"))
}
tictoc::toc()     

file.remove(here::here("data", "dat_vect.rda"))