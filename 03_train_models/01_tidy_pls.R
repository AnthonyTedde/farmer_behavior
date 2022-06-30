library(magrittr)
library(plsmod)

# CECI Load data
# Training data
data("train_full")
data("train_q20")
data("train_q10")
data("train_q05")
# Cross-validation data
data("train_full_cv")
data("train_q20_cv")
data("train_q10_cv")
data("train_q05_cv")

# CECI: change ncpu
source("globals/global_variables.R")  
source("helper_functions/misc.R")


# -------------------------------------------------------------------------------------- #
# Create recipe specification ####
# -------------------------------------------------------------------------------------- #

get_recipe <- function(dat){
  
  form <- milk_potential_predictors[milk_potential_predictors %in% names(dat)] %>% 
    paste(collapse = " + ") %>% 
    paste(y, ., sep = " ~ ") %>% 
    formula
  
  recipes::recipe(form, data = dat) %>% 
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

if(file.exists("data/dat_vect.rda")){
  data("dat_vect")
}else{
  dat_vect <- c("train_full", "train_q20", "train_q10", "train_q05")  
}

tictoc::tic()
for(d in dat_vect){
  print(glue::glue("#-------------------     {d}     -------------------#"))
  # data
  dat <- get(d)
  dat_cv <- get(paste(d, "cv", sep = "_"))
  # Get specifications
  recipe <- get_recipe(dat)
  mod <- get_model()
  wfl <- get_workflow(mod, recipe) 
  param <- get_param(wfl)
  # Run models 
  mdl <- run_mdl(
    wfl, 
    dat_cv, 
    param, 
    iter = niter # CECI globals
  )
  # Save result
  save_model("rslt")
  save_model("wfl")
  # Save progress
  dat_vect <- setdiff(dat_vect, d)
  save(dat_vect, file = here::here("data", "dat_vect.rda"))
}
tictoc::toc()     

file.remove(here::here("data", "dat_vect.rda"))