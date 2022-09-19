library(magrittr)
library(dplyr) # For broom tidy

data("milk_season_partial_augmented")
data("pls_final_mdl")

source("globals/global_variables.R")
 
# -------------------------------------------------------------------------------------- #
# Create dataset for training
# -------------------------------------------------------------------------------------- #
predictors <- broom::tidy(pls_final_mdl) %>% 
  dplyr::filter(type == "predictors") %>% 
  dplyr::pull(term) %>% unique
y <- "cluster"

dat <- milk_season_partial_augmented

all_dt <- dat %>% 
  dplyr::select(dplyr::all_of(c(y, predictors)))

# -------------------------------------------------------------------------------------- #
# Create train - test
# -------------------------------------------------------------------------------------- #

set.seed(1010)
initial_split <- rsample::initial_split(all_dt, prop = 9/10, strata = cluster)

train_dt <- rsample::training(initial_split)
test_dt <- rsample::testing(initial_split)

train_cv <- rsample::vfold_cv(all_dt, v = 10, strata = cluster)

  
doParallel::registerDoParallel(cores = ncpu) 
options(tidymodels.dark = T)

      
#---------------
# Getter functions
get_recipe <- function(dat){  
  recipes::recipe(
    x = dat, 
    var = names(dat),
    roles = c("outcome", rep("predictor", length(names(dat)) -1))
  ) %>% 
    bestNormalize::step_orderNorm(recipes::all_numeric_predictors()) 
}

get_model <- function(){ 
  parsnip::pls(
    num_comp = tune::tune(),
    predictor_prop = tune::tune(),
    # predictor_prop = 1
  ) %>% 
    parsnip::set_engine("mixOmics") %>% 
    parsnip::set_mode("classification") 
}

get_workflow <- function(mod, rec){  
  workflows::workflow() %>% 
    workflows::add_recipe(rec) %>% 
    workflows::add_model(mod) 
}


get_param <- function(wfl){ 
  wfl %>% 
    hardhat::extract_parameter_set_dials() %>% 
    update(
      num_comp = dials::num_comp(range = c(1L, 70L))
    ) 
}

#---------------
# Run baby run...


run_mdl <- function(wfl, dat_cv, param, iter = 500){ 
  pls_mod_wfl_rslt <- tune::tune_bayes(
    object = wfl,
    resamples = dat_cv,
    iter = iter,
    initial = initial_set_n, # CECI globals
    # metrics = yardstick::metric_set(yardstick::rmse),
    param_info = param,
    control = tune::control_bayes(
      verbose = T, 
      seed = 42, 
      uncertain = 3, 
      no_improve = early_stop # CECI globals
    )
  )
  return(pls_mod_wfl_rslt)
}

# For test purpose
# initial_set_n <- 4
# early_stop <- 1



# data
dat <- train_dt
dat_cv <- train_cv
 
# Get specifications
recipe <- get_recipe(dat)
mod <- get_model()
wfl <- get_workflow(mod, recipe) 
param <- get_param(wfl)

set.seed(1010)
# Run models 
tictoc::tic()
technico_cluster_mdl <- run_mdl( wfl,  dat_cv,  param,  iter = niter ) # niter -> CECI globals 
tictoc::toc()
# Save result

save(technico_cluster_mdl, file = "data/technico_cluster_mdl.rda")





