library(plsmod)
library(mixOmics)

dat <- all_data_augmented %>% 
  dplyr::relocate(cluster, .before = 1) %>% 
  dplyr::relocate(data_type, .after = 1) %>% 
  dplyr::mutate(cluster = factor(cluster))
  
pmax <- ncol(dat)
n <- nrow(dat)
dat <- dat[seq_len(n), seq_len(pmax)] 


# doParallel::registerDoParallel(cores = 1) 
options(tidymodels.dark = T)

  
#---------------
# Create the dataset
all_dt <- dat %>% 
  dplyr::select(!dplyr::starts_with("dim")) %>% 
  dplyr::relocate(cluster, .before = 1)

train_dt <- all_dt %>% 
  dplyr::filter(data_type == "train")
test_dt <- all_dt %>% 
  dplyr::filter(data_type == "test")

#---------------
# Create cross_validation sets
# train_dt_cv <- rsample::group_vfold_cv(
#   train_dt, 
#   group = farmerID,
#   v = 10
# )
train_dt_cv <- rsample::vfold_cv(
  train_dt, 
  strata = cluster,
  v = 10
)


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
    predictor_prop = tune::tune()
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
      # num_comp = dials::num_comp(range = c(1L, pmax))
      num_comp = dials::num_comp(range = c(1L, 40))
    ) 
}

#---------------
# Run baby run...


run_mdl <- function(wfl, dat_cv, param, iter = 500){ 
  tune::tune_bayes(
    object = wfl,
    resamples = dat_cv,
    iter = iter,
    initial = initial_set_n, # CECI globals
    # metrics = yardstick::metric_set(yardstick::roc_auc),
    param_info = param,
    control = tune::control_bayes(
      verbose = T, 
      seed = 42, 
      uncertain = 3, 
      no_improve = early_stop # CECI globals
    )
  )
}

# For test purpose
# initial_set_n <- 4
# early_stop <- 1



# data
dat <- train_dt %>%  
  dplyr::select(-c(
    "gradient_axis1", "farmerID", "year", "data_type"
  ))

dat_cv <- train_dt_cv
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

rslt_name <- glue::glue("train_pca_dim{i$ndim}_slice{i$nslice}_rslt")
wfl_name <- glue::glue("train_pca_dim{i$ndim}_slice{i$nslice}_wfl")
assign(rslt_name, mdl)
assign(wfl_name, wfl)
save(list = rslt_name, file = glue::glue("data/{rslt_name}.rda"), compress = "xz")
save(list = wfl_name, file = glue::glue("data/{wfl_name}.rda"), compress = "xz")
# Save progress
choice <- choice[-1]
save(choice, file = "data/choice.rda")
# dat_vect <- setdiff(dat_vect, d)
# save(dat_vect, file = here::here("data", "dat_vect.rda")) 





