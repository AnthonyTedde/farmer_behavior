library(magrittr)

data("cut_df")
data("all_data_augmented")

source("globals/global_variables.R")


# -------------------------------------------------------------------------------------- #
# Create variable
# -------------------------------------------------------------------------------------- #

# max_comp is defined in file 03_train_models/clustering/01_create_cluster.R
max_comp <- 55
pca_name <- paste0("dim", stringr::str_pad(seq_len(max_comp), width = 2, pad = "0"))
ndim <- c(2, 3, 4, seq(5, max_comp, by = 5))

# -------------------------------------------------------------------------------------- #
# PLS-DA
# -------------------------------------------------------------------------------------- #

if(file.exists("data/choice.rda")){
  data("choice")
}else{ 
  choice <- list(ndim = ndim,
                 nslice = 2:5) %>% 
    purrr::cross()
}


doParallel::registerDoParallel(cores = ncpu) 
options(tidymodels.dark = T)

for(i in choice){

  keep_dim_pca <- pca_name[seq_len(i$ndim)]
  max_dim_pca <- pca_name[i$ndim]
  
  #---------------
  # Create the slice
  slice <- paste0("slice", i$nslice)
  slice_df <- cut_df %>% 
    dplyr::filter(comp_pca == max_dim_pca) %>% 
    dplyr::select(dplyr::one_of(slice)) %>% 
    dplyr::rename(slice = !!slice)
  
  #---------------
  # Create the dataset
  all_dt <- all_data_augmented %>% 
    dplyr::bind_cols(slice_df) %>% 
    dplyr::mutate(cluster = paste0("cluster", slice) %>% 
                    factor(levels = paste0("cluster", seq_len(i$nslice)))) %>% 
    dplyr::relocate(cluster, .before = 1)
  
  train_dt <- all_dt %>% 
    dplyr::filter(data_type == "train")
  test_dt <- all_dt %>% 
    dplyr::filter(data_type == "test")
  
  #---------------
  # Create cross_validation sets
  train_dt_cv <- rsample::group_vfold_cv(
    train_dt, 
    group = farmerID,
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
        num_comp = dials::num_comp(range = c(1L, 99L))
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
  dat <- train_dt %>%  
    dplyr::select(-c(
      dplyr::all_of(pca_name), "slice", "gradient_axis1", "prd", "farmerID", "year",
      "data_type"
    ))
  
  dat_cv <- train_dt_cv
  # Get specifications
  recipe <- get_recipe(dat)
  mod <- get_model()
  wfl <- get_workflow(mod, recipe) 
  param <- get_param(wfl)
  # Run models 
  tictoc::tic()
  mdl <- run_mdl( wfl,  dat_cv,  param,  iter = niter ) # niter -> CECI globals 
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
  
}

file.remove("data/choice.rda")






