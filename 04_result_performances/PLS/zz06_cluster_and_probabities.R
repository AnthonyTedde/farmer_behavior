library(magrittr)

data("train_season_partial")
data("test_season_partial")
data("pls_final_mdl")

source("globals/global_variables.R")

train_pca_dt <- train_season_partial %>% 
  dplyr::select(-c("gradient_axis1", "farmerID",  "year"))
test_pca_dt <- test_season_partial %>% 
  dplyr::select(-c("gradient_axis1", "farmerID",  "year"))

# -------------------------------------------------------------------------------------- #
# Compute PCA
# -------------------------------------------------------------------------------------- #

train_pca <- FactoMineR::PCA(train_pca_dt, ncp = 20, graph = F)
max_comp <- min(which(train_pca$eig[, 3] > 99))
train_pca <- FactoMineR::PCA(train_pca_dt, ncp = max_comp, graph = F)

train_predict <- predict(train_pca, newdata = train_pca_dt)
test_predict <- predict(train_pca, newdata = test_pca_dt)

pca_name <- paste0("dim", stringr::str_pad(seq_len(max_comp), width = 2, pad = "0"))


# -------------------------------------------------------------------------------------- #
# Augment train and test
# -------------------------------------------------------------------------------------- #

train_season_partial_augmented <- train_season_partial %>% 
  dplyr::bind_cols(
    train_predict$coord %>% 
      tibble::as_tibble() %>% 
      setNames(pca_name)
  ) %>% 
  dplyr::mutate(
    prd = predict(pls_final_mdl, new_data = train_season_partial) %>% dplyr::pull(),
    .after = gradient_axis1
  ) %>% 
  tibble::add_column(data_type = "train")

test_season_partial_augmented <- test_season_partial %>% 
  dplyr::bind_cols(
    test_predict$coord %>% 
      tibble::as_tibble() %>% 
      setNames(pca_name)
  ) %>% 
  dplyr::mutate(
    prd = predict(pls_final_mdl, new_data = test_season_partial) %>% dplyr::pull(),
    .after = gradient_axis1
  ) %>% 
  tibble::add_column(data_type = "test")

## Bind train and test
all_data_augmented <- dplyr::bind_rows(
  train_season_partial_augmented,
  test_season_partial_augmented
)

# -------------------------------------------------------------------------------------- #
# Compute hclust
# -------------------------------------------------------------------------------------- #


ndim <- c(2, 3, 4, seq(5, max_comp, by = 5))
clst_lst <- vector(mode = "list", length = length(ndim))

for ( i in seq_along(ndim) ){
  n <- ndim[i]
  keep_dim_pca <- pca_name[seq_len(n)]
  dat <- all_data_augmented %>% 
    dplyr::select(dplyr::all_of(keep_dim_pca))
  data_dist <- dist(dat)   
  clst <- hclust(data_dist, method = "ward.D")
  clst_lst[[i]] <- clst
}


# -------------------------------------------------------------------------------------- #
# cut the trees
# -------------------------------------------------------------------------------------- #


cut_df <- seq_along(ndim) %>% purrr::map_dfr(.f = function(i){ 
  clst <- clst_lst[[i]]
  n <- ndim[i]
  keep_dim_pca <- pca_name[seq_len(n)]
  slice <- 2:5
  slice %>% purrr::map_dfc(.f = function(c){
    cutree(clst, k = c)
  }) %>% 
    setNames(paste0("slice", slice)) %>% 
    dplyr::mutate(comp_pca = dplyr::last(keep_dim_pca))
})


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
  
  
  run_mdl <- function(wfl, dat_cv, param, hyper){ 
    pls_mod_wfl_rslt <- tune::tune_grid(
      object = wfl,
      resamples = dat_cv,
      # metrics = yardstick::metric_set(yardstick::roc_auc),
      param_info = param,
      grid = hyper,
      # grid = 2,
      control = tune::control_grid( 
        verbose = T,
        save_workflow = T
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
  set.seed(1010)
  hyperparam <- dials::grid_latin_hypercube(
    param,
    size = latin_size,
    original = TRUE
  )
  # hyperparam %<>% dplyr::mutate(num_comp = 2)
  # Run models 
  tictoc::tic()
  mdl <- run_mdl( wfl,  dat_cv,  param,  hyperparam ) # niter -> CECI globals 
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
  
}

file.remove("data/choice.rda")







