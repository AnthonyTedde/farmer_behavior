library(magrittr)

data("train_season_partial")
data("test_season_partial")
data("pls_final_mdl")
data("milk_season_partial")
data("clust")

# -------------------------------------------------------------------------------------- #
# cut the trees
# -------------------------------------------------------------------------------------- #
cut <- 5
grp <- cutree(clst, k = cut)
grp %>% table

all_data_augmented %<>% 
  dplyr::mutate(
    cluster = glue::glue("cluster{grp}")
  )

all_data_augmented %>% 
  ggplot2::ggplot(ggplot2::aes(x = gradient_axis1, y = cluster)) +
  ggplot2::geom_boxplot()
  

# -------------------------------------------------------------------------------------- #
# PLS-DA
# -------------------------------------------------------------------------------------- #



doParallel::registerDoParallel(cores = ncpu) 
options(tidymodels.dark = T)

  
#---------------
# Create the dataset
all_dt <- all_data_augmented %>% 
  dplyr::select(!dplyr::all_of(pca_name)) %>% 
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
cluster_mdl <- run_mdl( wfl,  dat_cv,  param,  iter = niter ) # niter -> CECI globals 
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






