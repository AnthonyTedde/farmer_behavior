library(magrittr)

data("milk_season_partial_augmented")
data("all_data_augmented")
data("clst")
data("pls_final_mdl")

source("globals/global_variables.R")

# -------------------------------------------------------------------------------------- #
# cut the trees
# -------------------------------------------------------------------------------------- #
cut <- 5
grp <- cutree(clst, k = cut)
grp %>% table

milk_season_partial_augmented %<>% 
  dplyr::mutate(
    cluster_idx = grp,
    cluster = glue::glue("cluster{grp}")
  ) %>% 
  dplyr::mutate(
    prd = predict(pls_final_mdl, new_data = milk_season_partial_augmented) %>% 
      dplyr::pull(),
    .before = cluster
  )

# -------------------------------------------------------------------------------------- #
# Compute the pls compnents ####
# -------------------------------------------------------------------------------------- #

pls_raw_prd <- predict(pls_final_mdl, 
                       new_data = milk_season_partial_augmented, 
                       type = "raw")



projection <- pls_raw_prd$variates %>% tibble::as_tibble() %>% 
  setNames(., paste0("pls_dim", stringr::str_pad(seq_along(names(.)), width = 2, pad = 0)))

milk_season_partial_augmented %<>% 
  dplyr::bind_cols(projection)


# -------------------------------------------------------------------------------------- #
# Extract technico-eco ####
# -------------------------------------------------------------------------------------- #
pls_final_data_augmented <- all_data_augmented %>% 
  dplyr::mutate(cluster = 0) %>% 
  dplyr::select(-cluster) %>% 
  dplyr::inner_join(
    milk_season_partial_augmented %>% 
      dplyr::select(farmerID, year, cluster, cluster_idx, prd,
                    dplyr::all_of(names(projection))),
    by = c("farmerID", "year")
  ) %>% 
  dplyr::mutate(cluster = factor(cluster)) %>% 
  dplyr::relocate(cluster, .after = gradient_axis1) %>% 
  dplyr::relocate(data_type, .after = year)
 

if(!file.exists("data/dendr.rda")){ 
  dendr    <- ggdendro::dendro_data(clst, type="rectangle") # convert for ggplot
  
  
  save(dendr, file = "data/dendr.rda", compress = "xz")
}else(
  data("dendr")
)


pls_final_data_augmented %>% 
  ggplot2::ggplot(ggplot2::aes(
    x = gradient_axis1, 
    y = forcats::fct_reorder(cluster, gradient_axis1, .desc = T)
  )) +
  ggplot2::geom_boxplot()

rename_clst_fct <- function(i){
  c("Cluster neutral1", 
    "Cluster intensive",
    "Cluster extensive",
    "Cluster neurtal2",
    "Cluster highly extensive")[i]
}

pls_final_data_augmented %<>% 
  dplyr::mutate(cluster_explicit = rename_clst_fct(cluster_idx) %>% 
                  factor(levels = c("Cluster highly extensive",
                                    "Cluster extensive",
                                    "Cluster neutral1", 
                                    "Cluster neurtal2",
                                    "Cluster intensive" ) ), 
                .after = cluster) %>% 
  dplyr::relocate(cluster_idx, .after = cluster_explicit) %>% 
  dplyr::mutate(gradient_cat = dplyr::case_when(
    gradient_axis1 <= quantile(gradient_axis1, probs = .2) ~ "Highly extensive",
    gradient_axis1 <= quantile(gradient_axis1, probs = .4) ~ "Extensive",
    gradient_axis1 <= quantile(gradient_axis1, probs = .6) ~ "Neutral",
    gradient_axis1 <= quantile(gradient_axis1, probs = .8) ~ "Intensive",
    gradient_axis1 <= quantile(gradient_axis1, probs = 1) ~ "Highly intensive"
  ) %>% factor(
    levels = c("Highly extensive", "Extensive", "Neutral", "Intensive", "Highly intensive")
  ), .after = gradient_axis1)

milk_season_partial_augmented %<>% 
  dplyr::mutate(cluster_explicit = rename_clst_fct(cluster_idx) %>% 
                  factor(levels = c("Cluster highly extensive",
                                    "Cluster extensive",
                                    "Cluster neutral1", 
                                    "Cluster neurtal2",
                                    "Cluster intensive" ) ), 
                .after = cluster) %>% 
  dplyr::select(dplyr::any_of(names(pls_final_data_augmented)))

setdiff(names(pls_final_data_augmented), names(milk_season_partial_augmented))




save(pls_final_data_augmented, 
     file = "data/pls_final_data_augmented.rda",
     compress = "xz")
save(milk_season_partial_augmented, 
     file = "data/milk_season_partial_augmented.rda",
     compress = "xz")
save(rename_clst_fct, file = "data/rename_clst_fct.rda")
  

# -------------------------------------------------------------------------------------- #
# PLS-DA
# -------------------------------------------------------------------------------------- #

# dat <- milk_season_partial_augmented
dat <- all_data_augmented


doParallel::registerDoParallel(cores = ncpu) 
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
    # predictor_prop = tune::tune()
    predictor_prop = 1
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





