library(magrittr)

# -------------------------------------------------------------------------------------- #
# Get the data
# -------------------------------------------------------------------------------------- #

if(file.exists("data/dat_vect.rda")){
  data("dat_vect")
}else{
  dat_vect <- c(
    "train_year_full", "train_season_full", "train_season_partial",
    "train_winsum_full", "train_winsum_partial",
    "train_year_corrected_full", "train_season_corrected_full", 
    "train_season_corrected_partial", 
    "train_winsum_corrected_full", "train_winsum_corrected_partial"
  )
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
# Select the variables
# -------------------------------------------------------------------------------------- # 

# -------------------------------------------------------------------------------------- #
# Create recipe specification ####
# -------------------------------------------------------------------------------------- #

get_recipe <- function(dat, type){ 
  
  vars <- names(dat)
  roles <- c("outcome", rep("predictor", length(vars)-1))
  
  recipes::recipe(dat, vars = vars, roles = roles)
  
}  

# -------------------------------------------------------------------------------------- #
# Create model specification ####
# -------------------------------------------------------------------------------------- #


envir <- parsnip::get_model_env()

ls(envir) %>% 
  tibble::tibble(name = .) %>% 
  dplyr::filter(stringr::str_detect(name, "args")) %>% 
  dplyr::mutate(model = stringr::str_replace(name, "_args", ""),
         args  = purrr::map(name, ~envir[[.x]])) %>% 
  tidyr::unnest(args) %>% 
  dplyr::select(model:original) %>% 
  dplyr::filter(engine == "xgboost")

get_model <- function(){
   
  parsnip::boost_tree(
    # tree_depth = 5,
    tree_depth = tune::tune(),
    trees = 5000,
    # trees = tune::tune(),
    stop_iter = 10,
    learn_rate = tune::tune(), 
    mtry = tune::tune(),
    min_n = tune::tune(),
    loss_reduction = tune::tune(),
    sample_size = tune::tune(),
  ) %>% 
    parsnip::set_engine(
      "xgboost", 
      booster = 'gbtree',
      metrics = list("rmse"),
      count = F, # To use proportion for sample_size and mtry instead of full value
      objective = "reg:squarederror"
    ) %>% 
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

get_workflow(get_model(), get_recipe(train_year_full, type = "full")) %>% 
  hardhat::extract_parameter_set_dials() %>% 
  dplyr::pull(object)
  
get_param <- function(wfl){
  
  # max_variable <- wfl$pre$actions$recipe$recipe$var_info %>% nrow
  # max_sample <- wfl$pre$actions$recipe$recipe$template %>% nrow
  
  wfl %>% 
    hardhat::extract_parameter_set_dials() %>% 
    update(
      loss_reduction = dials::loss_reduction(range = c(-10, 1.7)),
      # trees = dials::trees(range = c(1L, 3000L)),
      # mtry = dials::mtry(c(0.1, 0.9)),
      mtry = dials::predictor_prop(c(0.1, 1)),
      sample_size = dials::sample_prop()
    )
  
}


# -------------------------------------------------------------------------------------- #
# Create workflow specification ####
# -------------------------------------------------------------------------------------- #

run_mdl <- function(wfl, dat_cv, param, iter = 150){ 
  mod_wfl_rslt <- tune::tune_bayes(
    object = wfl,
    resamples = dat_cv,
    iter = niter, # CECI globals
    initial = initial_set_n, # CECI globals
    metrics = yardstick::metric_set(yardstick::rmse),
    param_info = param,
    control = tune::control_bayes(verbose = T, 
                                  seed = 42, 
                                  no_improve = early_stop, # CECI globals
                                  uncertain = 5)
  )
  return(mod_wfl_rslt)
}

 


doParallel::registerDoParallel(cores = ncpu)  
options(tidymodels.dark = T)

m <- "xgb"


tictoc::tic()
for(d in dat_vect){
  print(glue::glue("#-------------------     {d}     -------------------#"))
  dat <- get(d) %>% 
    dplyr::select(-c("farmerID", "year")) %>% 
    dplyr::relocate(gradient_axis1, .before = 1)
  # print(dim(dat))
  # To change (branquignole):
  type <- strsplit(d, split = "_")[[1]][2]
  dat_cv <- get(paste(d, "cv", sep = "_"))
  recipe <- get_recipe(dat, type = type)
  mod <- get_model()
  wfl <- get_workflow(mod, recipe)
  
  param <- get_param(wfl)
  mdl <- run_mdl(wfl, dat_cv, param, iter = 150)
  # save result
  mdl_name <- paste(d, m, "rslt", sep = "_")
  assign(mdl_name, mdl)
  save(list = mdl_name,
       file = glue::glue("data/{mdl_name}.rda"),
       compress = "xz")
  # Save workflow
  wfl_name <- paste(d, m, "wfl", sep = "_")
  assign(wfl_name, wfl)
  save(list = wfl_name, 
       file = glue::glue("data/{wfl_name}.rda"),
       compress = "xz") 
  # Save progression
  dat_vect <- dat_vect[-1]
  save(dat_vect, file = "data/dat_vect.rda")
}
tictoc::toc()

if(file.exists("data/dat_vect.rda")) file.remove("data/dat_vect.rda")

# Thank you CECI !