library(magrittr)

# train
data("train_full")
data("train_quarter_full")
# cv
data("train_full_cv")
data("train_quarter_full_cv")
# test
data("test_full")
data("test_quarter_full")
# data("train_q20")
# data("train_q10")
# data("train_q05")
# data("train_q20_cv")
# data("train_q10_cv")
# data("train_q05_cv")

source("globals/global_variables.R")  

# -------------------------------------------------------------------------------------- #
# Select the variables
# -------------------------------------------------------------------------------------- # 

# CECI send rf_full and rf_quarter to server
compute_importance <- function(dat, type){
  all_data <- dat
  technico_milk_X <- all_data %>% dplyr::select(!dplyr::any_of(y))
  technico_milk_y <- all_data %>% dplyr::select(dplyr::all_of(y)) %>% dplyr::pull()
  
  tictoc::tic()
  rf <- randomForest::randomForest(
    technico_milk_X,  technico_milk_y, 
    mtry = floor(ncol(dat) / 3),
    ntree=10000, importance = T
  )
  tictoc::toc()
  
  mdl_name <- paste("rf", type, sep = "_")
  assign(mdl_name, rf)
  save(list = mdl_name, 
       file = here::here("data", glue::glue("{mdl_name}.rda")),
       compress = "xz")
  return(rf)
} 

get_importance <- function(thres = 0, dat, type){ 
  mdl_name <- paste("rf", type, sep = "_")
  if(!file.exists(glue::glue("data/{mdl_name}.rda"))){
    rf <- compute_importance(dat, type)
    assign(mdl_name, rf)
  }else{
    data(list = mdl_name)
  }  
  rf <- get(mdl_name)
  importance <- randomForest::importance(rf)
  return(
    names(importance[, 1])[ importance[, 1] >= quantile(importance[, 1],  probs = thres) ]
  ) 
} 

# Test
# working_full_dat <-  dplyr::bind_rows(train_full, test_full) %>%
#   dplyr::select(-c("farmerID", "year"))
# working_quarer_dat <- dplyr::bind_rows(train_quarter_full, test_quarter_full) %>%
#   dplyr::select(-c("farmerID", "year"))
# 
# get_importance_full <- get_importance(thres = .6, dat = working_full_dat,
#                                       type = "full")
# get_importance_quarter <- get_importance(dat = working_quarer_dat,
#                                          type = "quarter")
# 
# data("rf_full")
# data("rf_quarter")
# randomForest::varImpPlot(rf_full)
# randomForest::varImpPlot(rf_quarter)

# -------------------------------------------------------------------------------------- #
# Create recipe specification ####
# -------------------------------------------------------------------------------------- #

get_recipe <- function(dat, thres = 0, type){ 
  
  vars <- c(y, get_importance(thres, dat, type))
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
    # trees = 5000,
    trees = tune::tune(),
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

get_workflow(get_model(), get_recipe(train_full, type = "full")) %>% 
  hardhat::extract_parameter_set_dials() %>% 
  dplyr::pull(object)
  
get_param <- function(wfl){
  
  # max_variable <- wfl$pre$actions$recipe$recipe$var_info %>% nrow
  # max_sample <- wfl$pre$actions$recipe$recipe$template %>% nrow
  
  wfl %>% 
    hardhat::extract_parameter_set_dials() %>% 
    update(
      loss_reduction = dials::loss_reduction(range = c(-10, 1.7)),
      trees = dials::trees(range = c(1L, 3000L)),
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

# dat_vect <- c("train_full", "train_q20", "train_q10", "train_q05") 
m <- "xgb"

if(file.exists("data/step.rda")){
  data("step")
}else{
  dat_vect <- c("train_full", "train_quarter_full")  
  thres <- seq(0, .9, by = .1) # CECI by change by .1 
  step <- purrr::cross(list(d = dat_vect, t = thres)) 
}

tictoc::tic()
for(s in step){
  print(glue::glue("#-------------------     {s$d}/{s$t}     -------------------#"))
  dat <- get(s$d)
  print(dim(dat))
  # To change (branquignole):
  type <- ifelse(length(strsplit(s$d, split = "_")[[1]]) == 3, "quarter", "full")
  dat_cv <- get(paste(s$d, "cv", sep = "_"))
  recipe <- get_recipe(dat, s$t, type = type)
  mod <- get_model()
  wfl <- get_workflow(mod, recipe)
  
  param <- get_param(wfl)
  mdl <- run_mdl(wfl, dat_cv, param, iter = 150)
  # save result
  loss <- stringr::str_pad(s$t * 100, width = 3, pad = "0")
  mdl_name <- paste(s$d, m, loss, "rslt", sep = "_")
  assign(mdl_name, mdl)
  save(list = mdl_name,
       file = glue::glue("data/{mdl_name}.rda"),
       compress = "xz")
  # Save workflow
  wfl_name <- paste(s$d, m, loss, "wfl", sep = "_")
  assign(wfl_name, wfl)
  save(list = wfl_name, 
       file = glue::glue("data/{wfl_name}.rda"),
       compress = "xz") 
  # Save progression
  step <- step[-1]
  save(step, file = "data/step.rda")
}
tictoc::toc()

if(file.exists("data/step.rda")) file.remove("data/step.rda")

# Thank you CECI !