library(magrittr)

data("train_full")
data("train_q20")
data("train_q10")
data("train_q05")
data("test_full")
data("train_full_cv")
data("train_q20_cv")
data("train_q10_cv")
data("train_q05_cv")

source("globals/global_variables.R")  

# -------------------------------------------------------------------------------------- #
# Select the variables
# -------------------------------------------------------------------------------------- # 

compute_importance <- function(){
  all_data <- dplyr::bind_rows(train_full, test_full)
  technico_milk_X <- all_data %>% 
    dplyr::select(dplyr::all_of( 
      milk_potential_predictors[ milk_potential_predictors %in% names(train_full) ]
    ))
  technico_milk_y <- all_data %>% 
    dplyr::select(dplyr::all_of(y)) %>% dplyr::pull()
  
  tictoc::tic()
  rf <- randomForest::randomForest(
    technico_milk_X,  technico_milk_y, 
    ntree=10000, importance = T
  )
  tictoc::toc()
  
  save(rf, file = here::here("data", "rf.rda"))
  return(rf)
}


get_importance <- function(thres = 0){ 
  if(!file.exists("data/rf.rda")){
    rf <- compute_importance()
  }else{
    data(rf)
  }  
  importance <- randomForest::importance(rf)
  # randomForest::varImpPlot(rf)
  names(importance[, 1])[ importance[, 1] >= quantile(importance[, 1],  probs = thres) ]
} 

# -------------------------------------------------------------------------------------- #
# Create recipe specification ####
# -------------------------------------------------------------------------------------- #

get_recipe <- function(dat, thres = 0){ 
  
  form <- get_importance(thres) %>% 
    paste(collapse = " + ") %>% 
    paste(y, ., sep = " ~ ") %>% 
    formula
  
  recipes::recipe(form, data = dat)
  
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
    tree_depth = 5,
    trees = 5000,
    stop_iter = 5,
    learn_rate = tune::tune(), 
    mtry = tune::tune(),
    # min_n = tune::tune(), 
    loss_reduction = tune::tune(),
    sample_size = tune::tune(),
  ) %>% 
    parsnip::set_engine(
      "xgboost", 
      booster = 'gbtree',
      metrics = list("rmse"),
      count = F,
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

get_workflow(get_model(), get_recipe(train_full)) %>% 
  hardhat::extract_parameter_set_dials() %>% 
  dplyr::pull(object)
  
get_param <- function(wfl){
  
  # max_variable <- wfl$pre$actions$recipe$recipe$var_info %>% nrow
  # max_sample <- wfl$pre$actions$recipe$recipe$template %>% nrow
  
  wfl %>% 
    hardhat::extract_parameter_set_dials() %>% 
    update(
      loss_reduction = dials::loss_reduction(range = c(-10, 1.7)),
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
    iter = iter,
    initial = 10,
    metrics = yardstick::metric_set(yardstick::rmse),
    param_info = param,
    control = tune::control_bayes(verbose = T, seed = 42, uncertain = 5)
  )
  return(mod_wfl_rslt)
}



#rec
#mod
#wfl
#upt
#run


doParallel::registerDoParallel(cores = 5)  
options(tidymodels.dark = T)

dat_vect <- c("train_full", "train_q20", "train_q10", "train_q05") 
m <- "xgb"
thres <- seq(0, .9, by = .1)

tictoc::tic()
for(t in thres){ 
  for(d in dat_vect){
    print(glue::glue("#-------------------     {d}/{t}     -------------------#"))
    dat <- get(d)
    dat_cv <- get(paste(d, "cv", sep = "_"))
    recipe <- get_recipe(dat, t)
    mod <- get_model()
    wfl <- get_workflow(mod, recipe)
    
    param <- get_param(wfl)
    mdl <- run_mdl(wfl, dat_cv, param, iter = 150)
    # save result
    loss <- stringr::str_pad(t * 100, width = 3, pad = "0")
    mdl_name <- paste(d, m, loss, "rslt", sep = "_")
    assign(mdl_name, mdl)
    save(list = mdl_name,
         file = glue::glue("data/{mdl_name}.rda"),
         compress = "xz")
    # Save workflow
    wfl_name <- paste(d, m, loss, "wfl", sep = "_")
    assign(wfl_name, wfl)
    save(list = wfl_name, 
         file = glue::glue("data/{wfl_name}.rda"),
         compress = "xz")
  }
}
tictoc::toc()




tune::show_best(pls_mod_wfl_rslt)
one_se <- tune::select_by_one_std_err(pls_mod_wfl_rslt, num_comp, predictor_prop)

pls_fit <- pls_mod_wfl %>% 
  tune::finalize_workflow(one_se) %>% 
  parsnip::fit(dat)


dat %>% 
  broom::augment(x = pls_fit) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = .pred)
dat %>% 
  broom::augment(x = pls_fit) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = .pred)

test_full %>% 
  broom::augment(x = pls_fit) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = .pred)
test_full %>% 
  broom::augment(x = pls_fit) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = .pred)


# -------------------------------------------------------------------------------------- #
# Plot restults ####
# -------------------------------------------------------------------------------------- #

test_full %>% 
  broom::augment(x = pls_fit) %>% 
  ggplot2::ggplot(ggplot2::aes(x = gradient_axis1, y = .pred)) +
  ggplot2::geom_point()