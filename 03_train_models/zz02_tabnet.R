# library(torch)
# library(tabnet)
# library(tidyverse)
# library(tidymodels)
# library(finetune) # to use tuning functions from the new finetune package
# library(vip) # to plot feature importances
library(magrittr)


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


# Create recipe #### 
get_recipe <- function(dat, thres = 0){ 
  
  form <- get_importance(thres) %>% 
    paste(collapse = " + ") %>% 
    paste(y, ., sep = " ~ ") %>% 
    formula
  
  recipes::recipe(form, data = dat) %>% 
    bestNormalize::step_orderNorm()
  
}  
 

set.seed(777)
torch::torch_manual_seed(777)
  

# Model specification
tune::tunable(tabnet::tabnet() %>% parsnip::set_engine("torch"))

mod <- tabnet::tabnet(
  epochs = tune::tune(), 
  batch_size = tune::tune(), 
  decision_width = tune::tune(), 
  attention_width = tune::tune(),
  num_steps = tune::tune(), 
  penalty = tune::tune(),  
  momentum = tune::tune(),
  feature_reusage = tune::tune(), 
  virtual_batch_size = tune::tune(),
  learn_rate = tune::tune(),
  num_shared = tune::tune()
) %>%
  parsnip::set_engine("torch", verbose = TRUE) %>%
  parsnip::set_mode("regression")


# Create workfow
wf <- workflows::workflow() %>%
  workflows::add_model(mod) %>%
  workflows::add_recipe(rec)

grid <- wf %>%
  # dials::parameters() %>%
  hardhat::extract_parameter_set_dials() %>% 
  update(
    decision_width = tabnet::decision_width(range = c(20, 40)),
    attention_width = tabnet::attention_width(range = c(20, 40)),
    num_steps = tabnet::num_steps(range = c(3, 6)),
    learn_rate = dials::learn_rate(range = c(-2.5, -1)),
    batch_size = dials::batch_size(range = c(6, 9)),
    virtual_batch_size = dials::batch_size(range = c(6, 9))
  ) %>%
  dials::grid_max_entropy(size = 64)


options(tidymodels.dark = TRUE)
ctrl <- finetune::control_race(verbose = T, verbose_elim = TRUE)
folds <- technico_milk_cv
set.seed(777)

tictoc::tic()
res <- wf %>% 
  finetune::tune_race_anova(
    resamples = folds, 
    grid = grid,
    control = ctrl
  )
tictoc::toc()




mod <- tabnet::tabnet(
  epochs = 600, 
  batch_size = 405, 
  decision_width = 35, 
  attention_width = 35,
  num_steps = 4, 
  penalty = 0.1,  
  momentum = 0.2,
  feature_reusage = 1.42, 
  virtual_batch_size = 200,
  learn_rate = 0.05,
  num_shared = 2
) %>%
  parsnip::set_engine("torch", verbose = TRUE) %>%
  parsnip::set_mode("regression")

wf <- workflows::workflow() %>%
  workflows::add_model(mod) %>%
  workflows::add_recipe(rec)

fitted_model <- wf %>% 
  parsnip::fit(technico_milk_train)



preds_train <- technico_milk_train %>% 
  dplyr::bind_cols(predict(fitted_model, technico_milk_train))
preds_test <- technico_milk_test %>% 
  dplyr::bind_cols(predict(fitted_model, technico_milk_test))

yardstick::rsq(preds_train, gradient_axis1, .pred)
yardstick::rmse(preds_train, gradient_axis1, .pred)
yardstick::rsq(preds_test, gradient_axis1, .pred)
yardstick::rmse(preds_test, gradient_axis1, .pred)






