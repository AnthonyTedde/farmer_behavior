# Year effect ? 

data("working_dat")
data("train_full")
data("test_full")

source("globals/global_variables.R")

working_dat_mean <- working_dat %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(dplyr::across(
    .cols = -c("gradient_axis1", "farmerID"), 
    .fns = function(dat) dat - mean(dat)
  ) ) %>% 
  dplyr::ungroup()

train_full_mean <- working_dat_mean %>% 
  dplyr::inner_join(train_full %>% dplyr::select(farmerID, year))
test_full_mean <- working_dat_mean %>% 
  dplyr::inner_join(test_full %>% dplyr::select(farmerID, year))

# formula 
form <- milk_potential_predictors[milk_potential_predictors %in% names(dat)] %>% 
  paste(collapse = " + ") %>% 
  paste(y, ., sep = " ~ ") %>% 
  formula

# full pls
pls_full <- pls::mvr(form, ncomp = 30, data = train_full, 
                     scale = T, center = T,
                     method = pls::pls.options()$plsralg)

train_full_augmented <- train_full %>% 
  tibble::add_column(
    .pred = predict(pls_full, ncomp = 30, newdata = train_full) %>% drop
  )
test_full_augmented <- test_full %>% 
  tibble::add_column(
    .pred = predict(pls_full, ncomp = 30, newdata = test_full) %>% drop
  )

train_full_augmented %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = .pred)
train_full_augmented %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = .pred)
test_full_augmented %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = .pred)  


# mean pls
pls_mean <- pls::mvr(form, ncomp = 30, data = train_full_mean, 
                     scale = T, center = T,
                     method = pls::pls.options()$plsralg)

train_full_mean_augmented <- train_full_mean %>% 
  tibble::add_column(
    .pred = predict(pls_mean, ncomp = 30, newdata = train_full_mean) %>% drop
  )
test_full_mean_augmented <- test_full_mean %>% 
  tibble::add_column(
    .pred = predict(pls_mean, ncomp = 30, newdata = test_full_mean) %>% drop
  )

train_full_mean_augmented %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = .pred)
train_full_mean_augmented %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = .pred)
test_full_mean_augmented %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = .pred)  
  





