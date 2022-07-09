library(magrittr)

data("account_milk_test_full")
data("account_milk_train_full")
data("technico_milk")
data("working_dat")

source("globals/global_variables.R")

any(account_milk_test_full$CFERME %in% account_milk_train_full$CFERME)
any(account_milk_train_full$CFERME %in% account_milk_test_full$CFERME)

dat_ATE <- technico_milk %>% dplyr::select(farmerID, CFERME) %>% 
  dplyr::inner_join(working_dat) %>% 
  dplyr::select(CFERME, year, gradient_axis1)

dat_gui <- dplyr::bind_rows(account_milk_test_full, account_milk_train_full) %>% 
  dplyr::select(CFERME, year, coord_dim1)

dat_merge <- dplyr::inner_join(dat_ATE, dat_gui) 

dat_merge %>% 
  ggplot2::ggplot(ggplot2::aes(x = gradient_axis1, coord_dim1)) +
  ggplot2::geom_point()

with(dat_merge, cor(gradient_axis1, coord_dim1))


account_milk_train_full$coord_dim1 %>% hist
account_milk_test_full$coord_dim1 %>% hist

# pls

# form <- setdiff(milk_col_names, c("farmerID", "year")) %>% 
form <- names(account_milk_train_full[, 20:135]) %>% 
  paste(collapse = " + ") %>% 
  paste("coord_dim1", ., sep = " ~ ") %>% 
  formula

mdl <- pls::mvr(form, data = account_milk_train_full, 
                ncomp = 10, method = pls::pls.options()$plsralg, scale = T)

account_milk_train_full %<>% 
  dplyr::mutate(
    pred =  predict(mdl, newdata = account_milk_train_full, ncomp = 10) %>% drop
  )
account_milk_test_full %<>% 
  dplyr::mutate(
    pred =  predict(mdl, newdata = account_milk_test_full, ncomp = 10) %>% drop
  )

account_milk_train_full %>% 
  yardstick::rsq(truth = coord_dim1, estimate = pred)
account_milk_train_full %>% 
  yardstick::rmse(truth = coord_dim1, estimate = pred)
account_milk_test_full %>% 
  yardstick::rsq(truth = coord_dim1, estimate = pred)
account_milk_test_full %>% 
  yardstick::rmse(truth = coord_dim1, estimate = pred)







# form <- setdiff(milk_col_names, c("farmerID", "year")) %>%
form <- milk_potential_predictors[milk_potential_predictors %in% names(train_full)] %>% 
  paste(collapse = " + ") %>% 
  paste("gradient_axis1", ., sep = " ~ ") %>% 
  formula


mdl <- pls::mvr(form, data = train_full, 
                ncomp = 10, method = pls::pls.options()$plsralg, scale = T)

train_full %<>% 
  dplyr::mutate(
    pred =  predict(mdl, newdata = train_full, ncomp = 10) %>% drop
  )
test_full %<>% 
  dplyr::mutate(
    pred =  predict(mdl, newdata = test_full, ncomp = 10) %>% drop
  )

train_full %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = pred)
train_full %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = pred)
test_full %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = pred)
test_full %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = pred)


test_full %>% 
  ggplot2::ggplot(ggplot2::aes(x = gradient_axis1, y = pred)) +
  ggplot2::geom_point()
