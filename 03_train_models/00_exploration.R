library(magrittr)
data("train_full")
data("test_full")

source("globals/global_variables.R")
 

form <- milk_potential_predictors[milk_potential_predictors %in% names(train_full)] %>% 
  paste0("s(", ., ")") %>% 
  paste(collapse = " + ") %>% 
  paste("gradient_axis1", ., sep = " ~ ") %>% 
  formula

tictoc::tic()
technico_gam <- mgcv::gam(form, data = train_full,
                          method = "REML", gamma = 2)
tictoc::toc()

summary(technico_gam)

train_full %>% 
  dplyr::mutate(
    pred = predict(technico_gam) 
  ) %>% 
  # yardstick::rsq(truth = "gradient_axis1", estimate = "pred")
  yardstick::rmse(truth = "gradient_axis1", estimate = "pred")

test_full %>% 
  dplyr::mutate(
    pred = predict(technico_gam, newdata = test_full) 
  ) %>% 
  # dplyr::filter(pred > -5, pred < 5) %>% 
  # yardstick::rsq(truth = "gradient_axis1", estimate = "pred")
  yardstick::rmse(truth = "gradient_axis1", estimate = "pred")

technico_milk_train %>% 
  dplyr::mutate(
    pred = predict(technico_gam) 
  ) %>% 
  ggplot2::ggplot(ggplot2::aes(x = pred, y = gradient_axis1)) +
  ggplot2::geom_point()

technico_milk_test %>% 
  dplyr::mutate(
    pred = predict(technico_gam, newdata = technico_milk_test) 
  ) %>% 
  # dplyr::filter(pred > -5, pred < 5) %>% 
  ggplot2::ggplot(ggplot2::aes(x = pred, y = gradient_axis1)) +
  ggplot2::geom_point()

plot(technico_milk$pNAGase_milk, technico_milk$gradient_axis1)
plot(technico_milk$fat, technico_milk$gradient_axis1)

#-----------------------------------------------------------------------------------------
# PLS
#-----------------------------------------------------------------------------------------
formula <- predictors %>% 
  paste(collapse = " + ") %>% 
  paste("gradient_axis1", ., sep = " ~ ") %>% 
  formula


predictors2 <- c(
  "fat",
  "protein",
  "lactose",
  "pmilk",
  "pch4_full",
  "pBW",
  "pC4", 
  "pC14",
  "PC14.1", 
  "pC18.1_trs__tot",
  "pC18.1_c9",
  "pC18.1_c9__tot", 
  "pC18.1__tot",
  "pNa",
  "pCa",
  "pP",
  "pMg",
  "pK",
  "pBOHB_milk",
  "pglucose_6P_milk",
  "purea_milk",
  "pNAGase_milk",
  "pA30_CRM",
  "pK20_CRM",
  "pRCT_jr",
  "ppH_botter",
  "ppH",
  "pNa", 
  "PFA_o6"
  )

library(splines)
formula <- predictors2 %>% 
  paste0("ns(", ., ", df = 3)") %>% 
  paste(collapse = " + ") %>% 
  paste("gradient_axis1", ., sep = " ~ ") %>% 
  formula

library(splines)
formula <- milk_col_names %>% 
  # paste0("ns(", ., ", df = 3)") %>% 
  paste(collapse = " + ") %>% 
  paste("gradient_axis1", ., sep = " ~ ") %>% 
  formula

ncp <- 100
technico_pls <- pls::mvr(formula, data = technico_milk_train,  ncomp = ncp, 
                         scale = T, center = T,
                         # method = pls.options()$plsralg)
                         method = pls.options()$cpplsalg)

technico_pls <- lm(formula, data = technico_milk_train)

# summary(technico_pls)

technico_milk_train %>% 
  dplyr::mutate(
    pred = predict(technico_pls, ncomp = ncp ) %>% drop
  ) %>% 
  # yardstick::rsq(truth = "gradient_axis1", estimate = "pred")
  yardstick::rmse(truth = "gradient_axis1", estimate = "pred")

technico_milk_test %>% 
  dplyr::mutate(
    pred = predict(technico_pls, newdata = technico_milk_test, ncomp = ncp) %>% drop
  ) %>% 
  # dplyr::filter(pred > -5, pred < 5) %>% 
  # yardstick::rsq(truth = "gradient_axis1", estimate = "pred")
  yardstick::rmse(truth = "gradient_axis1", estimate = "pred")

technico_milk_train %>% 
  dplyr::mutate(
    pred = predict(technico_pls, ncomp = ncp ) %>% drop
  ) %>% 
  ggplot2::ggplot(ggplot2::aes(x = pred, y = gradient_axis1)) +
  ggplot2::geom_point()

technico_milk_test %>% 
  dplyr::mutate(
    pred = predict(technico_pls, newdata = technico_milk_test, ncomp = ncp) %>% drop
  ) %>% 
  # dplyr::filter(pred > -5, pred < 5) %>% 
  ggplot2::ggplot(ggplot2::aes(x = pred, y = gradient_axis1)) +
  ggplot2::geom_point()

plot(technico_milk$pNAGase_milk, technico_milk$gradient_axis1)
plot(technico_milk$fat, technico_milk$gradient_axis1)


# ----------------------------------------------------------------------------------------
# XGBoost
# ----------------------------------------------------------------------------------------
data("predictor_to_keep")
library(xgboost)

train_xgb <- xgb.DMatrix(as.matrix(technico_milk_train[, predictors]),
                         label = technico_milk_train[, "gradient_axis1", drop = T])
 
test_xgb <- xgb.DMatrix(as.matrix(technico_milk_test[, predictors]),
                         label = technico_milk_test[, "gradient_axis1", drop = T])

train_xgb <- xgb.DMatrix(as.matrix(technico_milk_train[, predictors2]),
                         label = technico_milk_train[, "gradient_axis1", drop = T])
 
test_xgb <- xgb.DMatrix(as.matrix(technico_milk_test[, predictors2]),
                         label = technico_milk_test[, "gradient_axis1", drop = T])

train_xgb <- xgb.DMatrix(as.matrix(technico_milk_train[, milk_col_names]),
                         label = technico_milk_train[, "gradient_axis1", drop = T])
 
test_xgb <- xgb.DMatrix(as.matrix(technico_milk_test[, milk_col_names]),
                         label = technico_milk_test[, "gradient_axis1", drop = T])

train_xgb <- xgb.DMatrix(as.matrix(technico_milk_train[, predictor_to_keep]),
                         label = technico_milk_train[, "gradient_axis1", drop = T]) 
test_xgb <- xgb.DMatrix(as.matrix(technico_milk_test[, predictor_to_keep]),
                         label = technico_milk_test[, "gradient_axis1", drop = T])
# ---------------------------------------------------------------------------- #
# -- gbtree -- ####

tree_cv <- xgb.cv(
  data = train_xgb,
  nrounds = 1000, nthread = 5,
  nfold = 10,
  metrics = list("rmse"),
  early_stopping_rounds = 5,
  # -----------
  # parameters
  # -----------
  booster = 'gbtree', # The default and the one supported by parsnip
  # booster = 'gblinear', # The default and the one supported by parsnip
  max_depth = 50, # tree_depth
  # eta = .02, # learning_rate [-10, 0, log10]
  eta = .02, # learning_rate [-10, 0, log10]
  colsample_bynode = .9, # mtry [0.1, 1] (Column subsampling)
  # colsample_bylevel = .5,
  # colsample_bytree = .9,
  subsample = .9, # sample_size [.1, 1] (Row subsampling)
  gamma = 30, # loss_reduction
  min_child_weight = 0, # min_split_loss [0, 300]
  lambda = 50, # set_engine->lambda, dials->penalty_L2
  alpha = 00, # set_engine->alpha, dials->penalty_L1
  # num_parallel_tree = 5,
  # max_depth = 6,
  # objective = "reg:squaredlogerror"
  objective = "reg:squarederror"
  # objective = "reg:gamma"
)

fit <- xgb.train(params = tree_cv$params,
                 data = train_xgb,
                 nrounds = tree_cv$best_iteration)


technico_milk_train$y_hat <- predict(fit, newdata = train_xgb)
technico_milk_test$y_hat <- predict(fit, newdata = test_xgb)

# R2
technico_milk_train %>% 
  yardstick::rsq(truth = "gradient_axis1", estimate = "y_hat")
technico_milk_test %>% 
  yardstick::rsq(truth = "gradient_axis1", estimate = "y_hat")
# RMSE
technico_milk_train %>% 
  yardstick::rmse(truth = "gradient_axis1", estimate = "y_hat")
technico_milk_test %>% 
  yardstick::rmse(truth = "gradient_axis1", estimate = "y_hat")


ggplot2::ggplot(data = technico_milk_train, 
                mapping = ggplot2::aes(y = y_hat, x = gradient_axis1)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red")

ggplot2::ggplot(data = technico_milk_test, 
                mapping = ggplot2::aes(y = y_hat, x = gradient_axis1)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red")




