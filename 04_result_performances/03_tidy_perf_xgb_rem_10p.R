library(magrittr)

library(plsmod)
library(bestNormalize)

dat_vect <- c("train_full", "train_q20", "train_q10", "train_q05") 

# Load data
data("train_full")
data("train_q20")
data("train_q10")
data("train_q05")
data("test_full")

# Load models results
data("train_full_xgb_rem_10p_rslt")
data("train_q20_xgb_rem_10p_rslt")
data("train_q10_xgb_rem_10p_rslt")
data("train_q05_xgb_rem_10p_rslt")

# Load workflow
data("train_full_xgb_rem_10p_wfl")
data("train_q20_xgb_rem_10p_wfl")
data("train_q10_xgb_rem_10p_wfl")
data("train_q05_xgb_rem_10p_wfl")


# -------------------------------------------------------------------------------------- #
# Compute fitted models
# -------------------------------------------------------------------------------------- #

for(d in dat_vect){
  
  rslt <- get(paste(d, "xgb_rem_10p", "rslt", sep = "_"))
  bst <- rslt %>% tune::select_best(metric = "rmse")
  onese <- rslt %>% tune::select_by_one_std_err(
    metric = "rmse", 
    dplyr::desc(loss_reduction)
  )
  
  dat <- get(d)
  
  # fit bst
  fit_bst <- get(paste(d, "xgb_rem_10p", "wfl", sep = "_")) %>% 
    tune::finalize_workflow(bst) %>% 
    parsnip::fit(dat)
  # fit onese
  fit_onese <- get(paste(d, "xgb_rem_10p", "wfl", sep = "_")) %>% 
    tune::finalize_workflow(onese) %>% 
    parsnip::fit(dat)
  
  fit_bst_name <- paste(d, "xgb_rem_10p_fit_bst", sep = "_") 
  assign(fit_bst_name, fit_bst)
  
  fit_onese_name <- paste(d, "xgb_rem_10p_fit_onese", sep = "_") 
  assign(fit_onese_name, fit_onese)
  
  # Save
  save(list = fit_bst_name, 
       file = glue::glue("data/{fit_bst_name}.rda"),
       compress = "xz")
  save(list = fit_onese_name, 
       file = glue::glue("data/{fit_onese_name}.rda"),
       compress = "xz")
  
}


# -------------------------------------------------------------------------------------- #
# Compute the performances
# -------------------------------------------------------------------------------------- #

dat_vect
fit_vect <- c("bst", "onese")
mod_vect <- "xgb_rem_10p"



perf_lst <- purrr::cross( list(dat = dat_vect, 
                              fit = fit_vect, 
                              mod = mod_vect) ) %>% 
  purrr::map(.f = function(d){ 
    #
    dat_train <- get(d$dat)
    dat_type <- strsplit(d$dat, split = "_")[[1]][length(strsplit(d$dat, split = "_")[[1]])]
    mod <- get(glue::glue("{d$dat}_{d$mod}_fit_{d$fit}"))
    # Test and train output
    list(
      train = dat_train %>% 
        dplyr::mutate(
          perf_type = "train",
          data_type = dat_type,
          fit_type = d$fit,
          prd = predict(mod, new_data = dat_train) %>% dplyr::pull()
        ),
      test = test_full %>% 
        dplyr::mutate(
          perf_type = "test",
          data_type = dat_type,
          fit_type = d$fit,
          prd = predict(mod, new_data = test_full) %>% dplyr::pull()
        )
    ) 
  })


perf_lst %<>% 
  purrr::transpose() %>% 
  purrr::map(purrr::reduce, dplyr::bind_rows)

perf_lst$train %>% 
  dplyr::group_by(perf_type, data_type, fit_type) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = prd)
perf_lst$train %>% 
  dplyr::group_by(perf_type, data_type, fit_type) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = prd)

perf_lst$test %>% 
  dplyr::group_by(perf_type, data_type, fit_type) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = prd)
perf_lst$test %>% 
  dplyr::group_by(perf_type, data_type, fit_type) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = prd)

perf_lst$train %>% 
  dplyr::filter(data_type %in% c("full", "q20")) %>% 
  ggplot2::ggplot(ggplot2::aes(x = prd, y = gradient_axis1)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "darkred") + 
  ggplot2::facet_grid(data_type ~ fit_type)

perf_lst$test %>% 
  dplyr::filter(data_type %in% c("full", "q20")) %>% 
  ggplot2::ggplot(ggplot2::aes(x = prd, y = gradient_axis1)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "darkred") + 
  ggplot2::facet_grid(data_type ~ fit_type)



fit <- train_full_pls_fit_bst$fit$fit$fit
fit$keepX
fit$ncomp

var <- fit$loadings$X %>% rownames()
fit$loadings.star
c(
  fit$loadings$X[, 1][fit$loadings$X[, 1] > 0] %>% names,
  fit$loadings$X[, 2][fit$loadings$X[, 2] > 0] %>% names,
  fit$loadings$X[, 3][fit$loadings$X[, 3] > 0] %>% names,
  fit$loadings$X[, 4][fit$loadings$X[, 4] > 0] %>% names,
  fit$loadings$X[, 5][fit$loadings$X[, 5] > 0] %>% names,
  fit$loadings$X[, 6][fit$loadings$X[, 6] > 0] %>% names,
  fit$loadings$X[, 7][fit$loadings$X[, 7] > 0] %>% names,
  fit$loadings$X[, 8][fit$loadings$X[, 8] > 0] %>% names,
  fit$loadings$X[, 9][fit$loadings$X[, 9] > 0] %>% names,
  fit$loadings$X[, 10][fit$loadings$X[, 10] > 0] %>% names,
  fit$loadings$X[, 11][fit$loadings$X[, 11] > 0] %>% names,
  fit$loadings$X[, 12][fit$loadings$X[, 12] > 0] %>% names
)
mean(c(
  fit$loadings$X[, 1][fit$loadings$X[, 1] > 0] %>% length,
  fit$loadings$X[, 2][fit$loadings$X[, 2] > 0] %>% length,
  fit$loadings$X[, 3][fit$loadings$X[, 3] > 0] %>% length,
  fit$loadings$X[, 4][fit$loadings$X[, 4] > 0] %>% length,
  fit$loadings$X[, 5][fit$loadings$X[, 5] > 0] %>% length,
  fit$loadings$X[, 6][fit$loadings$X[, 6] > 0] %>% length,
  fit$loadings$X[, 7][fit$loadings$X[, 7] > 0] %>% length,
  fit$loadings$X[, 8][fit$loadings$X[, 8] > 0] %>% length,
  fit$loadings$X[, 9][fit$loadings$X[, 9] > 0] %>% length,
  fit$loadings$X[, 10][fit$loadings$X[, 10] > 0] %>% length,
  fit$loadings$X[, 11][fit$loadings$X[, 11] > 0] %>% length,
  fit$loadings$X[, 12][fit$loadings$X[, 12] > 0] %>% length
))
 
c(
  fit$loadings.star[[1]][, 1][fit$loadings.star[[1]][, 1] > 0] %>% names,
  fit$loadings.star[[1]][, 2][fit$loadings.star[[1]][, 2] > 0] %>% names,
  fit$loadings.star[[1]][, 3][fit$loadings.star[[1]][, 3] > 0] %>% names,
  fit$loadings.star[[1]][, 4][fit$loadings.star[[1]][, 4] > 0] %>% names
)

mean(c( 
  length(fit$loadings.star[[1]][, 1][fit$loadings.star[[1]][, 1] > 0] %>% names),
  length(fit$loadings.star[[1]][, 2][fit$loadings.star[[1]][, 2] > 0] %>% names),
  length(fit$loadings.star[[1]][, 3][fit$loadings.star[[1]][, 3] > 0] %>% names),
  length(fit$loadings.star[[1]][, 4][fit$loadings.star[[1]][, 4] > 0] %>% names),
  length(fit$loadings.star[[1]][, 5][fit$loadings.star[[1]][, 5] > 0] %>% names),
  length(fit$loadings.star[[1]][, 6][fit$loadings.star[[1]][, 6] > 0] %>% names),
  length(fit$loadings.star[[1]][, 7][fit$loadings.star[[1]][, 7] > 0] %>% names),
  length(fit$loadings.star[[1]][, 8][fit$loadings.star[[1]][, 8] > 0] %>% names),
  length(fit$loadings.star[[1]][, 9][fit$loadings.star[[1]][, 9] > 0] %>% names),
  length(fit$loadings.star[[1]][, 10][fit$loadings.star[[1]][, 10] > 0] %>% names),
  length(fit$loadings.star[[1]][, 11][fit$loadings.star[[1]][, 11] > 0] %>% names),
  length(fit$loadings.star[[1]][, 12][fit$loadings.star[[1]][, 12] > 0] %>% names)
))


fit <- train_full_pls_fit_onese$fit$fit$fit
fit$ncomp
fit$keepX

mean(c( 
  length(fit$loadings.star[[1]][, 1][fit$loadings.star[[1]][, 1] > 0] %>% names),
  length(fit$loadings.star[[1]][, 2][fit$loadings.star[[1]][, 2] > 0] %>% names),
  length(fit$loadings.star[[1]][, 3][fit$loadings.star[[1]][, 3] > 0] %>% names),
  length(fit$loadings.star[[1]][, 4][fit$loadings.star[[1]][, 4] > 0] %>% names)
))
 
mean(c(
  fit$loadings$X[, 1][fit$loadings$X[, 1] > 0] %>% length,
  fit$loadings$X[, 2][fit$loadings$X[, 2] > 0] %>% length,
  fit$loadings$X[, 3][fit$loadings$X[, 3] > 0] %>% length,
  fit$loadings$X[, 4][fit$loadings$X[, 4] > 0] %>% length
))










