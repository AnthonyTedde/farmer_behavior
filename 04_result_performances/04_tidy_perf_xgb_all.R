 library(magrittr) 

dat_vect <- c("train_full", "train_q20", "train_q10", "train_q05") 
mod_type <- "xgb"
quantile <- stringr::str_pad(seq(0, .9, by = .1) * 100, width = 3, pad = 0)

# Load data
data(list = dat_vect)
data("test_full")

nam <- purrr::cross(list(d = dat_vect, m = mod_type, q = quantile)) %>%
  purrr::map(purrr::reduce, paste, sep = "_") %>% unlist

# Load models results
paste(nam, "rslt", sep = "_") %>% 
  data(list = .)

# Load workflow
paste(nam, "wfl", sep = "_") %>% 
  data(list = .)


# -------------------------------------------------------------------------------------- #
# Compute fitted models
# -------------------------------------------------------------------------------------- #

for(d in nam){
  
  splt <- stringr::str_split(d, pattern = "_") %>% unlist
  dat <- paste(splt[1], splt[2], sep = "_") %>% get

  
  rslt <- get(paste(d, "rslt", sep = "_"))
  bst <- rslt %>% tune::select_best(metric = "rmse")
  onese <- rslt %>% tune::select_by_one_std_err(
    metric = "rmse", 
    dplyr::desc(loss_reduction)
  ) 
  
  wfl <- get(paste(d, "wfl", sep = "_"))
  
  # fit bst
  fit_bst <- wfl %>% 
    tune::finalize_workflow(bst) %>% 
    parsnip::fit(dat)
  # fit onese
  fit_onese <- wfl %>% 
    tune::finalize_workflow(onese) %>% 
    parsnip::fit(dat)
  
  fit_bst_name <- paste(d, "fit_bst", sep = "_") 
  assign(fit_bst_name, fit_bst)
  
  fit_onese_name <- paste(d, "fit_onese", sep = "_") 
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

dat_vect <- c("train_full", "train_q20", "train_q10", "train_q05") 
mod_type <- "xgb"
quantile <- stringr::str_pad(seq(0, .9, by = .1) * 100, width = 3, pad = 0)
fit <- c("bst", "onese")



perf_lst <- purrr::cross( list(dat = dat_vect, 
                              q = quantile, 
                              mod = mod_type,
                              fit = fit) ) %>% 
  purrr::map(.f = function(d){ 
    #
    dat_train <- get(d$dat)
    dat_type <- strsplit(d$dat, split = "_")[[1]][length(strsplit(d$dat, split = "_")[[1]])]
    mod <- get(glue::glue("{d$dat}_{d$mod}_{d$q}_fit_{d$fit}"))
    # Test and train output
    list(
      train = dat_train %>% 
        dplyr::mutate(
          perf_type = "train",
          data_type = dat_type,
          fit_type = d$fit,
          quantile = d$q,
          prd = predict(mod, new_data = dat_train) %>% dplyr::pull()
        ),
      test = test_full %>% 
        dplyr::mutate(
          perf_type = "test",
          data_type = dat_type,
          fit_type = d$fit,
          quantile = d$q,
          prd = predict(mod, new_data = test_full) %>% dplyr::pull()
        )
    ) 
  })


perf_lst %<>% 
  purrr::transpose() %>% 
  purrr::map(purrr::reduce, dplyr::bind_rows)

perf_lst$train %>% 
  dplyr::group_by(perf_type, data_type, fit_type, quantile) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = prd)
perf_lst$train %>% 
  dplyr::group_by(perf_type, data_type, fit_type, quantile) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = prd)

perf_lst$test %>% 
  dplyr::group_by(perf_type, data_type, fit_type, quantile) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = prd)
perf_lst$test %>% 
  dplyr::group_by(perf_type, data_type, fit_type, quantile) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = prd)


perf_lst$train %>% 
  dplyr::group_by(perf_type, data_type, fit_type, quantile) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = prd) %>% 
  dplyr::rename(rmse = .estimate) %>% 
  ggplot2::ggplot(ggplot2::aes(y = rmse, x = quantile)) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(data_type ~ fit_type)

perf_lst$train %>% 
  dplyr::group_by(perf_type, data_type, fit_type, quantile) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = prd) %>% 
  dplyr::rename(rsq = .estimate) %>% 
  ggplot2::ggplot(ggplot2::aes(y = rsq, x = quantile)) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(data_type ~ fit_type)

perf_lst$test %>% 
  dplyr::group_by(perf_type, data_type, fit_type, quantile) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = prd) %>% 
  dplyr::rename(rsq = .estimate) %>% 
  ggplot2::ggplot(ggplot2::aes(y = rsq, x = quantile)) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(data_type ~ fit_type)


perf_lst$test %>% 
  dplyr::group_by(perf_type, data_type, fit_type, quantile) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = prd) %>% 
  dplyr::rename(rmse = .estimate) %>% 
  ggplot2::ggplot(ggplot2::aes(y = rmse, x = quantile)) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(data_type ~ fit_type)





perf_lst$train %>% 
  dplyr::group_by(perf_type, data_type, fit_type, quantile) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = prd) %>% 
  dplyr::filter(quantile == "060")
perf_lst$train %>% 
  dplyr::group_by(perf_type, data_type, fit_type, quantile) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = prd) %>% 
  dplyr::filter(quantile == "060")
perf_lst$test %>% 
  dplyr::group_by(perf_type, data_type, fit_type, quantile) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = prd) %>% 
  dplyr::filter(quantile == "060")
perf_lst$test %>% 
  dplyr::group_by(perf_type, data_type, fit_type, quantile) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = prd) %>% 
  dplyr::filter(quantile == "060")

  


perf_lst$train %>% 
  dplyr::filter(data_type %in% c("full"),
                quantile == "060",
                fit_type == "bst") %>% 
  ggplot2::ggplot(ggplot2::aes(x = prd, y = gradient_axis1)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "darkred")

perf_lst$test %>% 
  dplyr::filter(data_type %in% c("full"),
                quantile == "060",
                fit_type == "bst") %>% 
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









