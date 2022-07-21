library(magrittr)
library(workflows)

library(plsmod)
library(bestNormalize)

suffixe <- c("year_corrected_full", "season_corrected_full", "season_corrected_partial",  
             "winsum_corrected_full", "winsum_corrected_partial")  
train_vect <- paste("train", suffixe, sep = "_")  
test_vect <- paste("test", suffixe, sep = "_")
mdl_fitted_bst <- paste("train", suffixe, "pls_fit_bst", sep = "_")
mdl_fitted_onese <- paste("train", suffixe, "pls_fit_onese", sep = "_")

data(list = c(train_vect, test_vect, mdl_fitted_bst, mdl_fitted_onese))


# -------------------------------------------------------------------------------------- #
# Build information vector ####
# -------------------------------------------------------------------------------------- #

info <- list(train = train_vect, 
             test = test_vect, 
             suffixe = suffixe) %>% 
  purrr::pmap(list)

# Info_plus contains fitted model.
info_plus <- c(
  info %>% purrr::map(~c(.x, fit = "bst")),
  info %>% purrr::map(~c(.x, fit = "onese")) 
)


# -------------------------------------------------------------------------------------- #
# Compute performance tables ####
# -------------------------------------------------------------------------------------- #

perf_lst <- info_plus %>% 
  purrr::map(.f = function(d){ 
    #
    dat_train <- get(d$train)
    dat_test <- get(d$test)
    dat_type <- sub(pattern = "\\_corrected", replacement = "", d$suffixe)
    mod <- get(glue::glue("{d$train}_pls_fit_{d$fit}"))
    # Test and train output
    list(
      train = dat_train %>% 
        dplyr::mutate(
          perf_type = "train",
          data_type = dat_type,
          fit_type = d$fit,
          prd = predict(mod, new_data = dat_train) %>% dplyr::pull()
        ),
      test = dat_test %>% 
        dplyr::mutate(
          perf_type = "test",
          data_type = dat_type,
          fit_type = d$fit,
          prd = predict(mod, new_data = dat_test) %>% dplyr::pull()
        )
    ) 
  })


perf_lst %<>% 
  purrr::transpose() %>% 
  purrr::map(purrr::reduce, dplyr::bind_rows)

pls_perf_corrected_full <- perf_lst %>% 
  dplyr::bind_rows() %>% 
  tibble::add_column(yearly = "Corrected")

save(pls_perf_corrected_full, file = "data/pls_perf_corrected_full.rda")


# -------------------------------------------------------------------------------------- #
# Arrange performances tables (first glance) ####
# -------------------------------------------------------------------------------------- #

rmse_train <- perf_lst$train %>% 
  dplyr::group_by(perf_type, data_type, fit_type) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = prd) 

rsq_train <- perf_lst$train %>% 
  dplyr::group_by(perf_type, data_type, fit_type) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = prd)

rmse_test <- perf_lst$test %>% 
  dplyr::group_by(perf_type, data_type, fit_type) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = prd)

rsq_test <- perf_lst$test %>% 
  dplyr::group_by(perf_type, data_type, fit_type) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = prd)



corrected_pls_perf <- dplyr::bind_rows(
  rmse_train, rmse_test,
  rsq_train, rsq_test
) %>% 
  tibble::add_column(yearly = "Corrected")

save(corrected_pls_perf, file = "data/corrected_pls_perf.rda")
