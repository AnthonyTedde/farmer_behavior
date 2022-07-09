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
data("technico_milk")

# Load models results
data("train_full_rslt")
data("train_q20_rslt")
data("train_q10_rslt")
data("train_q05_rslt")

# Load workflow
data("train_full_wfl")
data("train_q20_wfl")
data("train_q10_wfl")
data("train_q05_wfl")


# -------------------------------------------------------------------------------------- #
# Compute fitted models
# -------------------------------------------------------------------------------------- #

for(d in dat_vect){
  
  rslt <- get(paste(d, "rslt", sep = "_"))
  bst <- rslt %>% tune::select_best()
  # onese <- rslt %>% tune::select_by_one_std_err(num_comp, predictor_prop)
  onese <- rslt %>% tune::select_by_one_std_err(num_comp)
  
  dat <- get(d)
  
  # fit bst
  fit_bst <- get(paste(d, "wfl", sep = "_")) %>% 
    tune::finalize_workflow(bst) %>% 
    parsnip::fit(dat)
  # fit onese
  fit_onese <- get(paste(d, "wfl", sep = "_")) %>% 
    tune::finalize_workflow(onese) %>% 
    parsnip::fit(dat)
  
  fit_bst_name <- paste(d, "pls_fit_bst", sep = "_") 
  assign(fit_bst_name, fit_bst)
  
  fit_onese_name <- paste(d, "pls_fit_onese", sep = "_") 
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
mod_vect <- "pls"



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

# - Best full -  << #

# 1% worst
#
rem <- technico_milk %>% 
  dplyr::group_by(farmerID) %>% 
  dplyr::count() %>% 
  dplyr::mutate(
    remove = ifelse(n < 3, T, F)
  )


train_full_rslt$.metrics[[1]]

tune::show_best(train_full_rslt, n = 700) %>% 
  ggplot2::ggplot(ggplot2::aes(num_comp, y = predictor_prop, z = mean)) +
  ggplot2::stat_density2d_filled()
  
plt <- function(dat){ 
  dat %>% 
    ggplot2::ggplot(ggplot2::aes(x = .pred, y = gradient_axis1)) +
    ggplot2::geom_point(ggplot2::aes(color = keep_099)) + 
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "darkred")
}
augment_dt <- function(newd){ 
  train_full_pls_fit_bst %>% 
    broom::augment(new_data = newd) %>% 
    dplyr::mutate(residuals = gradient_axis1 - .pred) %>% 
    dplyr::mutate(
      keep_099 = dplyr::case_when(
        residuals <= quantile(residuals, prob = .005) ~ F,
        residuals >= quantile(residuals, prob = .995) ~ F,
        T ~ T
      ), 
      keep_098 = dplyr::case_when(
        residuals <= quantile(residuals, prob = .010) ~ F,
        residuals >= quantile(residuals, prob = .990) ~ F,
        T ~ T
      ),
      ) %>% 
    dplyr::inner_join(rem)
}

perf <- function(dat, filter = T){ 
  d <- dat %>% 
    dplyr::filter(filter)
  list(
    r2 = yardstick::rsq(d, truth = gradient_axis1, estimate = .pred),
    rmse = yardstick::rmse(d, truth = gradient_axis1, estimate = .pred)
  ) %>% dplyr::bind_rows()
}

train_full_augmented <- augment_dt(newd = train_full)
test_full_augmented <- augment_dt(newd = test_full) 

# full
perf(dat = train_full_augmented) 
perf(dat = test_full_augmented) 
# 99%
perf(dat = train_full_augmented, filter = train_full_augmented$keep_099) 
perf(dat = test_full_augmented, filter = test_full_augmented$keep_099) 
# 99%
perf(dat = train_full_augmented, filter = train_full_augmented$keep_098) 
perf(dat = test_full_augmented, filter = test_full_augmented$keep_098) 

# Pref by year
train_full_augmented %>% 
  # dplyr::filter(keep_099) %>%
  dplyr::group_by(year) %>% 
  dplyr::mutate(n = dplyr::n()) %>% 
  dplyr::group_by(n, .add = T) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = .pred)
test_full_augmented %>% 
  # dplyr::filter(keep_099) %>%
  dplyr::group_by(year) %>% 
  dplyr::mutate(n = dplyr::n()) %>% 
  dplyr::group_by(n, .add = T) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = .pred)

# Degree of intensification by year
train_full_augmented %>% 
  dplyr::select(year, gradient_axis1, .pred, farmerID) %>% 
  dplyr::rename(
    obs = gradient_axis1,
    prd = .pred
  ) %>% 
  tidyr::pivot_longer(cols = c("obs", "prd"),
                      names_to = "gradient") %>% 
  dplyr::mutate(year = ordered(year)) %>% 
  ggplot2::ggplot(ggplot2::aes(axis1 = year, axis2 = year, y = value)) +
  # ggplot2::ggplot(ggplot2::aes(x = year, y = value, 
  #                              color = gradient, group = farmerID)) +
  ggalluvial::geom_flow() +
  scale_x_discrete(limits = c("str", "end")) +
  ggalluvial::geom_stratum() 
# +
#   ggplot2::geom_text(stat = "stratum", aes(label = ggalluvial::after_stat(stratum)))

set.seed(1010)
sub <- train_full_augmented %>% 
  dplyr::group_by(farmerID) %>% 
  dplyr::count() %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(n == max(n)) %>% 
  dplyr::sample_n(size = 12)

train_full_augmented %>% 
  dplyr::inner_join(sub) %>% 
  dplyr::select(year, gradient_axis1, .pred, farmerID) %>% 
  dplyr::rename(
    obs = gradient_axis1,
    prd = .pred
  ) %>% 
  tidyr::pivot_longer(cols = c("obs", "prd"),
                      names_to = "gradient") %>% 
  dplyr::mutate(year = ordered(year)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = year, y = value, group = gradient, color = gradient)) + 
  ggplot2::geom_point() +
  ggplot2::geom_smooth() +
  # ggplot2::geom_line() +
  ggplot2::facet_wrap("farmerID", nrow = 3)

set.seed(1010)
sub <- test_full_augmented %>% 
  dplyr::group_by(farmerID) %>% 
  dplyr::count() %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(n == max(n)) %>% 
  dplyr::sample_n(size = 6)

test_full_augmented %>% 
  dplyr::inner_join(sub) %>% 
  dplyr::select(year, gradient_axis1, .pred, farmerID) %>% 
  dplyr::rename(
    obs = gradient_axis1,
    prd = .pred
  ) %>% 
  tidyr::pivot_longer(cols = c("obs", "prd"),
                      names_to = "gradient") %>% 
  dplyr::mutate(year = ordered(year)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = year, y = value, group = gradient, color = gradient)) + 
  ggplot2::geom_point() +
  # ggplot2::geom_smooth(se = F) + 
  ggplot2::geom_line() +
  ggplot2::facet_wrap("farmerID", nrow = 3)

# -- > categories longitudinal










  
  ggplot2::geom_boxplot()
    
plt(train_full_augmented)
plt(test_full_augmented)

train_full_augmented %>%  
    ggplot2::ggplot(ggplot2::aes(x = .pred, y = gradient_axis1)) +
    ggplot2::geom_point(ggplot2::aes(color = year)) + 
    ggplot2::geom_abline(intercept = 0, slope = 1)
test_full_augmented %>% 
    ggplot2::ggplot(ggplot2::aes(x = .pred, y = gradient_axis1)) +
    ggplot2::geom_point(ggplot2::aes(color = year)) + 
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "darkred")

# Effect of the year ? 


test_full_augmented %>% 
  dplyr::filter(residuals < quantile(residuals, prob = 0.02))



# ------------  >> #



# What about t-outlier test

train_full_pls_fit_bst %>% 
  broom::tidy() 

residuals <- train_full_pls_fit_bst %>% 
  broom::augment(new_data = train_full) %>% 
  dplyr::mutate(residuals = gradient_axis1 - .pred) %>% 
  dplyr::pull(residuals)

threshold <- mean(residuals) + c(-3, 3) * sd(residuals)
train_full_cleaned <-  train_full[
  residuals > min(threshold) & residuals < max(threshold), 
]

fit_bst_cleaned <- get(paste(d, "wfl", sep = "_")) %>% 
  tune::finalize_workflow(bst) %>% 
  parsnip::fit(train_full_cleaned)

fit_bst_cleaned %>% 
  broom::augment(new_data = train_full_cleaned) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = .pred)
fit_bst_cleaned %>% 
  broom::augment(new_data = train_full_cleaned) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = .pred)

fit_bst_cleaned %>% 
  broom::augment(new_data = test_full) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = .pred) 
fit_bst_cleaned %>% 
  broom::augment(new_data = test_full) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = .pred)