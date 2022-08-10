library(magrittr) 
source("globals/global_variables.R")

# dat_vect <- c("train_year_full", "train_season_full") 

# train_season_partial_xgb
dat_vect <- c("train_season_partial", "train_season_corrected_partial")
mod_type <- "xgb"
# quantile <- stringr::str_pad(seq(0, .2, by = .1) * 100, width = 3, pad = 0)
# quantile <- stringr::str_pad(seq(0, .2, by = .1) * 100, width = 3, pad = 0)
quantile <- stringr::str_pad(0 * 100, width = 3, pad = 0)

# Load data
data(list = dat_vect)
data("test_season_partial")
data("test_season_corrected_partial")

nam <- purrr::cross(list(d = dat_vect, m = mod_type)) %>%
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
  
  splt <- stringr::str_split(d, pattern = "_xgb") %>% unlist
  dat <- splt[1] %>% get

  
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

dat_vect <- c("train_season_partial", "train_season_corrected_partial") 
mod_type <- "xgb"
# quantile <- stringr::str_pad(seq(0, .2, by = .1) * 100, width = 3, pad = 0)
# quantile <- stringr::str_pad(0 * 100, width = 3, pad = 0)
fit <- c("bst", "onese")



perf_lst <- purrr::cross( list(dat = dat_vect, 
                              # q = quantile, 
                              mod = mod_type,
                              fit = fit) ) %>% 
  purrr::map(.f = function(d){ 
    #
    print(d$dat)
    dat_train <- get(d$dat)
    dat_type <- strsplit(d$dat, split = "_")[[1]][2]
    mod <- get(glue::glue("{d$dat}_{d$mod}_fit_{d$fit}"))
    if("corrected" %in% (stringr::str_split(d$dat, pattern = "_") %>% unlist())){
      test_dat <- test_season_corrected_partial 
      type <- "corrected"
    }else{
      test_dat <- test_season_partial
      type <- "uncorrected"
    }
    # Test and train output
    list(
      train = dat_train %>% 
        dplyr::mutate(
          perf_type = "train",
          data_type = type,
          fit_type = d$fit,
          # quantile = d$q,
          prd = predict(mod, new_data = dat_train) %>% dplyr::pull()
        ),
      test = test_dat %>% 
        dplyr::mutate(
          perf_type = "test",
          data_type = type,
          fit_type = d$fit,
          # quantile = d$q,
          prd = predict(mod, new_data = test_dat) %>% dplyr::pull()
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
  dplyr::filter(quantile == "000")
perf_lst$train %>% 
  dplyr::group_by(perf_type, data_type, fit_type, quantile) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = prd) %>% 
  dplyr::filter(quantile == "000")
perf_lst$test %>% 
  dplyr::group_by(perf_type, data_type, fit_type, quantile) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = prd) %>% 
  dplyr::filter(quantile == "000")
perf_lst$test %>% 
  dplyr::group_by(perf_type, data_type, fit_type, quantile) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = prd) %>% 
  dplyr::filter(quantile == "000")

train_season_full %>% 
  broom::augment( x = train_season_full_xgb_000_fit_bst ) %>% 
  dplyr::group_by(year) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = .pred)
test_season_full %>% 
  broom::augment( x = train_season_full_xgb_000_fit_bst ) %>% 
  dplyr::group_by(year) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = .pred)

# Why 2019 and 2020 so bad ? 

data("technico_milk")

technico_milk %>% 
  dplyr::select(year, dplyr::all_of(tech_gradient_var)) %>% 
  tidyr::pivot_longer(cols = -c("year")) %>% 
  dplyr::mutate(year = factor(year)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = year, y = value)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_grid("name", scales = "free")

technico_milk %>% 
  dplyr::mutate(year = factor(year)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = gradient_axis1, y = gradient_axis2, color = year)) +
  ggplot2::geom_point()
  
technico_milk %>% 
  dplyr::mutate(year = factor(year)) %>% 
  ggplot2::ggplot(ggplot2::aes(y = gradient_axis1, x = year)) +
  ggplot2::geom_boxplot()
  
technico_milk %>% 
  dplyr::mutate(year = factor(year)) %>% 
  ggplot2::ggplot(ggplot2::aes(y = gradient_axis2, x = year)) +
  ggplot2::geom_boxplot()

# ---------------------------------------------- #
# << What about milk data
# ---------------------------------------------- #

data("working_dat")

mean_dat <- working_dat %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(dplyr::across(-c("gradient_axis1", "farmerID"),
                              .fns = ~{.x / mean(.x)})) %>% 
  dplyr::ungroup()
  


dat <- working_dat
dat <- mean_dat

dat_pca <- dat %>% 
  dplyr::select(-c("gradient_axis1", "farmerID", "year"))
milk_pca <- FactoMineR::PCA(dat_pca, ncp = 6)
coord <- predict(milk_pca, newdata = dat_pca)$coord %>% 
  tibble::as_tibble()
dat_pca <- dat %>% 
  dplyr::select(c("gradient_axis1", "farmerID", "year")) %>% 
  dplyr::bind_cols(coord)

dat_pca %>% 
  dplyr::mutate(year = factor(year)) %>% 
  tidyr::pivot_longer(cols = dplyr::contains("Dim")) %>% 
  ggplot2::ggplot(ggplot2::aes(x = year, y = value)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap("name", scales = "free", ncol = 2)

# Dim.2 (?)
contrib <-  milk_pca$var$contrib %>%
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  dplyr::arrange(dplyr::desc(Dim.2)) %>% 
  dplyr::pull(rowname)

var <- contrib[1:12]
dat %>% 
  dplyr::mutate(year = as.factor(year)) %>% 
  dplyr::select(year, dplyr::all_of(var)) %>% 
  tidyr::pivot_longer(cols = dplyr::all_of(var)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = year, y = value)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap("name", scales = "free", ncol = 3)

# pls with no correctd year

data("train_year_full")
data("test_year_full")
data("train_season_full")
data("test_season_full")
library(splines)

form <- working_dat %>% 
  dplyr::select(-c("gradient_axis1", "year", "farmerID")) %>% names %>% 
  # paste0(sep = "ns(", ., ", df = 2)") %>%
  paste(collapse = " + ") %>% 
  paste("gradient_axis1", ., sep = "~") %>% 
  formula()
  
  

ncomp <- 10
max_year <- 2021
# Model with no mean correction
train1 <- working_dat %>% 
  dplyr::inner_join(train_year_full %>% dplyr::select(c("year", "farmerID"))) %>% 
  dplyr::filter(year <= max_year)
test1 <- working_dat %>% 
  dplyr::inner_join(test_year_full %>% dplyr::select(c("year", "farmerID"))) %>% 
  dplyr::filter(year <= max_year)
mod1 <- pls::mvr(form, data = train1, ncomp = ncomp,
         method = pls::pls.options()$plsralg, scale = T)

# Model with mean correction
train2 <- mean_dat %>% 
  dplyr::inner_join(train_year_full %>% dplyr::select(c("year", "farmerID"))) %>% 
  dplyr::filter(year <= max_year)
test2 <- mean_dat %>% 
  dplyr::inner_join(test_year_full %>% dplyr::select(c("year", "farmerID"))) %>% 
  dplyr::filter(year <= max_year)
mod2 <- pls::mvr(form, data = train2, ncomp = ncomp,
         method = pls::pls.options()$plsralg, scale = T)

# quarlerly model with mean correction
form <- train_season_full %>% 
  dplyr::select(-c("farmerID", "year", "gradient_axis1")) %>% names %>% 
  # paste0(sep = "ns(", ., ", df = 2)") %>%
  paste(collapse = " + ") %>% 
  paste("gradient_axis1", ., sep = " ~ ") %>% 
  formula() 
train3 <- train_season_full
test3 <- test_season_full
mod3 <- pls::mvr(form, data = train3, ncomp = ncomp,
         method = pls::pls.options()$plsralg, scale = T)

# Performance
augmented_dat1 <- 
  dplyr::bind_rows(
    train1 %>% tibble::add_column(pred_type = "train"),
    test1 %>% tibble::add_column(pred_type = "test")
  ) 
augmented_dat1 %<>% 
  dplyr::mutate(
    .pred = predict(mod1, newdata = augmented_dat1, ncomp = ncomp) %>% drop
                ) %>% 
  tibble::add_column(data_type = "not corrected")

augmented_dat2 <- 
  dplyr::bind_rows(
    train2 %>% tibble::add_column(pred_type = "train"),
    test2 %>% tibble::add_column(pred_type = "test")
  ) 
augmented_dat2 %<>% 
  dplyr::mutate(
    .pred = predict(mod2, newdata = augmented_dat2, ncomp = ncomp) %>% drop
                ) %>% 
  tibble::add_column(data_type = "yearly corrected")

augmented_dat3 <- 
  dplyr::bind_rows(
    train3 %>% tibble::add_column(pred_type = "train"),
    test3 %>% tibble::add_column(pred_type = "test")
  ) 
augmented_dat3 %<>% 
  dplyr::mutate(
    .pred = predict(mod3, newdata = augmented_dat3, ncomp = ncomp) %>% drop
                ) %>% 
  tibble::add_column(data_type = "quarterly")


dplyr::bind_rows(augmented_dat1, augmented_dat2, augmented_dat3) %>% 
  dplyr::group_by(data_type, pred_type, year, farmerID) %>%
  # yardstick::rsq(truth = gradient_axis1, estimate = .pred)
  yardstick::rmse(truth = gradient_axis1, estimate = .pred) %>% 
  dplyr::mutate(year = as.factor(year)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = year, y = .estimate, color = data_type)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_grid("pred_type")


dplyr::bind_rows(augmented_dat1, augmented_dat2, augmented_dat3) %>% 
  dplyr::group_by(data_type, pred_type) %>%
  # yardstick::rsq(truth = gradient_axis1, estimate = .pred)
  yardstick::rmse(truth = gradient_axis1, estimate = .pred)

dplyr::bind_rows(augmented_dat1, augmented_dat2, augmented_dat3) %>% 
  ggplot2::ggplot(ggplot2::aes(
    x = gradient_axis1, y = .pred, color = data_type
    )) +
  ggplot2::geom_point(alpha = .2) +
  ggplot2::geom_abline(slope = 1, intercept = 0) +
  ggplot2::geom_smooth(se = F, method = "lm") +
  ggplot2::facet_grid("pred_type")
  # ggplot2::facet_grid(pred_type ~ year)


# Remonve outliers
d1 <- augmented_dat1 %>% 
  dplyr::filter(pred_type == "train") %>% 
  dplyr::mutate(resi = gradient_axis1 - .pred) %>% 
  dplyr::pull(resi)
train_year_full <- train_year_full[(abs(d1) < 3*sd(d1)), ]

d2 <- augmented_dat3 %>% 
  dplyr::filter(pred_type == "train") %>% 
  dplyr::mutate(resi = gradient_axis1 - .pred) %>% 
  dplyr::pull(resi)
train_season_full <- train_season_full[(abs(d2) < 3*sd(d2)), ]





test_year_full %>% 
  dplyr::mutate(year = as.factor(year)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = year, y = gradient_axis1)) +
  ggplot2::geom_boxplot()

dplyr::bind_rows(
  test_year_full %>% tibble::add_column(data_type = "test"),
  train_year_full %>% tibble::add_column(data_type = "train")
  ) %>% 
  dplyr::mutate(year = as.factor(year)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = year, y = gradient_axis1, color = data_type)) +
  ggplot2::geom_violin()


# What about milk data >>
# ------------------------------------------------ #



perf_lst$train %>% 
  dplyr::filter(data_type %in% c("quarter"),
                quantile == "000",
                fit_type == "bst") %>% 
  ggplot2::ggplot(ggplot2::aes(x = prd, y = gradient_axis1)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "darkred")

perf_lst$test %>% 
  dplyr::filter(data_type %in% c("quarter"),
                quantile == "000",
                fit_type == "bst") %>% 
  ggplot2::ggplot(ggplot2::aes(x = prd, y = gradient_axis1)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "darkred") + 
  ggplot2::facet_grid(data_type ~ fit_type)



fit <- train_year_full_pls_fit_bst$fit$fit$fit
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


fit <- train_year_full_pls_fit_onese$fit$fit$fit
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









