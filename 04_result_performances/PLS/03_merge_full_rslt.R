########


# Delete whole file.

###################3
library(magrittr)

data("pls_perf_corrected_full")
data("pls_perf_uncorrected_full")

pls_perf_full <- dplyr::bind_rows( pls_perf_corrected_full, pls_perf_uncorrected_full )



pls_perf_full %<>% 
  dplyr::mutate(
    relevance = stringr::str_split(data_type, pattern = "_", simplify = T)[, 2],
    .after = data_type
  ) %>% 
  dplyr::mutate( 
    data_type = stringr::str_split(data_type, pattern = "_", simplify = T)[, 1],
  )


# Is these a difference between corrected and uncorrected performance for season partial (?)
# Is the difference is really different from zero ?


difference <- pls_perf_full %>% 
  dplyr::filter(perf_type == "test") %>%
  dplyr::filter(
    ( yearly == "Uncorrected" & fit_type == "onese" & data_type == "year" & relevance == "full") | 
    ( yearly == "Corrected" & fit_type == "bst" & data_type == "season" & relevance == "partial")
  ) %>% 
  # dplyr::filter(perf_type == "test", data_type == "season", relevance == "partial") %>%
  # dplyr::filter(
  #   ( yearly == "Uncorrected" & fit_type == "onese") | 
  #   ( yearly == "Corrected" & fit_type == "bst")
  # ) %>% 
  dplyr::mutate(absolute_residuals = abs(gradient_axis1 - prd)) %>%
  # dplyr::mutate(absolute_residuals = gradient_axis1 - prd) %>%
  dplyr::select(farmerID, year, yearly, absolute_residuals) %>% 
  tidyr::pivot_wider(names_from = "yearly", values_from = "absolute_residuals") %>% 
  dplyr::mutate(diff = Corrected - Uncorrected) %>% 
  tidyr::drop_na()

difference$diff %>% hist
a <- ( (difference$diff - 0 ) / sd(difference$diff) )

b <- ( (mean(difference$diff) - 0 ) / (sd(difference$diff) / sqrt(nrow(difference))) )

x <- seq(-4, 4, length=100)
hx <- dt(x, df = nrow(difference)) 

# plot(x, hx, type="l", lty=2, xlab="x value",
#      ylab="Density", main="Comparison of t Distributions")

# hist(a)
# plot(density(a), lwd = 2, col = "red", main = "Density")
# lines(x, hx, lwd=2)
plot(x, hx, lwd=2)
abline(v = b)



# -------------------------------------------------------------------------------------- #
# Plot
# -------------------------------------------------------------------------------------- #


dt(difference$diff, df = nrow(difference) - 1, )

pls_perf_full %>% 
  dplyr::filter(perf_type == "test", data_type == "season", relevance == "partial") %>%
  dplyr::filter(
    ( yearly == "Uncorrected" & fit_type == "onese") | 
    ( yearly == "Corrected" & fit_type == "bst")
  ) %>% 
  dplyr::mutate(absolute_residuals = abs(gradient_axis1 - prd)) %>%
  # dplyr::mutate(absolute_residuals = gradient_axis1 - prd) %>%
  dplyr::select(farmerID, year, yearly, absolute_residuals, gradient_axis1, prd) %>% 
  ggplot2::ggplot(ggplot2::aes(x = prd, y = gradient_axis1, color = yearly)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1) +
  ggplot2::geom_smooth(method = "lm", se = F)

pls_perf_full %>% 
  dplyr::filter(perf_type == "test", data_type == "season", relevance == "partial") %>%
  dplyr::filter(
    ( yearly == "Uncorrected" & fit_type == "onese") | 
    ( yearly == "Corrected" & fit_type == "bst")
  ) %>% 
  dplyr::mutate(absolute_residuals = abs(gradient_axis1 - prd)) %>%
  # dplyr::mutate(absolute_residuals = gradient_axis1 - prd) %>%
  dplyr::select(farmerID, year, yearly, absolute_residuals, gradient_axis1, prd) %>% 
  dplyr::group_by(farmerID, yearly) %>% 
  dplyr::summarise(dplyr::across(c("gradient_axis1", "prd"), mean)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = prd, y = gradient_axis1, color = yearly)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1) +
  ggplot2::geom_smooth(method = "lm", se = F)

pls_perf_full %>% 
  dplyr::filter(perf_type == "test", data_type == "season", relevance == "partial") %>%
  dplyr::filter(
    ( yearly == "Uncorrected" & fit_type == "onese") | 
    ( yearly == "Corrected" & fit_type == "bst")
  ) %>% 
  dplyr::mutate(absolute_residuals = abs(gradient_axis1 - prd)) %>%
  # dplyr::mutate(absolute_residuals = gradient_axis1 - prd) %>%
  dplyr::select(farmerID, year, yearly, absolute_residuals, gradient_axis1, prd) %>% 
  dplyr::group_by(farmerID, yearly) %>% 
  dplyr::summarise(dplyr::across(c("gradient_axis1", "prd"), mean)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(residuals = abs(gradient_axis1 - prd)) %>% 
  dplyr::arrange(desc(residuals)) 

# case: 10037685
data("milk_season_clean_dat")
data("train_season_partial_pls_fit_bst")
data("train_season_partial_pls_fit_onese")
data("test_season_partial")
data("train_season_partial")

case <- 10037685

train_season_partial_pls_fit_bst %>% 
# train_season_partial_pls_fit_onese %>% 
  broom::tidy() %>% 
  dplyr::filter(term != "Y") %>%
  dplyr::group_by(component) %>%
  dplyr::slice_max(abs(value), n = 20) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(value, forcats::fct_reorder(term, value), 
                               fill = factor(component))) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::facet_wrap(~component, scales = "free_y") +
  ggplot2::labs(y = NULL)

   

a <- predict(train_season_partial_pls_fit_bst, new_data = test_season_partial, 
        type = "raw")

a <- a$variates %>% tibble::as_tibble()

test_season_partial %>% 
  dplyr::bind_cols(a) %>% 
  dplyr::mutate(interest = dplyr::if_else(farmerID == case, "Yep", "Nope")) %>% 
  ggplot2::ggplot(ggplot2::aes(x = gradient_axis1, 
                               y = dim1, color = interest)) +
  ggplot2::geom_point()


b <- predict(train_season_partial_pls_fit_bst, new_data = train_season_partial, 
        type = "raw")

b <- b$variates %>% tibble::as_tibble()

train_season_partial %>% 
  dplyr::bind_cols(b) %>% 
  # dplyr::mutate(interest = dplyr::if_else(farmerID == case, "Yep", "Nope")) %>% 
  ggplot2::ggplot(ggplot2::aes(y = gradient_axis1, x = dim4)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth() +
  ggplot2::geom_smooth(method = "lm")

 
## <<< TODO ON ALL DATA
train_pls <- train_season_partial %>% 
  dplyr::bind_cols(b)

grid <- with(train_pls, interp::interp(dim1, dim2, gradient_axis1,
                                       nx = 500, ny = 500))
griddf <- subset(data.frame(x = rep(grid$x, nrow(grid$z)),
                            y = rep(grid$y, each = ncol(grid$z)),
                            z = as.numeric(grid$z)),
                 !is.na(z))

ggplot2::ggplot(griddf, ggplot2::aes(x, y, z = z)) +
  ggplot2::geom_contour_filled(breaks = c(seq(-6, 6, by = 4))) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = dim1, y = dim2, z = NULL, 
                                             ), data = train_pls, 
                      alpha = .4)
 
train_pls %>% 
  # dplyr::mutate(interest = dplyr::if_else(farmerID == case, "Yep", "Nope")) %>% 
  ggplot2::ggplot(ggplot2::aes(y = dim1, x = dim2, color = gradient_axis1)) +
  ggplot2::geom_point()

# >>>
# Test gam over pls
new_train <- train_season_partial %>% 
  dplyr::bind_cols(b)
new_test <- test_season_partial %>% 
  dplyr::bind_cols(a)

form <- new_train %>% names %>% grep(pattern = "^dim", value = T) %>% 
  paste("s(", ., ")") %>% 
  # setdiff(., "dim1") %>% 
  # c("s(dim1)", .) %>% 
  paste(collapse = " + ") %>% 
  paste("gradient_axis1", ., sep = " ~ ") %>% 
  formula

gam_mod <- mgcv::gam(form, data = new_train, method = "REML", gamma = 3)

summary(gam_mod)

new_train %>% 
  dplyr::mutate(
    prd =  predict(gam_mod, newdata = new_train)
  ) %>% 
  # yardstick::rmse(truth = gradient_axis1, estimate = prd)
  yardstick::rsq(truth = gradient_axis1, estimate = prd)
new_test %>% 
  dplyr::mutate(
    prd =  predict(gam_mod, newdata = new_test)
  ) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = prd)
  # yardstick::rsq(truth = gradient_axis1, estimate = prd)







