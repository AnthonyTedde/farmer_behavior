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
  dplyr::filter(perf_type == "test", data_type == "season", relevance == "partial") %>%
  dplyr::filter(perf_type == "test", ) %>%
  dplyr::filter(
    ( yearly == "Uncorrected" & fit_type == "onese") | 
    ( yearly == "Corrected" & fit_type == "bst")
  ) %>% 
  dplyr::mutate(absolute_residuals = abs(gradient_axis1 - prd)) %>%
  # dplyr::mutate(absolute_residuals = gradient_axis1 - prd) %>%
  dplyr::select(farmerID, year, yearly, absolute_residuals) %>% 
  tidyr::pivot_wider(names_from = "yearly", values_from = "absolute_residuals") %>% 
  dplyr::mutate(diff = Corrected - Uncorrected) %>% 
  tidyr::drop_na()

difference$diff %>% hist
a <- ( (difference$diff - 0 ) / sd(difference$diff) )

b <- ( (mean(difference$diff) - 0 ) / sd(difference$diff) )

x <- seq(-4, 4, length=100)
hx <- dt(x, df = nrow(difference)) 

# plot(x, hx, type="l", lty=2, xlab="x value",
#      ylab="Density", main="Comparison of t Distributions")

# hist(a)
plot(density(a), lwd = 2, col = "red", main = "Density")
lines(x, hx, lwd=2)
abline(v = b)



# -------------------------------------------------------------------------------------- #
# Plot
# -------------------------------------------------------------------------------------- #


dt(difference$diff, df = nrow(difference) - 1, )

pls_perf_full %>% 
  dplyr::filter(perf_type == "train", data_type == "season", relevance == "partial") %>%
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



