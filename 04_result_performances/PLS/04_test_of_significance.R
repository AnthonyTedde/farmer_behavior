library(magrittr)

data("pls_perf_corrected_full")
data("pls_perf_uncorrected_full")
data("pls_final_rslt")

pls_perf_full <- dplyr::bind_rows( pls_perf_corrected_full, pls_perf_uncorrected_full )

pls_perf_full %<>% 
  dplyr::mutate(
    relevance = stringr::str_split(data_type, pattern = "_", simplify = T)[, 2],
    .after = data_type
  ) %>% 
  dplyr::mutate( 
    data_type = stringr::str_split(data_type, pattern = "_", simplify = T)[, 1],
  ) %>% 
  dplyr::inner_join(
    pls_final_rslt %>% 
      dplyr::select(model, yearly, data_type, relevance, fit_type)
  ) %>% 
      dplyr::select(model, farmerID, year, yearly, data_type, relevance, fit_type,
                    gradient_axis1, prd, perf_type) %>% 
  tibble::rowid_to_column()
  
pls_perf_train_full <- pls_perf_full %>% 
  dplyr::filter(perf_type == "train")
pls_perf_test_full <- pls_perf_full %>% 
  dplyr::filter(perf_type == "test")   

# -------------------------------------------------------------------------------------- #
# Is is worth to correct the data ? ####
# -> It adds a preprocess step.
# -------------------------------------------------------------------------------------- #
# m01-m05 v.s. m06-m10
# We compare with the test perforamnce

pls_perf_test_full %>% 
  dplyr::filter(yearly == "Uncorrected")

difference <- pls_perf_test_full %>% 
  dplyr::filter(model %in% c("m03", "m08")) %>% 
  dplyr::mutate(absolute_residuals = abs(gradient_axis1 - prd)) %>%
  # dplyr::filter(yearly == "Uncorrected")
  dplyr::select(farmerID, year, data_type, relevance, yearly, absolute_residuals) %>% 
  # dplyr::filter(yearly == "Uncorrected")
  # Yearly is Corrected | Uncorrected
  tidyr::pivot_wider(names_from = "yearly", values_from = "absolute_residuals") %>%  
  dplyr::mutate(diff = Corrected - Uncorrected) 
  

Xstd <- ( 
  (mean(difference$diff) - 0 ) / 
    ( sd(difference$diff) / sqrt(nrow(difference)) ) 
)

pt(Xstd, df = nrow(difference) - 1, lower.tail = F) * 2
# --> Not significative. We can keep one we want.
# So we choose the simplest: partial, season, uncorrected onese (m03)


x <- seq(-4, 4, length=100)
hx <- dt(x, df = nrow(difference)) 

plot(x, hx, lwd=2)
abline(v = Xstd, col = "red")


# -------------------------------------------------------------------------------------- #
# Fit final model
# -------------------------------------------------------------------------------------- #

pls_final_rslt %>% 
  dplyr::filter(model == "m03")

data("train_season_partial_rslt")
data("train_season_partial_wfl")

data("train_season_partial")
data("test_season_partial")

pls_final_data <- dplyr::bind_rows( train_season_partial, test_season_partial ) 

onese <- train_season_partial_rslt %>% 
  tune::select_by_one_std_err(num_comp)

pls_final_mdl <- train_season_partial_wfl %>% 
  tune::finalize_workflow(onese) %>% 
  parsnip::fit(pls_final_data)

# mixOmics::plotVar(pls_final_mdl$fit$fit$fit)
mixOmics::plotLoadings(pls_final_mdl$fit$fit$fit, comp = 2)
# mixOmics::network(pls_final_mdl$fit$fit$fit)
# mixOmics::cim(pls_final_mdl$fit$fit$fit)
mixOmics::plotArrow(pls_final_mdl$fit$fit$fit)
mixOmics::plotIndiv(pls_final_mdl$fit$fit$fit)

save(pls_final_mdl, file = "data/pls_final_mdl.rda", compress = "xz")
save(pls_final_data, file = "data/pls_final_data.rda", compress = "xz")

















