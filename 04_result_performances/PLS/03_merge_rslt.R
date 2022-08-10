library(magrittr)

data("uncorrected_pls_perf")
data("corrected_pls_perf")


pls_perf <- dplyr::bind_rows(uncorrected_pls_perf, corrected_pls_perf) %>% 
  dplyr::mutate(
    relevance = stringr::str_split(data_type, pattern = "_", simplify = T)[, 2],
    .after = data_type
  ) %>% 
  dplyr::mutate( 
    data_type = stringr::str_split(data_type, pattern = "_", simplify = T)[, 1],
  )


pls_perf_wide <- pls_perf %>% 
  dplyr::select(-.estimator) %>% 
  tidyr::pivot_wider(names_from = c("perf_type", ".metric"),
                     values_from = ".estimate")

year_perf <- pls_perf_wide %>% 
  dplyr::filter(data_type == "year") %>% 
  dplyr::group_by(data_type, relevance, yearly) %>% 
  # dplyr::filter(test_rsq == max(test_rsq)) %>% 
  dplyr::ungroup()

season_perf <- pls_perf_wide %>% 
  dplyr::filter(data_type == "season") %>% 
  dplyr::group_by(data_type, relevance, yearly) %>% 
  # dplyr::filter(test_rsq == max(test_rsq)) %>% 
  dplyr::ungroup()

winsum_perf <- pls_perf_wide %>% 
  dplyr::filter(data_type == "winsum") %>% 
  dplyr::group_by(data_type, relevance, yearly) %>% 
  # dplyr::filter(test_rsq == max(test_rsq)) %>% 
  dplyr::ungroup()


pls_final_rslt <- dplyr::bind_rows(year_perf, season_perf, winsum_perf) %>% 
  dplyr::relocate(yearly, .before = 1) %>% 
  dplyr::mutate(
    yearly = factor(yearly, levels = c("Uncorrected", "Corrected")),
    data_type = factor(data_type, levels = c("year", "season", "winsum")),
    relevance = factor(relevance, levels = c("full", "partial"))
  ) %>% 
  dplyr::arrange(yearly, data_type, relevance) %>% 
  dplyr::relocate(train_rsq, .before = train_rmse) %>% 
  dplyr::relocate(test_rsq, .before = test_rmse) 
# %>% 
#   dplyr::mutate(model = paste0("m", stringr::str_pad(1:10, width = 2, pad = "0")),
#                 .before = 1)

save(pls_final_rslt, file = "data/pls_final_rslt.rda")


data("train_year_full_rslt")
data("train_season_full_rslt")
data("train_season_partial_rslt")

hyperparam <- list(
  list(data_type = "year", relevance = "full"),
  list(data_type = "season", relevance = "full"),
  list(data_type = "season", relevance = "partial")
  ) %>% purrr::map(.f = function(x){
    rslt <- paste("train", x$data_type, x$relevance, "rslt", sep = "_") %>% 
      get
    bst <- rslt %>% tune::select_best() %>% 
      dplyr::select(predictor_prop, num_comp) %>% 
      tibble::add_column(
        data_type = x$data_type,
        relevance = x$relevance,
        fit_type = "bst"
      )
    onese <- rslt %>% tune::select_by_one_std_err(num_comp) %>% 
      dplyr::select(predictor_prop, num_comp) %>% 
      tibble::add_column(
        data_type = x$data_type,
        relevance = x$relevance,
        fit_type = "onese"
      )
    dplyr::bind_rows(bst, onese)
  }) %>% 
  dplyr::bind_rows()

rslt <- pls_final_rslt %>% 
  dplyr::filter(yearly == "Uncorrected", data_type != "winsum") %>% 
  dplyr::inner_join(hyperparam) %>% 
  dplyr::mutate(working_db = paste(data_type, relevance, sep ="_"),
                .before = 1) %>% 
  dplyr::select(-c("yearly", "data_type", "relevance")) %>% 
  dplyr::relocate(predictor_prop, .after = fit_type) %>% 
  dplyr::relocate(num_comp, .after = predictor_prop) %>% 
  dplyr::arrange(fit_type) %>% 
  dplyr::mutate(dplyr::across(
    .cols = c("predictor_prop", "train_rsq", "train_rmse", "test_rsq", "test_rmse"), 
    round, digits = 2)
  )


write.csv(rslt, file = "article_data/rslt.csv")