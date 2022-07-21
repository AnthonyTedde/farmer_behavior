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

pls_perf_wide %>% 
  dplyr::filter(data_type == "year") %>% 
  dplyr::group_by(data_type, relevance, yearly) %>% 
  dplyr::filter(test_rsq == max(test_rsq)) %>% 
  dplyr::ungroup()

pls_perf_wide %>% 
  dplyr::filter(data_type == "season") %>% 
  # dplyr::filter(fit_type == "bst")
  dplyr::group_by(data_type, relevance, yearly) %>% 
  dplyr::filter(test_rsq == max(test_rsq)) %>% 
  dplyr::ungroup()

pls_perf_wide %>% 
  dplyr::filter(data_type == "winsum") %>% 
  dplyr::group_by(data_type, relevance, yearly) %>% 
  dplyr::filter(test_rsq == max(test_rsq)) %>% 
  dplyr::ungroup()