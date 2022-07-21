data("working_dat")
data("working_dat_quarter")

names(working_dat_quarter)

pairs(working_dat_quarter[, 1:10])

working_dat %>% 
  # dplyr::select(-c("farmerID", "year")) %>% 
  dplyr::select(gradient_axis1, year, farmerID, dplyr::starts_with("pC", ignore.case = F)) %>% 
  tidyr::pivot_longer(cols = -c("gradient_axis1", "farmerID", "year")) %>% 
  dplyr::mutate(year = as.factor(year)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = value, y = gradient_axis1, color = year)) +
  ggplot2::geom_point(alpha = .1) +
  ggplot2::geom_smooth(method = "gam", se = F) +
  ggplot2::facet_wrap("name", scales = "free", ncol = 4)


working_dat %>% 
  # dplyr::select(-c("farmerID", "year")) %>% 
  dplyr::select(gradient_axis1, year, farmerID, dplyr::starts_with("pC", ignore.case = F)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(dplyr::across(dplyr::starts_with("pC"), ~{.x / mean(.x)})) %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_longer(cols = -c("gradient_axis1", "farmerID", "year")) %>% 
  dplyr::mutate(year = as.factor(year)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = value, y = gradient_axis1, color = year)) +
  ggplot2::geom_point(alpha = .1) +
  ggplot2::geom_smooth(method = "gam", se = F) +
  ggplot2::facet_wrap("name", scales = "free", ncol = 4)






data("account_milk_test_full")
data("account_milk_train_full")


dat_gui <- dplyr::bind_rows(account_milk_test_full, account_milk_train_full) %>%
  dplyr::select(CFERME, year, coord_dim1, AG_C18.2.c15) %>% 
  dplyr::rename(pC18.3_c9_c12_c15 = AG_C18.2.c15) %>% 
  dplyr::rename(gradient_axis1 = coord_dim1) %>% 
  dplyr::rename(farmerID = CFERME)


dat <- working_dat
dat <- dat_gui


dat %>% 
  dplyr::select(c("gradient_axis1", "farmerID", "year", "pC18.3_c9_c12_c15")) %>%
  dplyr::mutate(year = as.factor(year)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = pC18.3_c9_c12_c15, y = gradient_axis1, color = year)) +
  ggplot2::geom_point(alpha = .5) +
  ggplot2::geom_smooth(method = "lm", se = F)


dat %>% 
  dplyr::select(c("gradient_axis1", "farmerID", "year", "pC18.3_c9_c12_c15")) %>%
  dplyr::group_by(year) %>% 
  dplyr::mutate(dplyr::across(dplyr::starts_with("pC"), ~{.x / mean(.x)})) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = as.factor(year)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = pC18.3_c9_c12_c15, y = gradient_axis1, color = year)) +
  ggplot2::geom_point(alpha = .5) +
  ggplot2::geom_smooth(method = "lm", se = F)


data("milk_dat")
data("milk_quarter_dat")

# milk_dat %>% 
milk_quarter_dat %>% 
  tibble::as_tibble() %>% 
  dplyr::select(year, fat ) %>% 
  # dplyr::select(year, fat, protein, dplyr::starts_with("pC", ignore.case = F)) %>% 
  # dplyr::select(-pCa) %>% 
  dplyr::mutate(year = as.factor(year)) %>% 
  tidyr::pivot_longer(cols = -c("year")) %>%  
  ggplot2::ggplot(ggplot2::aes(x = year, y = value, color = year)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap("name", ncol = 4, scales = "free")
