library(magrittr)
 
data("technico_milk")
data("technico_milk_quarter")

source("globals/global_variables.R")

dat <- technico_milk %>% 
  dplyr::select(dplyr::any_of(milk_col_names)) %>% 
  dplyr::select(-c("farmerID", "year"))

naniar::n_miss(technico_milk_quarter)


# -------------------------------------------------------------------------------------- #
# Remove variables bringing the same information ####
# -------------------------------------------------------------------------------------- #

X_cor <- cor(dat)

upper<-X_cor
upper[upper.tri(X_cor)]<-""
mat<-as.data.frame(upper)

ut <- upper.tri(X_cor)
X_cor_flat <-   data.frame(
  row = rownames(X_cor)[row(X_cor)[ut]],
  column = rownames(X_cor)[col(X_cor)[ut]],
  cor  = X_cor[ut]
)

X_cor_flat[X_cor_flat$cor > 0.95, ]

variables_to_remove <- c(
  "pfat", "pch4_mir", "pprotein_N", "pprotein_N__old", "pcasein_tot", "pprotein_N__old2",
  "pC18.1_c9__tot", "pFA_mono", "pFA_insat", "pFA_shrt", "pFA_long", "pFA_o3", "pFA_trs__tot",
  "pC18.1__tot", "pcheese_yield_curd", "pRFI1"
)

working_dat <- technico_milk %>% 
  dplyr::select(gradient_axis1, dplyr::any_of(milk_col_names)) %>% 
  dplyr::select(!dplyr::all_of(variables_to_remove))


# -------------------------------------------------------------------------------------- #
# Mahalanobis GH sample selection ####
# -------------------------------------------------------------------------------------- #

milk_dat <- working_dat %>% 
  dplyr::select(dplyr::any_of(milk_col_names)) %>% 
  dplyr::select(-c("farmerID", "year"))

milk_pca <- FactoMineR::PCA(milk_dat, ncp = 20)
ncp <- min(which(milk_pca$eig[, 3] > 99))
milk_pca <- FactoMineR::PCA(milk_dat, ncp = ncp)

milk_maha <- mahalanobis(milk_pca$ind$coord,
                         colMeans(milk_pca$ind$coord),
                         cov(milk_pca$ind$coord))

milk_gh <- milk_maha / ncp
hist(milk_gh)
plot(milk_pca, label = "none")

mean(milk_gh > 5)

working_dat <- working_dat[milk_gh < 5, ]

# Just plot the difference after mahalanobis filter
milk_dat <- working_dat %>% 
  dplyr::select(dplyr::any_of(milk_col_names)) %>% 
  dplyr::select(-c("farmerID", "year"))

milk_pca <- FactoMineR::PCA(milk_dat, ncp = ncp, graph = F)
plot(milk_pca, label = "none")


# -------------------------------------------------------------------------------------- #
# Save the working db QUARTERLY ####
# -------------------------------------------------------------------------------------- #

technico_milk_quarter %<>%
  dplyr::select(gradient_axis1, year_quarter, dplyr::any_of(milk_col_names)) %>% 
  dplyr::select(!dplyr::all_of(variables_to_remove))

technico_milk_quarter_sub <- technico_milk_quarter %>% 
  dplyr::filter(year_quarter %in% c("Summer", "Winter"))
  

# Divide the gradient into class
technico_milk_quarter_sub %<>% 
  dplyr::mutate(
    gradient_axis1_cat = dplyr::case_when(
      gradient_axis1 <= quantile(gradient_axis1, .20) ~ "HE", # Highly Extensive
      gradient_axis1 <= quantile(gradient_axis1, .40) ~ "E", # Extensive
      gradient_axis1 <= quantile(gradient_axis1, .60) ~ "N", # Neutral
      gradient_axis1 <= quantile(gradient_axis1, .80) ~ "I", # Intensive
      gradient_axis1 <= quantile(gradient_axis1, 1) ~ "HI", # Highly Intensive
    ) %>% factor(levels = c("HE", "E", "N", "I", "HI"))
  )

# 1. anova

var_anova <- setdiff(
  names(technico_milk_quarter_sub), 
  c("gradient_axis1", "year_quarter", "farmerID", "year", "gradient_axis1_cat")
) %>% purrr::map(.f = function(y){ 
  form <- paste(y, "year_quarter*gradient_axis1_cat - 1", sep = " ~ ") %>% 
    formula
  m <- aov(form, data = technico_milk_quarter_sub)
  summary(m)
  
  broom::tidy(m) %>% 
    tibble::add_column(variable = y, .before = 1)
}) %>% 
  purrr::reduce(dplyr::bind_rows)


# Check interaction term
high_sig_interaction <- var_anova %>% 
  dplyr::filter(term %in% grep("\\:", term, value = T)) %>% 
  # ggplot2::ggplot(ggplot2::aes(x = variable, y = p.value)) +
  # ggplot2::geom_col()
  dplyr::filter(p.value <= 0.001)


var_anova %>% 
  dplyr::filter(term %in% grep("\\:", term, value = T)) %>% 
  # ggplot2::ggplot(ggplot2::aes(x = variable, y = p.value)) +
  # ggplot2::geom_col()
  dplyr::arrange(p.value)
  # dplyr::filter(p.value == min(p.value))

high_sig_interaction %>% 
  dplyr::arrange(dplyr::desc(p.value))

technico_milk_quarter_sub %>% 
  ggplot2::ggplot(ggplot2::aes(x = year_quarter, y = pC18.1_trs__tot, color = gradient_axis1_cat)) +
  ggplot2::geom_boxplot()

technico_milk %>% 
  dplyr::mutate(
    gradient_axis1_cat = dplyr::case_when(
      gradient_axis1 <= quantile(gradient_axis1, .20) ~ "HE", # Highly Extensive
      gradient_axis1 <= quantile(gradient_axis1, .40) ~ "E", # Extensive
      gradient_axis1 <= quantile(gradient_axis1, .60) ~ "N", # Neutral
      gradient_axis1 <= quantile(gradient_axis1, .80) ~ "I", # Intensive
      gradient_axis1 <= quantile(gradient_axis1, 1) ~ "HI", # Highly Intensive
    ) %>% factor(levels = c("HE", "E", "N", "I", "HI"))
  ) %>% 
  dplyr::mutate(year_quarter = "full_year") %>% 
  dplyr::bind_rows(technico_milk_quarter_sub) %>% 
  ggplot2::ggplot(ggplot2::aes(x = year_quarter, 
                               # y = pC18.1_trs__tot,
                               # y = pFA_sat, # Worse (?)
                               # y = pC18.2__tot, # Worse (?)
                               y = pC18.1_c9, # Worse (?)
                               # y = urea, 
                               color = gradient_axis1_cat)) +
  ggplot2::geom_boxplot()


# Rebuilt of the dataset
a <- c("gradient_axis1", "year_quarter", "farmerID", "year")
v <- high_sig_interaction %>% dplyr::pull(variable)
technico_milk_quarter_wide <- technico_milk_quarter %>% 
  dplyr::select(dplyr::all_of(c(a, v))) %>% 
  dplyr::filter(year_quarter %in% c("Summer", "Winter")) %>% 
  tidyr::pivot_wider(id_cols = c("farmerID", "year", "gradient_axis1"),
                     names_from = "year_quarter", values_from = v)

naniar::n_miss(technico_milk_quarter_wide)

naniar::gg_miss_upset(technico_milk_quarter_wide)

technico_milk_quarter_wide %<>% 
  tidyr::drop_na()

working_dat_quarter <- working_dat %>% 
  dplyr::select(!dplyr::any_of(v)) %>% 
  dplyr::select(-c("gradient_axis1")) %>% 
  dplyr::inner_join(technico_milk_quarter_wide, by = c("farmerID", "year")) %>% 
  dplyr::relocate(gradient_axis1, .after = year)  


# -------------------------------------------------------------------------------------- #
# Save the working db ####
# -------------------------------------------------------------------------------------- #

save(working_dat, file = "data/working_dat.rda", compress = "xz")
save(working_dat_quarter, file = "data/working_dat_quarter.rda", compress = "xz")


