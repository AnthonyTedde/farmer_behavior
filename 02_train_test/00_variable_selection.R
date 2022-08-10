library(magrittr)
 
data("technico_milk_year")
data("technico_milk_season")
data("technico_milk_year_corrected")
data("technico_milk_season_corrected")

data("milk_season_clean_dat")
data("milk_year_clean_dat")

source("globals/global_variables.R")

dat <- technico_milk_year %>% 
  dplyr::select(dplyr::any_of(milk_col_names)) %>% 
  dplyr::select(-c("farmerID", "year"))

naniar::n_miss(technico_milk_season)

# -------------------------------------------------------------------------------------- #
# Check outlier values from FA ####
# -------------------------------------------------------------------------------------- #

a <- technico_milk_year %>% 
  dplyr::select(pFA_med, pFA_shrt, pFA_long) %>% 
  rowSums()
hist(a, breaks = 50)
range(a)

technico_milk_year[a > 1.05, c("farmerID", "year")]


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

rem_dat <- X_cor_flat[X_cor_flat$cor > 0.99, ]

variables_to_remove <- c(
  # Based  on correlation
  setdiff(rem_dat$column, rem_dat$row),
  # Based on name duplication
  # with fat
  "pfat", 
  # with protein
  "pprotein_N__old2", "pprotein_N__old", "pprotein_N",
  # With lactose
  "plactose"
)

working_year_full <- technico_milk_year %>% 
  dplyr::select(gradient_axis1, dplyr::any_of(milk_col_names)) %>% 
  dplyr::select(!dplyr::all_of(variables_to_remove))  

working_year_corrected_full <- technico_milk_year_corrected %>% 
  dplyr::select(gradient_axis1, dplyr::any_of(milk_col_names)) %>% 
  dplyr::select(!dplyr::all_of(variables_to_remove))  

# -------------------------------------------------------------------------------------- #
# Save the working db QUARTERLY ####
# -------------------------------------------------------------------------------------- #

working_season_full <- technico_milk_season %>%
  dplyr::select(gradient_axis1, year_quarter, dplyr::any_of(milk_col_names)) %>% 
  dplyr::select(!dplyr::all_of(variables_to_remove))
  
working_winsum_full <- working_season_full %>% 
  dplyr::filter(year_quarter %in% c("Summer", "Winter"))
  
working_season_corrected_full <- technico_milk_season_corrected %>%
  dplyr::select(gradient_axis1, year_quarter, dplyr::any_of(milk_col_names)) %>% 
  dplyr::select(!dplyr::all_of(variables_to_remove))
  
working_winsum_corrected_full <- working_season_corrected_full %>% 
  dplyr::filter(year_quarter %in% c("Summer", "Winter"))

# Divide the gradient into class
get_gradient_quantile <- function(dat){
  dat %<>% 
    dplyr::mutate(
      gradient_axis1_cat = dplyr::case_when(
        gradient_axis1 <= quantile(gradient_axis1, .20) ~ "HE", # Highly Extensive
        gradient_axis1 <= quantile(gradient_axis1, .40) ~ "E", # Extensive
        gradient_axis1 <= quantile(gradient_axis1, .60) ~ "N", # Neutral
        gradient_axis1 <= quantile(gradient_axis1, .80) ~ "I", # Intensive
        gradient_axis1 <= quantile(gradient_axis1, 1) ~ "HI", # Highly Intensive
      ) %>% factor(levels = c("HE", "E", "N", "I", "HI"))
    ) 
}


# -------------------------------------------------------------------------------------- #
# Get partial ####
# -------------------------------------------------------------------------------------- #
working_season_partial <- get_gradient_quantile(working_season_full)
working_winsum_partial <- get_gradient_quantile(working_winsum_full)

working_season_corrected_partial <- get_gradient_quantile(working_season_corrected_full)
working_winsum_corrected_partial <- get_gradient_quantile(working_winsum_corrected_full)

# 1. anova

get_anova_tbl <- function(dat){
  var_anova <- setdiff(
    names(dat), 
    c("gradient_axis1", "year_quarter", "farmerID", "year", "gradient_axis1_cat")
  ) %>% purrr::map(.f = function(y){ 
    form <- paste(y, "year_quarter*gradient_axis1_cat - 1", sep = " ~ ") %>% 
      formula
    m <- aov(form, data = dat)
    summary(m)
    
    broom::tidy(m) %>% 
      tibble::add_column(variable = y, .before = 1)
  }) %>% 
    purrr::reduce(dplyr::bind_rows) 
}

# Check interaction term
get_higly_significant_interaction <- function(dat, sig = 0.001){
  dat %>% 
    dplyr::filter(term %in% grep("\\:", term, value = T)) %>% 
    dplyr::filter(p.value <= sig) 
}



working_season_anova <- get_anova_tbl(working_season_partial)
working_winsum_anova <- get_anova_tbl(working_winsum_partial)


working_season_high_sig <- get_higly_significant_interaction(working_season_anova)
working_winsum_high_sig <- get_higly_significant_interaction(working_winsum_anova)
working_season_no_sig <- get_higly_significant_interaction(
  working_season_anova,
  sig = 1
)
working_winsum_no_sig <- get_higly_significant_interaction(
  working_winsum_anova,
  sig = 1
)


# Corrected
# ----------

working_season_corrected_anova <- get_anova_tbl(working_season_corrected_partial)
working_winsum_corrected_anova <- get_anova_tbl(working_winsum_corrected_partial)
 
working_season_corrected_high_sig <- get_higly_significant_interaction(
  working_season_corrected_anova
  )
working_winsum_corrected_high_sig <- get_higly_significant_interaction(
  working_winsum_corrected_anova
  )
working_season_corrected_no_sig <- get_higly_significant_interaction(
  working_season_corrected_anova,
  sig = 1
)
working_winsum_corrected_no_sig <- get_higly_significant_interaction(
  working_winsum_corrected_anova,
  sig = 1
)
 

# Rebuilt of the dataset
get_partial_dat <- function(dat, sig_tbl){
  a <- c("gradient_axis1", "year_quarter", "farmerID", "year")
  v <- sig_tbl %>% dplyr::pull(variable)
  dat_wide <- dat %>% 
    dplyr::select(dplyr::all_of(c(a, v))) %>% 
    tidyr::pivot_wider(id_cols = c("farmerID", "year", "gradient_axis1"),
                       names_from = "year_quarter", values_from = v)
  
  
  working_year_full %>% 
    dplyr::select(!dplyr::any_of(v)) %>% 
    dplyr::select(-c("gradient_axis1")) %>% 
    dplyr::inner_join(dat_wide, by = c("farmerID", "year")) %>% 
    dplyr::relocate(gradient_axis1, .before = 1)   
}

working_season_full %<>% get_partial_dat(sig_tbl = working_season_no_sig)
working_season_partial %<>% get_partial_dat(sig_tbl = working_season_high_sig)
working_winsum_full %<>% get_partial_dat(sig_tbl = working_winsum_no_sig)
working_winsum_partial %<>% get_partial_dat(sig_tbl = working_winsum_high_sig)


 
naniar::n_miss(working_season_partial) 
naniar::gg_miss_upset(working_season_partial) 
working_season_partial %<>% 
  tidyr::drop_na()

naniar::n_miss(working_winsum_partial) 
naniar::gg_miss_upset(working_winsum_partial) 
working_winsum_partial %<>% 
  tidyr::drop_na()

naniar::n_miss(working_winsum_full) 
naniar::gg_miss_upset(working_winsum_full) 
working_winsum_full %<>% 
  tidyr::drop_na()

naniar::n_miss(working_season_full) 
naniar::gg_miss_upset(working_season_full) 
working_season_full %<>% 
  tidyr::drop_na()


# -------------------------------------------------------------------------------------- #
# monthly corrected ####
# -------------------------------------------------------------------------------------- #

working_season_corrected_full %<>% get_partial_dat(
  sig_tbl = working_season_corrected_no_sig
  )
working_season_corrected_partial %<>% get_partial_dat(
  sig_tbl = working_season_corrected_high_sig
  )
working_winsum_corrected_full %<>% get_partial_dat(
  sig_tbl = working_winsum_corrected_no_sig
  )
working_winsum_corrected_partial %<>% get_partial_dat(
  sig_tbl = working_winsum_corrected_high_sig
  )

 
naniar::n_miss(working_season_corrected_partial) 
naniar::gg_miss_upset(working_season_corrected_partial) 
working_season_corrected_partial %<>% 
  tidyr::drop_na()

naniar::n_miss(working_winsum_corrected_partial) 
naniar::gg_miss_upset(working_winsum_corrected_partial) 
working_winsum_corrected_partial %<>% 
  tidyr::drop_na()

naniar::n_miss(working_winsum_corrected_full) 
naniar::gg_miss_upset(working_winsum_corrected_full) 
working_winsum_corrected_full %<>% 
  tidyr::drop_na()

naniar::n_miss(working_season_corrected_full) 
naniar::gg_miss_upset(working_season_corrected_full) 
working_season_corrected_full %<>% 
  tidyr::drop_na()


# -------------------------------------------------------------------------------------- #
# Get full milk ####
# -------------------------------------------------------------------------------------- #

get_partial_milk_dat <- function(dat, sig_tbl){
  a <- c("year_quarter", "farmerID", "year")
  v <- sig_tbl %>% dplyr::pull(variable)
  dat_wide <- dat %>% 
    dplyr::select(dplyr::all_of(c(a, v))) %>% 
    tidyr::pivot_wider(id_cols = c("farmerID", "year"),
                       names_from = "year_quarter", values_from = v)
  
  
  milk_year_clean_dat  %>% 
    dplyr::select(!dplyr::any_of(v)) %>% 
    dplyr::inner_join(dat_wide, by = c("farmerID", "year")) 
}

working_season_partial_milk <- milk_season_clean_dat %>% 
  get_partial_milk_dat(sig_tbl = working_season_high_sig)


naniar::n_miss(working_season_partial_milk) 
naniar::gg_miss_upset(working_season_partial_milk) 
working_season_partial_milk %<>% 
  tidyr::drop_na()

# -------------------------------------------------------------------------------------- #
# Save the working db ####
# -------------------------------------------------------------------------------------- #


save(working_year_full, file = "data/working_year_full.rda", compress = "xz")
save(working_season_full, file = "data/working_season_full.rda", compress = "xz")
save(working_season_partial, file = "data/working_season_partial.rda", compress = "xz")
save(working_winsum_full, file = "data/working_winsum_full.rda", compress = "xz")
save(working_winsum_partial, file = "data/working_winsum_partial.rda", compress = "xz")


save(working_year_corrected_full, 
     file = "data/working_year_corrected_full.rda", 
     compress = "xz")
save(working_season_corrected_full, 
     file = "data/working_season_corrected_full.rda", 
     compress = "xz")
save(working_season_corrected_partial, 
     file = "data/working_season_corrected_partial.rda", 
     compress = "xz")
save(working_winsum_corrected_full, 
     file = "data/working_winsum_corrected_full.rda", 
     compress = "xz")
save(working_winsum_corrected_partial, 
     file = "data/working_winsum_corrected_partial.rda", 
     compress = "xz")

save(working_season_partial_milk, 
     file = "data/working_season_partial_milk.rda", 
     compress = "xz")