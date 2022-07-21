library(magrittr)


# -------------------------------------------------------------------------------------- #
# Load data ####
# -------------------------------------------------------------------------------------- #

data("working_year_full")
data("working_season_full")
data("working_season_partial")
data("working_winsum_full")
data("working_winsum_partial")
# milk corrected
data("working_year_corrected_full")
data("working_season_corrected_full")
data("working_season_corrected_partial")
data("working_winsum_corrected_full")
data("working_winsum_corrected_partial")
  
# -------------------------------------------------------------------------------------- #
# Create train / test group ####
# -------------------------------------------------------------------------------------- #

# By farm id
n_farm <- length(unique(working_year_full$farmerID))
prop <- .40

set.seed(1010)
test_id <- sample(seq_len(n_farm), n_farm * prop)
train_id <- setdiff(seq_len(n_farm), test_id)

test_id <- unique(working_year_full$farmerID)[test_id]
train_id <- unique(working_year_full$farmerID)[train_id]

# -------------------------------------------------------------------------------------- #
# Create final datasets ####
# -------------------------------------------------------------------------------------- #

train_year_full <- working_year_full %>% dplyr::filter(farmerID %in% train_id) 
test_year_full <- working_year_full %>% dplyr::filter(farmerID %in% test_id)

# pfat à supprimer + widened the data
train_season_full <- working_season_full %>% dplyr::filter(farmerID %in% train_id) 
test_season_full <- working_season_full %>% dplyr::filter(farmerID %in% test_id)

train_season_partial <- working_season_partial %>% dplyr::filter(farmerID %in% train_id)
test_season_partial <- working_season_partial %>% dplyr::filter(farmerID %in% test_id)

# pfat à supprimer + widened the data
train_winsum_full <- working_winsum_full %>% dplyr::filter(farmerID %in% train_id) 
test_winsum_full <- working_winsum_full %>% dplyr::filter(farmerID %in% test_id)
 
train_winsum_partial <- working_winsum_partial %>% dplyr::filter(farmerID %in% train_id)
test_winsum_partial <- working_winsum_partial %>% dplyr::filter(farmerID %in% test_id)


# ---------------
# corrected 
# ---------------

train_year_corrected_full <- working_year_corrected_full %>% 
  dplyr::filter(farmerID %in% train_id) 
test_year_corrected_full <- working_year_corrected_full %>% 
  dplyr::filter(farmerID %in% test_id)

# pfat à supprimer + widened the data
train_season_corrected_full <- working_season_corrected_full %>% 
  dplyr::filter(farmerID %in% train_id) 
test_season_corrected_full <- working_season_corrected_full %>% 
  dplyr::filter(farmerID %in% test_id)

train_season_corrected_partial <- working_season_corrected_partial %>% 
  dplyr::filter(farmerID %in% train_id)
test_season_corrected_partial <- working_season_corrected_partial %>% 
  dplyr::filter(farmerID %in% test_id)

# pfat à supprimer + widened the data
train_winsum_corrected_full <- working_winsum_corrected_full %>% 
  dplyr::filter(farmerID %in% train_id) 
test_winsum_corrected_full <- working_winsum_corrected_full %>% 
  dplyr::filter(farmerID %in% test_id)
 
train_winsum_corrected_partial <- working_winsum_corrected_partial %>% 
  dplyr::filter(farmerID %in% train_id)
test_winsum_corrected_partial <- working_winsum_corrected_partial %>% 
  dplyr::filter(farmerID %in% test_id)

# is any na ? 
naniar::n_var_miss(train_year_full)
naniar::n_var_miss(test_year_full)
naniar::n_var_miss(train_season_full)
naniar::n_var_miss(test_season_full)
naniar::n_var_miss(train_winsum_full)
naniar::n_var_miss(test_winsum_full)
naniar::n_var_miss(train_season_partial)
naniar::n_var_miss(test_season_partial)
naniar::n_var_miss(train_winsum_partial)
naniar::n_var_miss(test_winsum_partial)

naniar::n_var_miss(train_year_corrected_full)
naniar::n_var_miss(test_year_corrected_full)
naniar::n_var_miss(train_season_corrected_full)
naniar::n_var_miss(test_season_corrected_full)
naniar::n_var_miss(train_winsum_corrected_full)
naniar::n_var_miss(test_winsum_corrected_full)
naniar::n_var_miss(train_season_corrected_partial)
naniar::n_var_miss(test_season_corrected_partial)
naniar::n_var_miss(train_winsum_corrected_partial)
naniar::n_var_miss(test_winsum_corrected_partial)
# -------------------------------------------------------------------------------------- #
# Print some data ####
# -------------------------------------------------------------------------------------- #

dplyr::bind_rows(
  test_year_full %>% dplyr::mutate(type = "test"),
  train_year_full %>% dplyr::mutate(type = "train")
) %>% 
  ggplot2::ggplot(ggplot2::aes(x = gradient_axis1, y = type, fill = type)) +
  ggplot2::ggtitle("Full train / test") +
  ggplot2::geom_violin()


dplyr::bind_rows(
  test_year_full %>% dplyr::mutate(type = "test"),
  train_year_full %>% dplyr::mutate(type = "train")
) %>% 
  dplyr::mutate(year = as.factor(year)) %>% 
  ggplot2::ggplot(ggplot2::aes(y = gradient_axis1, x = year, fill = type)) +
  ggplot2::geom_boxplot()

# -------------------------------------------------------------------------------------- #
# Save ####
# -------------------------------------------------------------------------------------- #



save(train_year_full, file = "data/train_year_full.rda", compress = "xz")
save(test_year_full, file = "data/test_year_full.rda", compress = "xz") 
save(train_season_full, file = "data/train_season_full.rda", compress = "xz")
save(test_season_full, file = "data/test_season_full.rda", compress = "xz")  
save(train_winsum_full, file = "data/train_winsum_full.rda", compress = "xz")
save(test_winsum_full, file = "data/test_winsum_full.rda", compress = "xz") 
save(train_season_partial, file = "data/train_season_partial.rda", compress = "xz")
save(test_season_partial, file = "data/test_season_partial.rda", compress = "xz")  
save(train_winsum_partial, file = "data/train_winsum_partial.rda", compress = "xz")
save(test_winsum_partial, file = "data/test_winsum_partial.rda", compress = "xz") 


save(train_year_corrected_full, 
     file = "data/train_year_corrected_full.rda", 
     compress = "xz")
save(test_year_corrected_full, 
     file = "data/test_year_corrected_full.rda", 
     compress = "xz") 
save(train_season_corrected_full, 
     file = "data/train_season_corrected_full.rda", 
     compress = "xz")
save(test_season_corrected_full, 
     file = "data/test_season_corrected_full.rda", 
     compress = "xz")  
save(train_winsum_corrected_full, 
     file = "data/train_winsum_corrected_full.rda", 
     compress = "xz")
save(test_winsum_corrected_full, 
     file = "data/test_winsum_corrected_full.rda", 
     compress = "xz") 
save(train_season_corrected_partial, 
     file = "data/train_season_corrected_partial.rda", 
     compress = "xz")
save(test_season_corrected_partial, 
     file = "data/test_season_corrected_partial.rda", 
     compress = "xz")  
save(train_winsum_corrected_partial, 
     file = "data/train_winsum_corrected_partial.rda", 
     compress = "xz")
save(test_winsum_corrected_partial, 
     file = "data/test_winsum_corrected_partial.rda", 
     compress = "xz") 
