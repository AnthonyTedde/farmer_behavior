library(magrittr)

data("train_season_partial")
data("test_season_partial")
data("pls_final_mdl")

source("globals/global_variables.R")

train_pca_dt <- train_season_partial %>% 
  dplyr::select(-c("gradient_axis1", "farmerID",  "year"))
test_pca_dt <- test_season_partial %>% 
  dplyr::select(-c("gradient_axis1", "farmerID",  "year"))

# -------------------------------------------------------------------------------------- #
# Compute PCA
# -------------------------------------------------------------------------------------- #

train_pca <- FactoMineR::PCA(train_pca_dt, ncp = 20, graph = F)
max_comp <- min(which(train_pca$eig[, 3] > 99))
train_pca <- FactoMineR::PCA(train_pca_dt, ncp = max_comp, graph = F)

train_predict <- predict(train_pca, newdata = train_pca_dt)
test_predict <- predict(train_pca, newdata = test_pca_dt)

pca_name <- paste0("dim", stringr::str_pad(seq_len(max_comp), width = 2, pad = "0"))


# -------------------------------------------------------------------------------------- #
# Augment train and test
# -------------------------------------------------------------------------------------- #

train_season_partial_augmented <- train_season_partial %>% 
  dplyr::bind_cols(
    train_predict$coord %>% 
      tibble::as_tibble() %>% 
      setNames(pca_name)
  ) %>% 
  dplyr::mutate(
    prd = predict(pls_final_mdl, new_data = train_season_partial) %>% dplyr::pull(),
    .after = gradient_axis1
  ) %>% 
  tibble::add_column(data_type = "train")

test_season_partial_augmented <- test_season_partial %>% 
  dplyr::bind_cols(
    test_predict$coord %>% 
      tibble::as_tibble() %>% 
      setNames(pca_name)
  ) %>% 
  dplyr::mutate(
    prd = predict(pls_final_mdl, new_data = test_season_partial) %>% dplyr::pull(),
    .after = gradient_axis1
  ) %>% 
  tibble::add_column(data_type = "test")

## Bind train and test
all_data_augmented <- dplyr::bind_rows(
  train_season_partial_augmented,
  test_season_partial_augmented
)

# -------------------------------------------------------------------------------------- #
# Compute hclust
# -------------------------------------------------------------------------------------- #


ndim <- c(2, 3, 4, seq(5, max_comp, by = 5))
clst_lst <- vector(mode = "list", length = length(ndim))

for ( i in seq_along(ndim) ){
  n <- ndim[i]
  keep_dim_pca <- pca_name[seq_len(n)]
  dat <- all_data_augmented %>% 
    dplyr::select(dplyr::all_of(keep_dim_pca))
  data_dist <- dist(dat)   
  clst <- hclust(data_dist, method = "ward.D")
  clst_lst[[i]] <- clst
}


# -------------------------------------------------------------------------------------- #
# cut the trees
# -------------------------------------------------------------------------------------- #


cut_df <- seq_along(ndim) %>% purrr::map_dfr(.f = function(i){ 
  clst <- clst_lst[[i]]
  n <- ndim[i]
  keep_dim_pca <- pca_name[seq_len(n)]
  slice <- 2:5
  slice %>% purrr::map_dfc(.f = function(c){
    cutree(clst, k = c)
  }) %>% 
    setNames(paste0("slice", slice)) %>% 
    dplyr::mutate(comp_pca = dplyr::last(keep_dim_pca))
})


# -------------------------------------------------------------------------------------- #
# Save data
# -------------------------------------------------------------------------------------- #
save(clst_lst, file = "data/clst_lst.rda", compress = "xz")
save(cut_df, file = "data/cut_df.rda", compress = "xz")
save(all_data_augmented, file = "data/all_data_augmented.rda", compress = "xz")