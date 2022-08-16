library(magrittr)

data("train_season_partial")
data("test_season_partial")
data("pls_final_mdl")
data("milk_season_partial")

source("globals/global_variables.R")

all_dt <- dplyr::bind_rows( 
  train_season_partial %>% tibble::add_column(data_type = "train"),
  test_season_partial %>% tibble::add_column(data_type = "test")
)   

# -------------------------------------------------------------------------------------- #
# VIP score
# -------------------------------------------------------------------------------------- #
  
vip_score <- mixOmics::vip(pls_final_mdl$fit$fit$fit) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble()

vip_score %<>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("comp")) %>% 
  dplyr::group_by(rowname) %>% 
  dplyr::summarise(vip_mean = mean(value)) %>% 
  dplyr::arrange(desc(vip_mean))

threshold <- 1.5
sum(vip_score$vip_mean > threshold)
sum(vip_score$vip_mean)

pred_interest <- vip_score %>% 
  dplyr::filter(vip_mean > threshold) %>% 
  dplyr::pull(rowname)

# barplot(vip_score,
#         beside = TRUE, col = c("lightblue", "mistyrose", "lightcyan"),
#         ylim = c(0, 1.7), legend = rownames(vip_score),
#         main = "Variable Importance in the Projection", font.main = 4)

technico_pca_dt <- all_dt %>% dplyr::select( dplyr::all_of(pred_interest) )
all_pca_dt <- milk_season_partial %>% dplyr::select( dplyr::all_of(pred_interest) )


# -------------------------------------------------------------------------------------- #
# Compute PCA
# -------------------------------------------------------------------------------------- #
threshold <- 99
all_pca <- FactoMineR::PCA(all_pca_dt, ncp = 20, graph = F)
max_comp <- min(which(all_pca$eig[, 3] > threshold))
all_pca <- FactoMineR::PCA(all_pca_dt, ncp = max_comp, graph = F)

all_predict <- predict(all_pca, newdata = all_pca_dt)
technico_milk_predict <- predict(all_pca, newdata = technico_pca_dt)

pca_name <- paste0("dim", stringr::str_pad(seq_len(max_comp), width = 2, pad = "0"))


# -------------------------------------------------------------------------------------- #
# Augment train and test
# -------------------------------------------------------------------------------------- #

milk_season_partial_augmented <- dplyr::bind_rows(
  milk_season_partial %>% 
    dplyr::bind_cols(
      all_predict$coord %>% 
        tibble::as_tibble() %>% 
        setNames(pca_name)
    )
)


all_data_augmented <- dplyr::bind_rows(
  all_dt %>% 
    dplyr::bind_cols(
      technico_milk_predict$coord %>% 
        tibble::as_tibble() %>% 
        setNames(pca_name)
    )
)
  
train_season_partial_augmented <- all_data_augmented %>% 
  dplyr::filter(data_type == "train")

test_season_partial_augmented <- all_data_augmented %>% 
  dplyr::filter(data_type == "test")


# -------------------------------------------------------------------------------------- #
# Compute hclust
# -------------------------------------------------------------------------------------- #


ndim <- max_comp 
keep_dim_pca <- pca_name[seq_len(ndim)]
dat <- milk_season_partial_augmented %>% 
  dplyr::select(dplyr::all_of(keep_dim_pca))
data_dist <- dist(dat, method = "euclidean")   
clst <- hclust(data_dist, method = "ward.D")
# clst <- hclust(data_dist, method = "complete")

# plot(clst)
save(clst, file = "data/clst.rda", compress = "xz")


