library(magrittr)
data("milk_year_02_dat")
data("milk_season_02_dat")


# -------------------------------------------------------------------------------------- #
# ICAR ####
# -------------------------------------------------------------------------------------- #

milk_year_02_dat %>%  dplyr::pull(fat) %>% hist(breaks = 50)
milk_season_02_dat %>%  dplyr::pull(fat) %>% hist(breaks = 50)

milk_year_02_dat %>%  dplyr::pull(protein) %>% hist(breaks = 50)
milk_season_02_dat %>%  dplyr::pull(protein) %>% hist(breaks = 50)

milk_year_02_dat %>%  dplyr::pull(pprotein_N) %>% hist(breaks = 50)
milk_season_02_dat %>%  dplyr::pull(pprotein_N) %>% hist(breaks = 50)

# -------------------------------------------------------------------------------------- #
# Mahalanobis ####
# -------------------------------------------------------------------------------------- #

# ----------------
# Milk season
# ----------------

threshold <- 5

d <- milk_season_02_dat %>% 
  dplyr::select(-c("farmerID", "year", "year_quarter"))

milk_pca <- FactoMineR::PCA(d, ncp = 30, graph = F)
ncp <- min(which(milk_pca$eig[, 3] > 99))
milk_pca <- FactoMineR::PCA(d, ncp = ncp, graph = F)

milk_maha <- mahalanobis(milk_pca$ind$coord,
                         colMeans(milk_pca$ind$coord),
                         cov(milk_pca$ind$coord))

milk_gh <- milk_maha / ncp
# hist(milk_gh, breaks = 50)
plot(milk_pca, label = "none")

mean(milk_gh > threshold)

milk_season_02_dat <- milk_season_02_dat[milk_gh < threshold, ]


# ----------------
# year dat
# ----------------

milk_sub_dat <- milk_year_02_dat %>% 
  dplyr::select(-c("farmerID", "year"))

milk_pca <- FactoMineR::PCA(milk_sub_dat, ncp = 30, graph = F)
ncp <- min(which(milk_pca$eig[, 3] > 99))
milk_pca <- FactoMineR::PCA(milk_sub_dat, ncp = ncp, graph = F)

milk_maha <- mahalanobis(milk_pca$ind$coord,
                         colMeans(milk_pca$ind$coord),
                         cov(milk_pca$ind$coord))

milk_gh <- milk_maha / ncp
# hist(milk_gh, breaks = 50)
plot(milk_pca, label = "none")

mean(milk_gh > threshold)

milk_year_02_dat <- milk_year_02_dat[milk_gh < threshold, ]

# -------------------------------------------------------------------------------------- #
# save ####
# -------------------------------------------------------------------------------------- #

milk_year_clean_dat <- milk_year_02_dat
milk_season_clean_dat <- milk_season_02_dat

save(milk_year_clean_dat, 
     file ="data/milk_year_clean_dat.rda", 
     compress = "xz")
save(milk_season_clean_dat, 
     file ="data/milk_season_clean_dat.rda", 
     compress = "xz")
