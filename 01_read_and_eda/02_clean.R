library(magrittr)
data("milk_dat")
data("milk_quarter_dat")


# -------------------------------------------------------------------------------------- #
# ICAR ####
# -------------------------------------------------------------------------------------- #

milk_dat %>% 
  dplyr::pull(fat) %>% hist(breaks = 50)
milk_quarter_dat %>% 
  dplyr::pull(fat) %>% hist(breaks = 50)

milk_dat %>% 
  dplyr::pull(protein) %>% hist(breaks = 50)
milk_quarter_dat %>% 
  dplyr::pull(protein) %>% hist(breaks = 50)

milk_dat %>% 
  dplyr::pull(pprotein_N) %>% hist(breaks = 50)
milk_quarter_dat %>% 
  dplyr::pull(pprotein_N) %>% hist(breaks = 50)

milk_dat %<>% 
  dplyr::filter(fat > 1.5 & fat < 9) %>% 
  dplyr::filter(fat > 1 & fat < 7) 
milk_quarter_dat %<>% 
  dplyr::filter(fat > 1.5 & fat < 9) %>% 
  dplyr::filter(fat > 1 & fat < 7) 


# -------------------------------------------------------------------------------------- #
# Mahalanobis ####
# -------------------------------------------------------------------------------------- #

# ----------------
# Milk quarter
# ----------------

milk_dat <- milk_quarter_dat %>% 
  dplyr::select(-c("farmerID", "year", "year_quarter"))

milk_pca <- FactoMineR::PCA(milk_dat, ncp = 20, graph = F)
ncp <- min(which(milk_pca$eig[, 3] > 99))
milk_pca <- FactoMineR::PCA(milk_dat, ncp = ncp, graph = F)

milk_maha <- mahalanobis(milk_pca$ind$coord,
                         colMeans(milk_pca$ind$coord),
                         cov(milk_pca$ind$coord))

milk_gh <- milk_maha / ncp
# hist(milk_gh, breaks = 50)
plot(milk_pca, label = "none")

mean(milk_gh > 3)

milk_quarter_dat <- milk_quarter_dat[milk_gh < 3, ]


# ----------------
# technico dat
# ----------------

milk_sub_dat <- milk_dat %>% 
  dplyr::select(-c("farmerID", "year"))

milk_pca <- FactoMineR::PCA(milk_sub_dat, ncp = 20, graph = F)
ncp <- min(which(milk_pca$eig[, 3] > 99))
milk_pca <- FactoMineR::PCA(milk_sub_dat, ncp = ncp, graph = F)

milk_maha <- mahalanobis(milk_pca$ind$coord,
                         colMeans(milk_pca$ind$coord),
                         cov(milk_pca$ind$coord))

milk_gh <- milk_maha / ncp
# hist(milk_gh, breaks = 50)
plot(milk_pca, label = "none")

mean(milk_gh > 3)

milk_dat <- milk_dat[milk_gh < 3, ]

# -------------------------------------------------------------------------------------- #
# save ####
# -------------------------------------------------------------------------------------- #

save(milk_dat, 
     file ="data/milk_dat.rda", 
     compress = "xz")
save(milk_quarter_dat, 
     file ="data/milk_quarter_dat.rda", 
     compress = "xz")
