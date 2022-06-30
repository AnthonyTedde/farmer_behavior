library(magrittr)
 
data("technico_milk")

source("globals/global_variables.R")

dat <- technico_milk %>% 
  dplyr::select(dplyr::all_of(milk_col_names)) %>% 
  dplyr::select(-c("farmerID", "year"))


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
  dplyr::select(gradient_axis1, dplyr::all_of(milk_col_names)) %>% 
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
# Save the working db ####
# -------------------------------------------------------------------------------------- #

save(working_dat, file = "data/working_dat.rda")


