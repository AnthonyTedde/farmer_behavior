
library(magrittr)

data("technico_dat")

technico_dat$exercice %>% table
t <- technico_dat %>% 
  dplyr::filter(LAITVT != 0) %>%
  dplyr::select(dplyr::all_of(tech_gradient_var))
  
 
pca <- FactoMineR::PCA(technico_dat[-c(1, 2)], ncp = 2)
pca <- FactoMineR::PCA(t, ncp = 2)
factoextra::fviz_pca_ind(pca, geom.ind = "point")

# Grad = coord from first component
coord_dim1 <- pca$ind$coord[,1]
coord_dim2 <- pca$ind$coord[,2]
# Grad creation with scale 0-100%
coord_dim1 <- ((coord_dim1+abs(min(coord_dim1)))/
                 (max(coord_dim1)+abs(min(coord_dim1))))*100

technico_gradient <- data %>%
  dplyr::mutate(coord_dim1 = factor(coord_dim1),
                coord_dim2 = factor(coord_dim2))


# Merge
source("globals/global_variables.R")
data("milk_dat")
match <- read.table("raw_data/matching_tbl.txt", 
                    header = FALSE, 
                    col.names = matching_col_names)

match %<>%  
  dplyr::select(farmerID, CFERME) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(CFERME != 0)
head(match) 

technico_milk <- technico_dat %>% 
  dplyr::inner_join(match) %>% 
  dplyr::mutate(
    # year = stringr::str_pad(exercice, width = 3, pad = "0") %>% 
    year = stringr::str_pad(EXERCICE, width = 3, pad = "0") %>% 
      paste0(2, .) %>% as.integer()
  ) %>%   
  dplyr::inner_join(milk_dat, by = c("farmerID", "year")) %>% 
  tibble::as_tibble()

t <- technico_milk %>% 
  dplyr::filter(LAITVT != 0) %>%
  dplyr::select(dplyr::all_of(tech_gradient_var))

t$LAITVT %>% hist

pca <- FactoMineR::PCA(t, scale.unit = T, ncp = 2)

plot(pca, choix = "var")

factoextra::fviz_pca_ind(pca, geom.ind = "point")

technico_maha <- mahalanobis(pca$ind$coord,
                             center = colMeans(pca$ind$coord),
                             cov = cov(pca$ind$coord))
technico_gh <- technico_maha / 2

mean(technico_gh > 5)

technico_


