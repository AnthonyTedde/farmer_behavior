library(magrittr)

source("globals/global_variables.R")
data("technico_dat")
data("milk_year_clean_dat")
data("milk_season_clean_dat")
data("milk_year_corrected_clean_dat")
data("milk_season_corrected_clean_dat")

technico_dat$exercice %>% table

# ----------------------------------------------------------------------------------------
# Compute Dalcq AC gradient ####
# ----------------------------------------------------------------------------------------

t <- technico_dat %>% 
  # We select the variables genuinly selected by Dalcq AC for the gradient
  dplyr::select(dplyr::all_of(tech_gradient_var))
  
npc <- 2 
technico_pca <- FactoMineR::PCA(t, ncp = npc)
#factoextra::fviz_pca_ind(technico_pca, geom.ind = "point")

technico_maha <- mahalanobis(technico_pca$ind$coord,
                             center = colMeans(technico_pca$ind$coord),
                             cov = cov(technico_pca$ind$coord))
technico_gh <- technico_maha / npc

mean(technico_gh > 5)
# "Outliers are set above 5
t <- t[technico_gh < 5, ]
technico_dat <- technico_dat[technico_gh < 5, ]

technico_pca <- FactoMineR::PCA(t, ncp = npc)
plot(technico_pca, choix = "ind", label = "none")


technico_dat %<>% 
  # dplyr::filter(technico_gh < 5) %>% 
  dplyr::bind_cols(
    technico_pca$ind$coord %>% 
      tibble::as_tibble() %>% 
      setNames(nm = c("gradient_axis1", "gradient_axis2"))
  )


# ----------------------------------------------------------------------------------------
# Merge milk data with Dalcq AC gradient ####
# ----------------------------------------------------------------------------------------
  
match <- read.table("raw_data/matching_tbl.txt", 
                    header = FALSE, 
                    col.names = matching_col_names)

match %<>%  
  dplyr::select(farmerID, CFERME) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(CFERME != 0)
head(match) 

technico_milk_year <- technico_dat %>% 
  dplyr::inner_join(match) %>% 
  dplyr::mutate(
    year = stringr::str_pad(exercice, width = 3, pad = "0") %>%
    # year = stringr::str_pad(EXERCICE, width = 3, pad = "0") %>% 
      paste0(2, .) %>% as.integer()
  ) %>%   
  dplyr::inner_join(milk_year_clean_dat, by = c("farmerID", "year")) %>% 
  tibble::as_tibble()

technico_milk_year_corrected <- technico_dat %>% 
  dplyr::inner_join(match) %>% 
  dplyr::mutate(
    year = stringr::str_pad(exercice, width = 3, pad = "0") %>%
    # year = stringr::str_pad(EXERCICE, width = 3, pad = "0") %>% 
      paste0(2, .) %>% as.integer()
  ) %>%   
  dplyr::inner_join(milk_year_corrected_clean_dat, by = c("farmerID", "year")) %>% 
  tibble::as_tibble()


save(technico_milk_year, 
     file = "data/technico_milk_year.rda", 
     compress = "xz")

save(technico_milk_year_corrected, 
     file = "data/technico_milk_year_corrected.rda",
     compress = "xz")


# ----------------------------------------------------------------------------------------
# Merge milk data with Dalcq AC gradient QUARTERLY ####
# ----------------------------------------------------------------------------------------

technico_milk_season <- technico_dat %>% 
  dplyr::inner_join(match) %>% 
  dplyr::mutate(
    year = stringr::str_pad(exercice, width = 3, pad = "0") %>%
    # year = stringr::str_pad(EXERCICE, width = 3, pad = "0") %>% 
      paste0(2, .) %>% as.integer()
  ) %>%   
  dplyr::inner_join(milk_season_clean_dat, by = c("farmerID", "year")) %>% 
  tibble::as_tibble()

technico_milk_season_corrected <- technico_dat %>% 
  dplyr::inner_join(match) %>% 
  dplyr::mutate(
    year = stringr::str_pad(exercice, width = 3, pad = "0") %>%
    # year = stringr::str_pad(EXERCICE, width = 3, pad = "0") %>% 
      paste0(2, .) %>% as.integer()
  ) %>%   
  dplyr::inner_join(milk_season_corrected_clean_dat, by = c("farmerID", "year")) %>% 
  tibble::as_tibble()

save(technico_milk_season, 
     file = "data/technico_milk_season.rda", 
     compress = "xz")

save(technico_milk_season_corrected, 
     file = "data/technico_milk_season_corrected.rda",
     compress = "xz")