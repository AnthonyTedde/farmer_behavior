# Load magrittr pipe operator
`%>%` <- magrittr::`%>%`

data("pls_final_data_augmented")
data("milk_season_partial_augmented")


# -------------------------------------------------------------------------------------- #
# plot function
# -------------------------------------------------------------------------------------- #

# dat <- pls_final_data_augmented
dat <- milk_season_partial_augmented
axis1 <- "pls_dim01"
axis2 <- "pls_dim02"
# var <- "gradient_axis1"
# hname <- "Genuine gradient"
var <- "prd"
hname <- "Predicted gradient"

contour_plt <- function(dat, axis1, axis2, var){
  
  x <- dat[, axis1, drop = T]
  y <- dat[, axis2, drop = T]
  z <- dat[, var, drop = T]
  
  grid_grad <-  interp::interp( x, y, z, nx = 500, ny = 500)
  
  griddf_grad <- tibble::tibble(
    x = rep(grid_grad$x, nrow(grid_grad$z)),
    y = rep(grid_grad$y, each = ncol(grid_grad$z)),
    z = as.numeric(grid_grad$z)
  ) %>% 
    tidyr::drop_na() %>% 
    setNames(nm = c("axis1", "axis2", "gradient")) %>% 
    tibble::add_column(
      # type = hname,
      axis1_nam = axis1,
      axis2_nam = axis2
    ) 
  
  
  pal <- ggsci::pal_simpsons()(5)
  
  
  griddf_grad %>% 
    ggplot2::ggplot(ggplot2::aes(x = axis1, y = axis2, z = gradient)) +
    ggplot2::geom_contour_filled(
      breaks = quantile(pls_final_data_augmented$gradient_axis1,
                        probs = seq(0, 1, 0.2))
    ) +
    ggplot2::scale_color_manual(
      name = "Clustering based interpretation: ",
      values = pal[c(1, 2, 5)]) +
    ggplot2::scale_fill_manual(
      values = pal,
      name = "Gradient level: ",
      labels = c("Highly extensive",
                 "Extensive",
                 "Neutral",
                 "Intensive",
                 "Highly intensive")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
                   # legend.title = ggplot2::element_blank())
}

# Create contour
# pls
gradient_pls <- contour_plt(dat = pls_final_data_augmented, 
                            axis1 = "pls_dim01",
                            axis2 = "pls_dim02", 
                            var = "gradient_axis1") +
  ggplot2::ggtitle("Observed gradient projected to the first two components")

prd_pls <- contour_plt(dat = pls_final_data_augmented, 
                       axis1 = "pls_dim01",
                       axis2 = "pls_dim02", 
                       var = "prd") +
  ggplot2::ggtitle("Predicted gradient projected to the first two components")


dat <- pls_final_data_augmented %>% 
  dplyr::rename(gradient = prd, 
                axis1 = pls_dim01,
                axis2 = pls_dim02)
# dplyr::filter(! cluster_explicit %in% c("Cluster_neutral1", "Cluster_neurtal2")) 
prd_full_pls <- contour_plt(dat = milk_season_partial_augmented, 
                            axis1 = "pls_dim01",
                            axis2 = "pls_dim02", 
                            var = "prd") +
  ggforce::geom_mark_ellipse(ggplot2::aes(label = "Extrapolate outside"),
                             data = dat,
                             expand = ggplot2::unit(0, "mm"), size = 1) +
  ggplot2::ggtitle("Predicted gradient projected to the first two components (full milk db)")
# pca

dat <- pls_final_data_augmented %>% 
  dplyr::rename(gradient = gradient_axis1, 
                axis1 = dim01,
                axis2 = dim02) %>% 
  dplyr::filter(! cluster_explicit %in% c("Cluster neutral1", "Cluster neurtal2")) 
gradient_pca <- contour_plt(dat = pls_final_data_augmented, 
                            axis1 = "dim01",
                            axis2 = "dim02", 
                            var = "gradient_axis1") + 
  ggforce::geom_mark_ellipse(
    ggplot2::aes(group = cluster_explicit, 
        color = cluster_explicit),
    data = dat,
    expand = ggplot2::unit(0, "mm"), size = 1
  ) +
  ggplot2::ggtitle("Observed gradient projected to the first two components")

dat <- pls_final_data_augmented %>% 
  dplyr::rename(gradient = prd, 
                axis1 = dim01,
                axis2 = dim02) %>% 
  dplyr::filter(! cluster_explicit %in% c("Cluster neutral1", "Cluster neurtal2")) 
prd_pca <- contour_plt(dat = pls_final_data_augmented, 
                       axis1 = "dim01",
                       axis2 = "dim02", 
                       var = "prd") +
  ggforce::geom_mark_ellipse(
    ggplot2::aes(group = cluster_explicit, 
        color = cluster_explicit),
    data = dat,
    expand = ggplot2::unit(0, "mm"), size = 1
  ) +
  ggplot2::ggtitle("Predicted gradient projected to the first two components")
  
dat <- milk_season_partial_augmented %>% 
  dplyr::rename(gradient = prd, 
                axis1 = dim01,
                axis2 = dim02) %>% 
  dplyr::filter(! cluster_explicit %in% c("Cluster neutral1", "Cluster neurtal2")) 
dat2 <- pls_final_data_augmented %>% 
  dplyr::rename(gradient = gradient_axis1, 
                axis1 = dim01,
                axis2 = dim02)
prd_full_pca <- contour_plt(dat = milk_season_partial_augmented, 
                            axis1 = "dim01",
                            axis2 = "dim02", 
                            var = "prd") +
  ggforce::geom_mark_ellipse(
    ggplot2::aes(group = cluster_explicit, 
        color = cluster_explicit),
    data = dat,
    expand = ggplot2::unit(0, "mm"), size = 1
  ) + 
  ggforce::geom_mark_ellipse(ggplot2::aes(label = "Original exposure"),
                             data = dat2,
                             expand = ggplot2::unit(0, "mm"), size = 1) +
  ggplot2::ggtitle("Predicted gradient projected to the first two components (full milk db)")
     




lgd <-  gradient_pca +
  ggplot2::theme(
    legend.box = "vertical" 
  )    
lgd <- cowplot::get_legend(lgd)

finalize_pls_plt <- function(d, type = "PLS"){
  d +
    ggplot2::xlab(
      label = glue::glue(glue::glue("First {type} dimension"))
    ) +
    ggplot2::ylab(
      label = glue::glue(glue::glue("Second {type} dimension"))
    ) +
    ggplot2::theme(legend.position = "none",
                   legend.title = ggplot2::element_blank())
}

gradient_pls <- gradient_pls %>% finalize_pls_plt()
prd_pls <- prd_pls %>% finalize_pls_plt()
prd_full_pls <- prd_full_pls %>% finalize_pls_plt()
gradient_pca <- gradient_pca %>% finalize_pls_plt(type = "PCA")
prd_pca <- prd_pca %>% finalize_pls_plt(type = "PCA")
prd_full_pca <- prd_full_pca %>% finalize_pls_plt(type = "PCA")

pls_plt <- cowplot::plot_grid(gradient_pls, prd_pls, prd_full_pls, ncol = 3)
pca_plt <-  cowplot::plot_grid(gradient_pca, prd_pca, prd_full_pca, ncol = 3)
plt <- cowplot::plot_grid(pls_plt, pca_plt, ncol = 1)
cowplot::plot_grid( plt, lgd, ncol = 1, rel_heights = c(10, 1.5) )


with(pls_final_data_augmented, cor(prd, gradient_axis1)^2)
with(pls_final_data_augmented, cor(prd, dim01)^2)
with(pls_final_data_augmented, cor(gradient_axis1, dim01)^2)

with(milk_season_partial_augmented, cor(prd, dim01)^2)


# -------------------------------------------------------------------------------------- #
# (Re)labellization ####
# -------------------------------------------------------------------------------------- #

milk_season_partial_augmented %<>% 
  dplyr::mutate(gradient_prd_category = dplyr::case_when(
    prd < quantile(prd, prob = .2) ~ "Highly extensive",
    prd < quantile(prd, prob = .4) ~ "Extensive",
    prd < quantile(prd, prob = .6) ~ "Neutral",
    prd < quantile(prd, prob = .8) ~ "Intensive",
    T ~ "Highly intensive"
  ) %>% 
    factor(levels = c("Highly extensive", "Extensive", "Neutral", 
                      "Intensive", "Highly intensive")), 
  .before = farmerID)

with(milk_season_partial_augmented, table(gradient_prd_category, cluster))

cluster_gradient_tbl <- ( with(milk_season_partial_augmented, 
                               table(gradient_prd_category, cluster)) / 
  nrow(milk_season_partial_augmented)  * 100) %>% 
  round(digits = 2)

colSums(cluster_gradient_tbl)
 
# The same but using the proportions of data distribution in cluster

milk_season_partial_augmented %<>% 
  dplyr::mutate(gradient_prd_category = dplyr::case_when(
    prd < quantile(prd, prob = .0853) ~ "Highly extensive",
    prd < quantile(prd, prob = .0853 + .2117) ~ "Extensive",
    prd < quantile(prd, prob = .0853 + .2117 + .2976) ~ "Neutral",
    prd < quantile(prd, prob = .0853 + .2117 + .2976 + .2102) ~ "Intensive",
    T ~ "Highly intensive"
  ) %>% 
    factor(levels = c("Highly extensive", "Extensive", "Neutral", 
                      "Intensive", "Highly intensive")), 
  .before = farmerID)

with(milk_season_partial_augmented, table(gradient_prd_category, cluster))

cluster_gradient_tbl <- ( with(milk_season_partial_augmented, 
                               table(gradient_prd_category, cluster)) / 
  nrow(milk_season_partial_augmented)  * 100) %>% 
  round(digits = 2)

cluster_gradient_tbl <- ( with(milk_season_partial_augmented, 
                               table(gradient_prd_category, cluster_explicit)) / 
  nrow(milk_season_partial_augmented)  * 100) %>% 
  round(digits = 2)

colSums(cluster_gradient_tbl)



# The same but with reference value 
with(pls_final_data_augmented, table(gradient_cat, cluster))

cluster_gradient_tbl <- ( with(pls_final_data_augmented, 
                               table(gradient_cat, cluster)) / 
  nrow(pls_final_data_augmented)  * 100) %>% 
  round(digits = 2)

cluster_gradient_tbl <- ( with(pls_final_data_augmented, 
                               table(gradient_cat, cluster_explicit)) / 
  nrow(pls_final_data_augmented)  * 100) %>% 
  round(digits = 2)

colSums(cluster_gradient_tbl)
rowSums(cluster_gradient_tbl)



milk_season_partial_augmented %>% 
  ggplot2::ggplot(ggplot2::aes(y = prd, x = dim01)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, size = 1, color = "red")

milk_season_partial_augmented %>% 
  ggplot2::ggplot(ggplot2::aes(y = prd, x = dim01)) +
  ggplot2::geom_point()

