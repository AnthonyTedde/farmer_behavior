library(magrittr)

data("pls_final_data_augmented")
data("milk_season_partial_augmented")
data("dendr")
data("clst")
data("rename_clst_fct")

k_threshold <- milk_season_partial_augmented$cluster_explicit %>% 
  unique %>% length()
 
# Construct dendrogram for plotting 
cut    <- milk_season_partial_augmented$cluster_explicit 
cut.df <- data.frame(label = seq_along(cut), cluster = cut)
dendr[["labels"]] <- merge(dendr[["labels"]], cut.df, by="label")

# Segment dendrogram
seg <- ggdendro::segment(dendr) %>% 
  tibble::as_tibble() %>%  
  tibble::rowid_to_column() %>%
  dplyr::arrange(desc(y)) %>%
  dplyr::slice_head(n = (k_threshold - 1) * 4) %>%
  dplyr::arrange(rowid) %>% dplyr::select(-rowid)
  # dplyr::filter(y > 2500)
tozero <- seg$yend %>% unique %>% sort %>% head(k_threshold)
seg %<>%  dplyr::mutate( yend = ifelse(yend %in% tozero, 0, yend) )

# Lavel dendrogram
lbl <- ggdendro::label(dendr) %>% 
  tibble::as_tibble() %>% 
  dplyr::group_by(cluster) %>% 
  dplyr::summarise(
    x = min(x), 
    y_max = max(y),
    y = 0, 
    n = dplyr::n()
  )

  
# Update labels
lbl %<>% 
  dplyr::arrange(x) %>% 
  tibble::rowid_to_column() %>% 
  dplyr::select(-x) %>%  
  dplyr::inner_join(
    seg %>%  dplyr::filter(yend == 0) %>% 
      tibble::rowid_to_column() %>% 
      dplyr::select(rowid, x) 
  ) %>% dplyr::select(-rowid) %>% 
  dplyr::arrange(cluster)

# milk_season_partial_augmented$cluster %>% table

ggplot2::ggplot() + 
  ggplot2::geom_segment(
    # data = ggdendro::segment(dendr),
    data = seg,
    ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
  ) + 
  ggplot2::geom_label(data = lbl,
                     ggplot2::aes(x, 
                                  y, 
                                  color = cluster, 
                                  label = n),
            size = k_threshold)
  # ggplot2::coord_flip() +
  # ggplot2::scale_y_reverse(expand = c(0.2, 0)) +
  # ggplot2::theme(
  #   axis.line.y = ggplot2::element_blank(),
  #   axis.ticks.y = ggplot2::element_blank(),
  #   axis.text.y = ggplot2::element_blank(),
  #   axis.title.y = ggplot2::element_blank(),
  #   panel.background = ggplot2::element_rect(fill="white"),
  #   panel.grid = ggplot2::element_blank()
  # )


pls_final_data_augmented %>% 
  ggplot2::ggplot(ggplot2::aes(
    x = gradient_axis1, 
    y = forcats::fct_reorder(cluster_explicit, gradient_axis1, .desc = T)
  )) +
  ggplot2::geom_boxplot()
milk_season_partial_augmented %>% 
  ggplot2::ggplot(ggplot2::aes(
    x = prd, 
    y = forcats::fct_reorder(cluster_explicit, prd, .desc = T)
  )) +
  ggplot2::geom_boxplot()

d <- dplyr::bind_rows(
  pls_final_data_augmented %>% 
    dplyr::select(gradient_axis1, cluster_explicit) %>% 
    tibble::add_column(data = "Gradient observed technico-milk") %>% 
    dplyr::rename(gradient = gradient_axis1),
  pls_final_data_augmented %>% 
    dplyr::select(prd, cluster_explicit) %>% 
    tibble::add_column(data = "Gradient predicted technico-milk") %>% 
    dplyr::rename(gradient = prd),
  milk_season_partial_augmented %>% 
    dplyr::select(prd, cluster_explicit) %>% 
    tibble::add_column(data = "Gradient predicted milk") %>% 
    dplyr::rename(gradient = prd),
) %>% 
  dplyr::mutate(
    data = factor(data, 
                  levels = c("Gradient observed technico-milk",
                             "Gradient predicted technico-milk",
                             "Gradient predicted milk"))
  ) 

d %>% 
  ggplot2::ggplot(ggplot2::aes(
    x = gradient, 
    y = forcats::fct_reorder(cluster_explicit, gradient, .desc = F),
    color = data
  )) +
  ggplot2::xlab(label = "Intensivity gradient") +
  ggplot2::ylab(label = "Cluster interpretation") +
  ggplot2::geom_boxplot(outlier.shape = NA) +
  ggplot2::coord_cartesian(ylim = quantile(d$gradient, probs = c(.1, .9))) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank()
    )
  
  


milk_season_partial_augmented %>% 
  # dplyr::filter(! cluster_explicit %in% c("Cluster neutral1", "Cluster neurtal2")) %>% 
  dplyr::filter(! cluster_explicit %in% c("Cluster neutral1")) %>% 
  ggplot2::ggplot(ggplot2::aes(x = dim01, y = dim02, color = cluster_explicit)) +
  ggforce::geom_mark_ellipse(ggplot2::aes(fill = cluster_explicit, 
                                 label = cluster_explicit),
                             expand = ggplot2::unit(0, "mm")
                             ) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank()
    )

pls_final_data_augmented %>% 
  # dplyr::filter(! cluster_explicit %in% c("Cluster neutral1", "Cluster neurtal2")) %>% 
  dplyr::filter(! cluster_explicit %in% c("Cluster neutral1")) %>% 
  dplyr::filter(! gradient_cat %in% c("Neutral")) %>% 
  ggplot2::ggplot(ggplot2::aes(x = dim01, y = dim02, group = cluster_explicit)) +
  ggforce::geom_mark_ellipse(ggplot2::aes(label = cluster_explicit), 
                             expand = ggplot2::unit(0, "mm")
                             ) +
  ggplot2::geom_point(ggplot2::aes(color = gradient_cat), size = 3) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank()
    )


