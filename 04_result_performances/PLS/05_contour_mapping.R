library(magrittr)
library(plsmod)
library(workflows)
library(mixOmics)
library(tidymodels)

data("pls_final_mdl")
data("pls_final_data")


pls_final_mdl %>% 
  broom::tidy() %>% 
  dplyr::filter(term != "Y") %>%
  dplyr::filter(component <= 2) %>% 
  dplyr::mutate(contrib = ifelse(value < 0, "negative", "positive")) %>% 
  dplyr::group_by(component, contrib) %>%
  dplyr::slice_max(abs(value), n = 5) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(value, forcats::fct_reorder(term, value), 
                               fill = factor(component))) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::facet_wrap(~component, scales = "free_y") +
  ggplot2::labs(y = NULL) 

pls_raw_prd <- predict(pls_final_mdl, new_data = pls_final_data, type = "raw")



projection <- pls_raw_prd$variates %>% tibble::as_tibble()


 
## <<< TODO ON ALL DATA
pls_final_data_augmented <- pls_final_data %>% 
  dplyr::mutate(prd = predict(pls_final_mdl, new_data = pls_final_data) %>% 
                  dplyr::pull()) %>% 
  dplyr::bind_cols(projection)


# Explained variance 
mixOmics::explained_variance(pls_final_mdl$fit$fit$fit$X, 
                             pls_final_mdl$fit$fit$fit$variates$X, 
                             ncomp =8)

# Explained variance of y from dim.x:
yvariance <- mixOmics::explained_variance(pls_final_mdl$fit$fit$fit$Y, 
                             pls_final_mdl$fit$fit$fit$variates$X, 
                             ncomp =8)
# Verify the explained variance here: 
lm(gradient_axis1 ~ dim1, data = pls_final_data_augmented) %>% 
  summary

# ------------------------------
# Create interpolated map
# ------------------------------


full_mapping <- purrr::cross2( 
  list( 
    list(axis1 = "dim1", axis2 = "dim2", error = F)
    # list(axis1 = "dim1", axis2 = "dim2", error = T)
    # list(axis1 = "dim1", axis2 = "dim3"),
    # list(axis1 = "dim3", axis2 = "dim4", error = F)
  ),
  list(
    list(var = "gradient_axis1", hname = "Genuine gradient"),
    list(var = "prd", hname = "Predicted gradient")
  )
) %>% 
  purrr::map(purrr::flatten) %>%  
  append(list(list(
    axis1 = "dim1", axis2 = "dim2", error = T,
    # axis1 = "dim3", axis2 = "dim4", error = T,
    var = "gradient_axis1", var2 = "prd", hname = "Absolute error"
  ))) %>% 
  purrr::map(.f = function(grad){
    x <- pls_final_data_augmented[, grad$axis1, drop = T]
    y <- pls_final_data_augmented[, grad$axis2, drop = T]
      z <- pls_final_data_augmented[, grad$var, drop = T]
    if (grad$error){
      z <- abs(z - pls_final_data_augmented[, grad$var2, drop = T])
    }
      grid_grad <-  interp::interp( x, y, z, nx = 500, ny = 500)
      
      griddf_grad <- tibble::tibble(
        x = rep(grid_grad$x, nrow(grid_grad$z)),
        y = rep(grid_grad$y, each = ncol(grid_grad$z)),
        z = as.numeric(grid_grad$z)
      ) %>% 
        tidyr::drop_na() %>% 
        setNames(nm = c("axis1", "axis2", "gradient")) %>% 
        tibble::add_column(
          type = grad$hname,
          axis1_nam = grad$axis1,
          axis2_nam = grad$axis2
        ) 
  }) %>% 
  dplyr::bind_rows()

gradient_mapping <- full_mapping %>% 
  dplyr::filter(type != "Absolute error")
error_mapping <- full_mapping %>% 
  dplyr::filter(type == "Absolute error") %>% 
  dplyr::rename(error = gradient)

pal <-  wesanderson::wes_palette(name = "Zissou1", n = 5, type = "continuous")
# pal = ggsci::scale_fill_material("indigo")
pal <- ggsci::pal_simpsons()(5)


pls_final_data_augmented %<>% 
  dplyr::mutate(gradient_cat = dplyr::case_when(
    gradient_axis1 <= quantile(gradient_axis1, probs = .2) ~ "Highly extensive",
    gradient_axis1 <= quantile(gradient_axis1, probs = .4) ~ "Extensive",
    gradient_axis1 <= quantile(gradient_axis1, probs = .6) ~ "Neutral",
    gradient_axis1 <= quantile(gradient_axis1, probs = .8) ~ "Intensive",
    gradient_axis1 <= quantile(gradient_axis1, probs = 1) ~ "Highly intensive"
  ) %>% factor(
    levels = c("Highly extensive", "Extensive", "Neutral", "Intensive", "Highly intensive")
  )) %>% 
  dplyr::mutate(resid = abs(gradient_axis1 - prd)) %>% 
  dplyr::mutate(worse = ifelse(resid > quantile(resid, probs = .95), T, F))

worse_dt <- pls_final_data_augmented %>% 
  dplyr::filter(worse) %>% 
  dplyr::select(farmerID, year, resid, dim1, dim2, gradient_axis1, gradient_cat) %>% 
  dplyr::rename(axis1 = dim1, axis2 = dim2) 
  # dplyr::pull(gradient_cat) %>% table
  # dplyr::pull(farmerID) %>% unique

gradient_mapping %>% 
  ggplot2::ggplot(ggplot2::aes(x = axis1, y = axis2, z = gradient)) +
  ggplot2::geom_contour_filled(
    breaks = quantile(pls_final_data_augmented$gradient_axis1,
                      probs = seq(0, 1, 0.2))
  ) +
  ggplot2::scale_fill_manual(
    values = pal,
    labels = c("Highly extensive",
               "Extensive",
               "Neutral",
               "Intensive",
               "Highly intensive")
  ) +
  # ggplot2::geom_point(
  #   mapping = ggplot2::aes(z = NULL), 
  #   data = worse_dt,
  #   alpha = .5
  # ) +
  ggplot2::xlab(
    label = glue::glue("First PLS dimension ({round(yvariance[1] * 100, digits = 2)}%)")
  ) +
  ggplot2::ylab(
    label = glue::glue("Second PLS dimension ({round(yvariance[2] * 100, digits = 2)}%)")
  ) +
  ggplot2::facet_wrap(type ~ ., ncol = 2) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = ggplot2::element_blank())



# -------------------------------------------------------------------------------------- #
# Analysis of error in distribution
# -------------------------------------------------------------------------------------- #

pls_final_mdl %>% 
  broom::tidy() %>% 
  dplyr::filter(term != "Y") %>%
  dplyr::filter(component <= 2) %>% 
  dplyr::mutate(contrib = ifelse(value < 0, "negative", "positive")) %>% 
  # dplyr::group_by(component, contrib) %>%
  dplyr::group_by(component) %>%
  dplyr::slice_max(abs(value), n = 5) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(value, forcats::fct_reorder(term, value), 
                               fill = factor(component))) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::facet_wrap(~component, scales = "free_y") +
  ggplot2::labs(y = NULL) 



# Les coefficients sont comparables grâce à la normalisation des X:
terms <- pls_final_mdl %>% 
  broom::tidy() %>% 
  dplyr::filter(term != "Y") %>%
  dplyr::filter(component <= 1) %>% 
  dplyr::mutate(contrib = ifelse(value < 0, "negative", "positive")) %>% 
  dplyr::group_by(component) %>%
  # dplyr::group_by(component, contrib) %>%
  dplyr::slice_max(abs(value), n = 10) %>%
  dplyr::ungroup() %>% 
  dplyr::pull(term)

a <- mixOmics::vip(pls_final_mdl$fit$fit$fit) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble()

a %<>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("comp")) %>% 
  dplyr::group_by(rowname) %>% 
  dplyr::summarise(vip_mean = mean(value)) %>% 
  dplyr::arrange(desc(vip_mean))
  
a[a$vip_mean < 0.5, ]
terms <- a$rowname[1:9] 

pls_final_data_normalized <- pls_final_data_augmented %>% 
  dplyr::select(farmerID, year, worse, gradient_cat, gradient_axis1, dplyr::all_of(terms)) %>% 
  dplyr::mutate(across(dplyr::all_of(terms), ~{bestNormalize::orderNorm(.x)$x.t}))
    
d <- dplyr::bind_rows(
  pls_final_data_normalized %>% 
    dplyr::filter(worse) %>% 
    dplyr::mutate(outlier_type = "Outliers"),
  pls_final_data_normalized %>% 
    dplyr::mutate(outlier_type = "All")
) %>% 
  dplyr::filter(gradient_cat != "Neutral") %>% 
  # dplyr::mutate(across(dplyr::all_of(terms), ~{log(.x)})) %>%
  tidyr::pivot_longer(dplyr::all_of(terms)) %>% 
  dplyr::mutate(name = factor(name, levels = terms))


var <- list(
  "pC10_Autumn" =  "pC10 (autumn)",
  "pC18.2_c9_c12_Autumn" =  "pC18:2 cis9 cis12 (autumn)",
  "pC12_Autumn" =  "pC12 (autumn)",
  "pglucose_blood_Autumn" =  "pglucose blood (autumn)",
  "pC10_Winter" =  "pC10 (winter)",
  "pglucose_blood_Winter" =  "pglucose blood (winter)",
  "pC12_Winter" =  "pC12 (winter)",
  "pK20_CRM_Winter" =  "pK20 CRM (winter)",
  "pLBA_Winter" =  "pLBA (Winter)"
)
var_labeller <- function(variable,value){
  return(var[value])
}

d %>% 
  ggplot2::ggplot(ggplot2::aes(y = gradient_cat, x = value, fill = outlier_type)) +
  # ggplot2::ggplot(ggplot2::aes(y = gradient_cat, x = value, fill = outlier_type)) +
  ggplot2::geom_boxplot(coef = 1, outlier.shape = NA) +
  ggplot2::scale_x_continuous(breaks =  0) +
  ggplot2::coord_cartesian(xlim = c(-2, 2))  +
  ggplot2::facet_wrap(name ~., labeller = var_labeller) +
  ggplot2::theme_minimal() +
  ggsci::scale_fill_simpsons() +
  ggplot2::xlab(
    label = "Normalized value of the predictors"
  ) +
  ggplot2::ylab(
    label = "Quantile based level of the intensity gradient"
  ) +
  # ggsci::scale_color_simpsons() +
  ggplot2::theme(
    # legend.position = "bottom",
    legend.title = ggplot2::element_blank()
  )

pls_final_data_augmented %>% 
  dplyr::mutate(outlier_type = ifelse(worse, "Outliers", "Inliners")) %>% 
  ggplot2::ggplot(ggplot2::aes(x = prd, y = gradient_axis1, color = outlier_type)) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggsci::scale_fill_simpsons() +
  ggsci::scale_color_simpsons() +
  ggplot2::theme(
    # legend.position = "bottom",
    legend.title = ggplot2::element_blank()
  )
  
  
  




error_mapping %>% 
  ggplot2::ggplot(ggplot2::aes(x = axis1, y = axis2, z = error)) +
  ggplot2::geom_contour_filled(
    # breaks = quantile(error_mapping$error,
    #                   probs = seq(0, 1, 0.333333))
    breaks = c(0, 1, 2, max(error_mapping$error))
  ) +
  # ggplot2::scale_fill_manual(
  #   values = pal,
  #   labels = c("High error",
  #              # "Extensive",
  #              "Neutral",
  #              # "Intensive",
  #              "Highly intensive")
  # ) +
  ggplot2::xlab(
    label = glue::glue("First PLS dimension ({round(yvariance[1] * 100, digits = 2)}%)")
  ) +
  ggplot2::ylab(
    label = glue::glue("Second PLS dimension ({round(yvariance[2] * 100, digits = 2)}%)")
  ) +
  ggplot2::facet_grid(type ~ .) +
  # ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = ggplot2::element_blank())


# >>>


# -------------------------------------------------------------------------------------- #
# Worse points ####
# -------------------------------------------------------------------------------------- #

pls_final_data_augmented %>% 
  ggplot2::ggplot(ggplot2::aes(y = gradient_axis1, x = prd)) +
  ggplot2::geom_point()




# << What on the whole dataset

data("working_season_partial_milk")


milk_raw_prd <- predict(pls_final_mdl, new_data = working_season_partial_milk, 
                        type = "raw")  

projection <- milk_raw_prd$variates %>% tibble::as_tibble()
 
 
## <<< TODO ON ALL DATA
working_season_partial_milk_augmented <- working_season_partial_milk %>% 
  dplyr::mutate(prd = predict(pls_final_mdl, new_data = working_season_partial_milk) %>% 
                  dplyr::pull()) %>% 
  dplyr::bind_cols(projection)
  

full_mapping_milk <- purrr::cross2( 
  list( 
    list(axis1 = "dim1", axis2 = "dim2")
    # list(axis1 = "dim1", axis2 = "dim3"),
    # list(axis1 = "dim3", axis2 = "dim4")
  ),
  list(
    # list(var = "gradient_axis1", hname = "Genuine gradient")
    list(var = "prd", hname = "Full predicted gradient")
  )
) %>% 
  purrr::map(purrr::flatten) %>%  
  purrr::map(.f = function(grad){
    
    grid_grad <-  interp::interp(
      working_season_partial_milk_augmented[, grad$axis1, drop = T], 
      working_season_partial_milk_augmented[, grad$axis2, drop = T], 
      working_season_partial_milk_augmented[, grad$var, drop = T],
      nx = 500, ny = 500)
    
    griddf_grad <- tibble::tibble(
      x = rep(grid_grad$x, nrow(grid_grad$z)),
      y = rep(grid_grad$y, each = ncol(grid_grad$z)),
      z = as.numeric(grid_grad$z)
    ) %>% 
      tidyr::drop_na() %>% 
      setNames(nm = c("axis1", "axis2", "gradient")) %>% 
      tibble::add_column(
        type = grad$hname,
        axis1_nam = grad$axis1,
        axis2_nam = grad$axis2
        )
  }) %>% 
  dplyr::bind_rows()


pal <- ggsci::pal_simpsons()(5)


full_mapping %>% 
  dplyr::bind_rows(full_mapping_milk) %>% 
  ggplot2::ggplot(ggplot2::aes(x = axis1, y = axis2, z = gradient)) +
  ggplot2::geom_contour_filled(
    breaks = quantile(pls_final_data_augmented$gradient_axis1,
                      probs = seq(0, 1, 0.2))
  ) +
  ggplot2::scale_fill_manual(
    values = pal,
    labels = c("Highly extensive",
               "Extensive",
               "Neutral",
               "Intensive",
               "Highly intensive")
  ) +
  ggplot2::xlab(
    label = glue::glue("First PLS dimension ({round(yvariance[1] * 100, digits = 2)}%)")
  ) +
  ggplot2::ylab(
    label = glue::glue("Second PLS dimension ({round(yvariance[2] * 100, digits = 2)}%)")
  ) +
  # ggplot2::facet_grid(type ~ .) +
  ggplot2::facet_wrap("type", ncol = 1) +
  # ggplot2::theme_minimal() +
  # ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = ggplot2::element_blank())




# >> 