library(magrittr)

data("technico_milk")
source("globals/global_variables.R")

technico_milk_X <- technico_milk %>% 
  dplyr::select(dplyr::all_of(milk_col_names)) %>% 
  dplyr::select(-c("farmerID", "year"))
technico_milk_y <- technico_milk$gradient_axis1

tictoc::tic()
rfcv_output <- randomForest::rfcv(
  technico_milk_X,  technico_milk_y, 
  ntree=1000  
)
tictoc::toc()

with(rfcv_output, plot(n.var, error.cv, log = "x", type = "o", lwd = 2))

tictoc::tic()
rf <- randomForest::randomForest(
  technico_milk_X,  technico_milk_y, 
  ntree=10000, importance = T
)
tictoc::toc()

importance <- randomForest::importance(rf)
randomForest::varImpPlot(rf)

dat <- technico_milk %>% 
  dplyr::select(milk_col_names) %>% 
  dplyr::select(-c("farmerID", "year"))

X_cor <- cor(dat)

upper<-X_cor
upper[upper.tri(X_cor)]<-""
mat<-as.data.frame(upper)
mat[1:5, 1:5]

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
  "pC18.1__tot", "pbotter_yield", "pcheese_yield_curd", "pRFI1"
)


# Remove and rerun RandomForest

technico_milk_X <- technico_milk %>% 
  dplyr::select(dplyr::all_of(milk_col_names)) %>% 
  dplyr::select(-dplyr::all_of(variables_to_remove)) %>% 
  dplyr::select(-c("farmerID", "year"))
technico_milk_y <- technico_milk$gradient_axis1

tictoc::tic()
rfcv_output <- randomForest::rfcv(
  technico_milk_X,  technico_milk_y, 
  ntree=1000  
)
tictoc::toc()

with(rfcv_output, plot(n.var, error.cv, log = "x", type = "o", lwd = 2))

tictoc::tic()
rf <- randomForest::randomForest(
  technico_milk_X,  technico_milk_y, 
  ntree=10000, importance = T
)
tictoc::toc()

importance <- randomForest::importance(rf)
randomForest::varImpPlot(rf)

importance[, 1] %>% hist
no_important_variable <- importance[, 1][importance[, 1] < 20]

plot(technico_milk_X$plactofer_no_EMR, technico_milk_y)
cor(technico_milk_X$plactofer_no_EMR, technico_milk_y)
plot(technico_milk_X$ppH, technico_milk_y)
cor(technico_milk_X$ppH, technico_milk_y)
plot(technico_milk_X$pBLG, technico_milk_y)
cor(technico_milk_X$pBLG, technico_milk_y)
plot(technico_milk_X$pBOHB_milk, technico_milk_y)
cor(technico_milk_X$pBOHB_milk, technico_milk_y)


plot(technico_milk_X$pC4, technico_milk_y)
cor(technico_milk_X$pC4, technico_milk_y)
plot(technico_milk_X$pC18.2_c9_c12, technico_milk_y)
cor(technico_milk_X$pC18.2_c9_c12, technico_milk_y)


predictor_to_keep <- setdiff(milk_col_names, c(no_important_variable, variables_to_remove))

save(predictor_to_keep, file = "data/predictor_to_keep.rda")