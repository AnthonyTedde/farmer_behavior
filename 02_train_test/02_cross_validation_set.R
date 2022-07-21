library(magrittr)

data("train_year_full")
data("train_season_full")
data("train_winsum_full")
data("train_season_partial")
data("train_winsum_partial")

data("train_year_corrected_full")
data("train_season_corrected_full")
data("train_winsum_corrected_full")
data("train_season_corrected_partial")
data("train_winsum_corrected_partial")


set.seed(1010) 
train_year_full_cv <- rsample::group_vfold_cv(
  train_year_full, 
  group = farmerID,
  v = 10
)
train_season_full_cv <- rsample::group_vfold_cv(
  train_season_full, 
  group = farmerID,
  v = 10
)
train_winsum_full_cv <- rsample::group_vfold_cv(
  train_winsum_full, 
  group = farmerID,
  v = 10
)
train_season_partial_cv <- rsample::group_vfold_cv(
  train_season_partial, 
  group = farmerID,
  v = 10
)
train_winsum_partial_cv <- rsample::group_vfold_cv(
  train_winsum_partial, 
  group = farmerID,
  v = 10
)

train_year_corrected_full_cv <- rsample::group_vfold_cv(
  train_year_corrected_full, 
  group = farmerID,
  v = 10
)
train_season_corrected_full_cv <- rsample::group_vfold_cv(
  train_season_corrected_full, 
  group = farmerID,
  v = 10
)
train_winsum_corrected_full_cv <- rsample::group_vfold_cv(
  train_winsum_corrected_full, 
  group = farmerID,
  v = 10
)
train_season_corrected_partial_cv <- rsample::group_vfold_cv(
  train_season_corrected_partial, 
  group = farmerID,
  v = 10
)
train_winsum_corrected_partial_cv <- rsample::group_vfold_cv(
  train_winsum_corrected_partial, 
  group = farmerID,
  v = 10
)

save(train_year_full_cv, file = "data/train_year_full_cv.rda", compress = "xz")
save(train_season_full_cv, file = "data/train_season_full_cv.rda", compress = "xz")
save(train_winsum_full_cv, file = "data/train_winsum_full_cv.rda", compress = "xz")
save(train_season_partial_cv, file = "data/train_season_partial_cv.rda", compress = "xz")
save(train_winsum_partial_cv, file = "data/train_winsum_partial_cv.rda", compress = "xz")

save(train_year_corrected_full_cv, 
     file = "data/train_year_corrected_full_cv.rda", 
     compress = "xz")
save(train_season_corrected_full_cv, 
     file = "data/train_season_corrected_full_cv.rda", 
     compress = "xz")
save(train_winsum_corrected_full_cv, 
     file = "data/train_winsum_corrected_full_cv.rda", 
     compress = "xz")
save(train_season_corrected_partial_cv, 
     file = "data/train_season_corrected_partial_cv.rda", 
     compress = "xz")
save(train_winsum_corrected_partial_cv, 
     file = "data/train_winsum_corrected_partial_cv.rda", 
     compress = "xz")