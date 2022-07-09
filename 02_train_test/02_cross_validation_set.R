library(magrittr)

data("train_full")
data("train_quarter_full")
# data("train_q05")
# data("train_q10")
# data("train_q20")

set.seed(1010) 
train_full_cv <- rsample::group_vfold_cv(
  train_full, 
  group = farmerID,
  v = 10
)
train_quarter_full_cv <- rsample::group_vfold_cv(
  train_quarter_full, 
  group = farmerID,
  v = 10
)

# train_q05_cv <- rsample::group_vfold_cv(
#   train_q05, 
#   group = farmerID,
#   v = 10
# )
# train_q10_cv <- rsample::group_vfold_cv(
#   train_q10, 
#   group = farmerID,
#   v = 10
# )
# train_q20_cv <- rsample::group_vfold_cv(
#   train_q20, 
#   group = farmerID,
#   v = 10
# )
 
save(train_full_cv, file = "data/train_full_cv.rda", compress = "xz")
save(train_quarter_full_cv, file = "data/train_quarter_full_cv.rda", compress = "xz")
# save(train_q05_cv, file = "data/train_q05_cv.rda")
# save(train_q10_cv, file = "data/train_q10_cv.rda")
# save(train_q20_cv, file = "data/train_q20_cv.rda")