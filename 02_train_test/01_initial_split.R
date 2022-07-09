library(magrittr)


# -------------------------------------------------------------------------------------- #
# Load data ####
# -------------------------------------------------------------------------------------- #

data("working_dat")
data("working_dat_quarter")


# -------------------------------------------------------------------------------------- #
# Helper functions ####
# -------------------------------------------------------------------------------------- #

get_train_quantile_base <- function(q = .1, 
                                    train = T){ 
  qm <- quantile(working_dat$gradient_axis1, q)
  qmid <- quantile(working_dat$gradient_axis1, .5+c(-1, 1)*q/2)
  qM <- quantile(working_dat$gradient_axis1, 1-q) 
  
  if (train)
    dat <- train_full
  else
    dat <- test_full
  
  dat %>% 
    dplyr::filter(
      gradient_axis1 < qm |
        gradient_axis1 > min(qmid) & gradient_axis1 < max(qmid) |
        gradient_axis1 > qM
    ) 
}

# -------------------------------------------------------------------------------------- #
# Create train / test group ####
# -------------------------------------------------------------------------------------- #

# By farm id
# n_farm <- length(unique(working_dat$farmerID))
prop <- .20

# set.seed(42)
# test_id <- sample(seq_len(n_farm), n_farm * prop)
# train_id <- setdiff(seq_len(n_farm), test_id)
# 
# working_dat_byfarm_lst <- working_dat %>% 
#   dplyr::group_by(farmerID) %>% 
#   dplyr::group_split()
# 
# length(working_dat_byfarm_lst)


mean_grad_by_farm <- working_dat %>% 
  dplyr::group_by(farmerID) %>% 
  dplyr::summarise(mean_grad = mean(gradient_axis1)) %>% 
  dplyr::mutate(
    ranking = dplyr::case_when(
      mean_grad <= quantile(mean_grad, prob = .1) ~ "q01",
      mean_grad <= quantile(mean_grad, prob = .2) ~ "q02",
      mean_grad <= quantile(mean_grad, prob = .3) ~ "q03",
      mean_grad <= quantile(mean_grad, prob = .4) ~ "q04",
      mean_grad <= quantile(mean_grad, prob = .5) ~ "q05",
      mean_grad <= quantile(mean_grad, prob = .6) ~ "q06",
      mean_grad <= quantile(mean_grad, prob = .7) ~ "q07",
      mean_grad <= quantile(mean_grad, prob = .8) ~ "q08",
      mean_grad <= quantile(mean_grad, prob = .9) ~ "q09",
      T ~ "q10",
    )
  )

farm_by_ranking <- mean_grad_by_farm %>% 
  dplyr::group_by(ranking) %>% 
  dplyr::group_split()

set.seed(1010)
test_id <- farm_by_ranking %>% 
  purrr::map(~sample(.x$farmerID, 
                     size = floor(nrow(.x) * prop) + sample(c(-1, +1), size = 1))) %>% 
  purrr::reduce(c)

train_id <- setdiff(working_dat$farmerID, test_id)

# -------------------------------------------------------------------------------------- #
# Create final datasets ####
# -------------------------------------------------------------------------------------- #

# train data.frame (full)
#train_full <- working_dat_byfarm_lst[train_id] %>% 
#purrr::reduce(dplyr::bind_rows)
train_full <- working_dat %>% 
  dplyr::filter(farmerID %in% train_id)
  
# test data.frame (full)
test_full <- working_dat %>% 
  dplyr::filter(farmerID %in% test_id)
# train / test quantile 20%
train_q20 <- get_train_quantile_base(q = .2)
test_q20 <- get_train_quantile_base(q = .2, train = F) 
# train / test quantile 10%
train_q10 <- get_train_quantile_base(q = .1)
test_q10 <- get_train_quantile_base(q = .1, train = F) 
# train / test quantile 5%
train_q05 <- get_train_quantile_base(q = .05) 
test_q05 <- get_train_quantile_base(q = .05, train = F)

train_quarter_full <- working_dat_quarter %>% 
  dplyr::filter(farmerID %in% train_id)
test_quarter_full <- working_dat_quarter %>% 
  dplyr::filter(farmerID %in% test_id)


# -------------------------------------------------------------------------------------- #
# Print some data ####
# -------------------------------------------------------------------------------------- #

dplyr::bind_rows(
  test_full %>% dplyr::mutate(type = "test"),
  train_full %>% dplyr::mutate(type = "train")
) %>% 
  ggplot2::ggplot(ggplot2::aes(x = gradient_axis1, y = type, fill = type)) +
  ggplot2::ggtitle("Full train / test") +
  ggplot2::geom_violin()


dplyr::bind_rows(
  test_full %>% dplyr::mutate(type = "test"),
  train_q20 %>% dplyr::mutate(type = "train")
) %>% 
  ggplot2::ggplot(ggplot2::aes(x = gradient_axis1, y = type, fill = type)) +
  ggplot2::ggtitle("train q20 / test") +
  ggplot2::geom_violin()

# -------------------------------------------------------------------------------------- #
# Save ####
# -------------------------------------------------------------------------------------- #

save(train_full, file = "data/train_full.rda", compress = "xz")
save(test_full, file = "data/test_full.rda", compress = "xz") 
save(train_quarter_full, file = "data/train_quarter_full.rda", compress = "xz")
save(test_quarter_full, file = "data/test_quarter_full.rda", compress = "xz") 

save(train_q20, file = "data/train_q20.rda", compress = "xz")
save(test_q20, file = "data/test_q20.rda", compress = "xz") 
save(train_q10, file = "data/train_q10.rda", compress = "xz")
save(test_q10, file = "data/test_q10.rda", compress = "xz") 
save(train_q05, file = "data/train_q05.rda", compress = "xz")
save(test_q05, file = "data/test_q05.rda", compress = "xz") 

