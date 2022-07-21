library(magrittr)
library(splines)

data("train_full")
data("test_full")
data("train_quarter_full")
data("test_quarter_full")

train_quarter_full %<>% 
  tidyr::drop_na()
test_quarter_full %<>% 
  tidyr::drop_na()

# tmp_train_full <- train_full
# tmp_train_quarter <- train_quarter_full
# tmp_test_full <- test_full
# tmp_test_quarter <- test_quarter_full

# train_full %<>% 
#   dplyr::filter(!year %in% c(2019, 2020))
# train_quarter_full %<>% 
#   dplyr::filter(!year %in% c(2019, 2020))
# test_full %<>% 
#   dplyr::filter(!year %in% c(2019, 2020))
# test_quarter_full %<>% 
#   dplyr::filter(!year %in% c(2019, 2020))

# tmp_train_full -> train_full
# tmp_train_quarter -> train_quarter_full
# tmp_test_full -> test_full
# tmp_test_quarter -> test_quarter_full



form <- setdiff(names(train_full), c("gradient_axis1", "farmerID", "year")) %>% 
  paste0("ns(", ., ", df = 2)") %>%
  paste(collapse = " + ") %>% 
  paste("gradient_axis1", ., sep = " ~ ") %>% 
  formula()

# # Interactin 1
# interaction_term <- setdiff(names(train_quarter_full), 
#                         c("gradient_axis1", "farmerID", "year")) %>% 
#   grep(pattern = "Summer|Winter$", value = T) %>% 
#   sub(pattern = "\\_Summer|\\_Winter$", replacement = "") %>% 
#   unique %>% 
#   paste0(., "_", "Summer", " * ", ., "_", "Winter")
# 
# simple_term <- setdiff(names(train_quarter_full), 
#                         c("gradient_axis1", "farmerID", "year")) %>% 
#   grepl(pattern = "Summer|Winter$") %>% 
#   `!` %>% 
#   `[`(setdiff(names(train_quarter_full), c("gradient_axis1", "farmerID", "year")), .)
# 
# form_quarter <- c(simple_term, interaction_term) %>% 
#   # paste0("ns(", ., ", df = 2)") %>%
#   paste(collapse = " + ") %>% 
#   paste("gradient_axis1", ., sep = " ~ ") %>% 
#   formula()

# Interactin 2
interaction_term <- setdiff(names(train_quarter_full), 
                        c("gradient_axis1", "farmerID", "year")) %>% 
  grep(pattern = "Summer|Winter$", value = T) %>% 
  sub(pattern = "\\_Summer|\\_Winter$", replacement = "") %>% 
  unique %>% 
  paste0(., "_", "Summer", " : ", ., "_", "Winter")

simple_term <- setdiff(names(train_quarter_full), 
                        c("gradient_axis1", "farmerID", "year")) %>% 
  paste0("ns(", ., ", df = 2)")
  # grepl(pattern = "Summer|Winter$") %>% 
  # `!` %>% 
  # `[`(setdiff(names(train_quarter_full), c("gradient_axis1", "farmerID", "year")), .)

form_quarter <- c(simple_term, interaction_term) %>% 
  # paste0("ns(", ., ", df = 2)") %>%
  paste(collapse = " + ") %>% 
  paste("gradient_axis1", ., sep = " ~ ") %>% 
  formula()
   
# # No interaction
# form_quarter <- setdiff(names(train_quarter_full),
#                         c("gradient_axis1", "farmerID", "year")) %>%
#   paste0("ns(", ., ", df = 2)") %>%
#   paste(collapse = " + ") %>%
#   paste("gradient_axis1", ., sep = " ~ ") %>%
#   formula()

ncp_year <- 15
ncp_quarter <- 10
m_year <- pls::mvr(form, ncomp = ncp_year, data = train_full, scale = T,
                   method = pls::pls.options()$plsralg)
m_quarter <- pls::mvr(form_quarter, ncomp = ncp_quarter, data = train_quarter_full, scale = T,
                      method = pls::pls.options()$plsralg)

train_full_augmented <- train_full %>% 
  dplyr::mutate(
    .pred = predict(m_year, newdata = train_full, ncomp = ncp_year) %>% drop
  )
test_full_augmented <- test_full %>% 
  dplyr::mutate(
    .pred = predict(m_year, newdata = test_full, ncomp = ncp_year) %>% drop
  )

train_quarter_full_augmented <- train_quarter_full %>% 
  dplyr::mutate(
    .pred = predict(m_quarter, newdata = train_quarter_full, ncomp = ncp_quarter) %>% drop
  )
test_quarter_full_augmented <- test_quarter_full %>% 
  dplyr::mutate(
    .pred = predict(m_quarter, newdata = test_quarter_full, ncomp = ncp_quarter) %>% drop
  )


# ---------------------
# perf yearly model
# ---------------------

# train
dplyr::bind_rows(

dplyr::bind_rows(
  dplyr::bind_rows(
    train_full_augmented %>% 
      yardstick::rsq(truth = gradient_axis1, estimate = .pred),
    train_full_augmented %>% 
      yardstick::rmse(truth = gradient_axis1, estimate = .pred)
  ) %>% 
    tibble::add_column(type = "train", .before = 1),
  dplyr::bind_rows(
    # test
    test_full_augmented %>% 
      yardstick::rsq(truth = gradient_axis1, estimate = .pred),
    test_full_augmented %>% 
      yardstick::rmse(truth = gradient_axis1, estimate = .pred)
  )%>% 
    tibble::add_column(type = "train", .before = 1)
) %>% 
  tibble::add_column(data = "full year", .before = 1),
  

# ---------------------
# perf quarterly model
# ---------------------

# train
dplyr::bind_rows(
  dplyr::bind_rows(
    train_quarter_full_augmented %>% 
      yardstick::rsq(truth = gradient_axis1, estimate = .pred),
    train_quarter_full_augmented %>% 
      yardstick::rmse(truth = gradient_axis1, estimate = .pred) 
  ) %>% 
    tibble::add_column(type = "train", .before = 1),
  # test
  dplyr::bind_rows(
    test_quarter_full_augmented %>% 
      yardstick::rsq(truth = gradient_axis1, estimate = .pred),
    test_quarter_full_augmented %>% 
      yardstick::rmse(truth = gradient_axis1, estimate = .pred)
  ) %>% 
    tibble::add_column(type = "test", .before = 1) 
) %>% 
  tibble::add_column(data = "quarter year", .before = 1)

)

# train
train_quarter_full_augmented %>% 
  dplyr::group_by(year) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = .pred)
train_quarter_full_augmented %>% 
  dplyr::group_by(year) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = .pred) 
# test
test_quarter_full_augmented %>% 
  dplyr::group_by(year) %>% 
  yardstick::rsq(truth = gradient_axis1, estimate = .pred)
test_quarter_full_augmented %>% 
  dplyr::group_by(year) %>% 
  yardstick::rmse(truth = gradient_axis1, estimate = .pred)


dplyr::bind_rows(
  train_full_augmented %>% 
    tibble::add_column(data = "full") %>% 
    tibble::add_column(type = "train"),
  train_quarter_full_augmented %>% 
    tibble::add_column(data = "quarter") %>% 
    tibble::add_column(type = "train"),
  test_full_augmented %>% 
    tibble::add_column(data = "full") %>% 
    tibble::add_column(type = "test"),
  test_quarter_full_augmented %>% 
    tibble::add_column(data = "quarter") %>% 
    tibble::add_column(type = "test")
) %>% 
  ggplot2::ggplot(ggplot2::aes(x = gradient_axis1, y = .pred, color = data)) +
  ggplot2::geom_point() + 
  ggplot2::geom_smooth(se = F, method = "lm") +
  ggplot2::geom_abline(slope = 1, intercept = 0) +
  ggplot2::facet_grid("type")


# t-outliers
# train_full_res <- train_full_augmented %>%
#   dplyr::mutate(residual = gradient_axis1 - .pred, .before = 1) %>%
#   dplyr::pull(residual)
# train_full <- train_full[(abs(train_full_res) < sd(train_full_res) * 3), ]
# train_full_res <- train_quarter_full_augmented %>%
#   dplyr::mutate(residual = gradient_axis1 - .pred, .before = 1) %>%
#   dplyr::pull(residual)
# train_quarter_full <- train_quarter_full[(abs(train_full_res) < sd(train_full_res) * 3), ]

