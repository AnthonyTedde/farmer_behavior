library(magrittr)

data("technico_milk")

source("globals/global_variables.R")

gradient1_form <- milk_col_names %>% 
  paste(collapse = " + ") %>% 
  paste("gradient_axis1", ., sep = "~") %>% 
  as.formula()

gradient2_form <- milk_col_names %>% 
  paste(collapse = " + ") %>% 
  paste("gradient_axis2", ., sep = "~") %>% 
  as.formula()

# LM
gradient1_lm <- lm(gradient1_form, technico_milk)
summary(gradient1_lm)
gradient2_lm <- lm(gradient2_form, technico_milk)
summary(gradient2_lm)

plot(gradient1_lm)

# GLM
# gradient1_glm <- glm(gradient1_form, data = technico_milk, 
#                      family = gaussian(link = "log"))

# pls
gradient1_pls <- pls::mvr(gradient1_form, ncomp = 60, data = technico_milk, scale = T,
                          method = "cppls")
 
response <- data.frame(
  obs = technico_milk$gradient_axis1, 
  prd = predict(gradient1_pls, ncomp = 60) %>% drop
)  

response %>% 
  ggplot2::ggplot(ggplot2::aes(x = obs, y = prd)) +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") + 
  ggplot2::geom_point()  

yardstick::rsq(response, truth = obs, estimate = prd)
