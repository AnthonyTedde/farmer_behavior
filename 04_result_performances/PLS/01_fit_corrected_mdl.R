library(magrittr)

library(plsmod)
library(bestNormalize)

suffixe <- c("year_corrected_full", "season_corrected_full", "season_corrected_partial",  
             "winsum_corrected_full", "winsum_corrected_partial")  
train_vect <- paste("train", suffixe, sep = "_")  
test_vect <- paste("test", suffixe, sep = "_")
rslt_vect <- paste("train", suffixe, "rslt", sep = "_")
wfl_vect <- paste("train", suffixe, "wfl", sep = "_")

data(list = c(train_vect, test_vect, rslt_vect, wfl_vect))
 
   
# -------------------------------------------------------------------------------------- #
# Compute fitted models
# -------------------------------------------------------------------------------------- #

info <- list(train = train_vect, 
             test = test_vect, 
             rslt = rslt_vect, 
             wfl = wfl_vect,
             suffixe = suffixe) %>% 
  purrr::pmap(list)
for(d in info){
  
  
  rslt <- get(d$rslt)
  wfl <- get(d$wfl)
  bst <- rslt %>% tune::select_best()
  # onese <- rslt %>% tune::select_by_one_std_err(num_comp, predictor_prop)
  onese <- rslt %>% tune::select_by_one_std_err(num_comp)
  
  dat <- get(d$train)
  
  # fit bst
  fit_bst <- wfl %>% 
    tune::finalize_workflow(bst) %>% 
    parsnip::fit(dat)
  # fit onese
  fit_onese <- wfl %>% 
    tune::finalize_workflow(onese) %>% 
    parsnip::fit(dat)
  
  fit_bst_name <- paste(d$train, "pls_fit_bst", sep = "_") 
  assign(fit_bst_name, fit_bst)
  
  fit_onese_name <- paste(d$train, "pls_fit_onese", sep = "_") 
  assign(fit_onese_name, fit_onese)
  
  # Save
  save(list = fit_bst_name, 
       file = glue::glue("data/{fit_bst_name}.rda"),
       compress = "xz")
  save(list = fit_onese_name, 
       file = glue::glue("data/{fit_onese_name}.rda"),
       compress = "xz")
  
}