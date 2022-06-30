library(magrittr)
data("technico_milk")

technico_milk %>% 
  dplyr::pull(fat) %>% hist

technico_milk %>% 
  dplyr::pull(protein) %>% hist

technico_milk %<>% 
  dplyr::filter(fat > 1.5 & fat < 9) %>% 
  dplyr::filter(fat > 1 & fat < 7) 

save(technico_milk, file ="data/technico_milk.rda" )