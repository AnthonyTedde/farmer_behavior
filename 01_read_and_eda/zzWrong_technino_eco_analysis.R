library(magrittr)

# AC-gradient variables
tech_gradient_var <- c(
  "VACTRAHASF", "LITLAIHASF", "UGBTOTHASF", "PCSCONSSF", "PCMAISSF", "PCPRAIRISF", 
  "PCFAUCH1C", "PCFAUCHAC", "PCENS1C", "PCENSAC", "NHAPRE", "AREUGBPRE", "AREUGBMAI", 
  "EQCONCVT", "LAITVT"
)

wrong_technico_dat <- read.csv(
  file="raw_data/technico_eco.csv",
  # file="raw_data/DB_technico_20201210.csv",
  sep = ';', dec = ".",
  header=TRUE, 
  na.strings = c(".")
)

match <- read.table("raw_data/matching_tbl.txt", 
                    header = FALSE, 
                    col.names = matching_col_names)

match %<>%  
  dplyr::select(farmerID, CFERME) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(CFERME != 0)
head(match) 

wrong_technico_milk <- wrong_technico_dat %>% 
  dplyr::inner_join(match) %>% 
  dplyr::mutate(
    # year = stringr::str_pad(exercice, width = 3, pad = "0") %>% 
    year = stringr::str_pad(exercice, width = 3, pad = "0") %>% 
      paste0(2, .) %>% as.integer()
  ) %>%   
  dplyr::inner_join(milk_dat, by = c("farmerID", "year")) %>% 
  tibble::as_tibble() %>% 
  # Selection des variables d'AC
  dplyr::select(c("farmerID", "exercice", dplyr::all_of(tech_gradient_var)))


wrong_technico_milk$LAITVT %>% hist

wrong_technico_milk <- wrong_technico_milk %>% 
  dplyr::filter(LAITVT == 0) 

write.csv(wrong_technico_milk, file = "raw_data/technico_milk_zero.csv")

wrong_technico_NA <- wrong_technico_milk %>% 
  dplyr::mutate(dplyr::across(
    .cols = dplyr::everything(), 
    .fns = function(x) ifelse(x == 0, NA, x)
  ))

naniar::gg_miss_upset(wrong_technico_NA)




