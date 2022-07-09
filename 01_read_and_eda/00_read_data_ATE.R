library(magrittr)

source("globals/global_variables.R")

# ----------------------------------------------------------------------------------------
# Read milk data ####
# ----------------------------------------------------------------------------------------
  
#file's reading
milk_dat <- read.csv(
  file="raw_data/milk_total.csv",
  sep = ';', dec = ".",
  header=FALSE, 
  na.strings = c("."), 
  col.names = milk_col_names
)

head(milk_dat)  

# Fatty acids columns
FAs <- c(
  grep(pattern = "^pC[[:digit:]]", milk_col_names, value = T),
  grep(pattern = "^pFA", milk_col_names, value = T)
)

# Units
milk_dat %<>% 
  # 1. Change fat, protein, lactose g/cl -> g/dl (* 10)
  dplyr::mutate(dplyr::across(
    .cols = c("fat", "protein", "lactose"),
    .fns = function(x) x/10
  )) %>% 
  # 2. Update milk to express dl instead of kg
  dplyr::mutate(pmilk_dl = pmilk * 10/1.03) %>% 
  # 3. Express the FA in percentage in fat %>% 
  dplyr::mutate(dplyr::across(
    .cols = dplyr::all_of(FAs),
    ~ (.x * pmilk_dl) / (fat * pmilk_dl)
    # list(perc = ~ (.x * pmilk_dl) / (fat * pmilk_dl))
  ))

save(milk_dat, file = "data/milk_dat.rda")
  
# ----------------------------------------------------------------------------------------
# Read monthly milk data ####
# ----------------------------------------------------------------------------------------

milk_month_dat <- read.csv(
  file="raw_data/milk_month.csv",
  sep = ';', dec = ".",
  header=FALSE, 
  na.strings = c("."),
  col.names = milk_month_col_names
)

milk_month_dat %<>%  
  dplyr::mutate( 
    year_quarter = lubridate::quarter(lubridate::my(paste(month, year, sep = "-"))),
    .after = month
  ) %>% 
  dplyr::mutate( year_quarter = dplyr::case_when(
    year_quarter == 1 ~ "Winter",
    year_quarter == 2 ~ "Spring",
    year_quarter == 3 ~ "Summer",
    year_quarter == 4 ~ "Autumn"
  ) ) 

t <- milk_month_dat[1:10, 1:12]
mean(is.na(milk_month_dat$lactose))

milk_quarter_dat <- milk_month_dat %>% 
  dplyr::group_by(farmerID, year, year_quarter) %>% 
  dplyr::summarise(
  dplyr::across(dplyr::all_of(c(labo, predictions)), 
                .fns = function(x){
    weight <- dplyr::cur_data() %>% 
      dplyr::pull(paste("n", dplyr::cur_column(), sep = "_"))
    weighted.mean(x, weight, na.rm = T)
  })
)

dim(milk_quarter_dat)

milk_quarter_dat %<>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(dplyr::across(
    .cols = c("fat", "protein", "lactose"),
    .fns = function(x) x/10
  )) %>% 
  # 2. Update milk to express dl instead of kg
  dplyr::mutate(pmilk_dl = pmilk * 10/1.03) %>% 
  # 3. Express the FA in percentage in fat %>% 
  dplyr::mutate(dplyr::across(
    .cols = dplyr::all_of(FAs),
    ~ (.x * pmilk_dl) / (fat * pmilk_dl)
    # list(perc = ~ (.x * pmilk_dl) / (fat * pmilk_dl))
  ))

save(milk_quarter_dat, 
     file = "data/milk_quarter_dat.rda", 
     compress = "xz")

# ----------------------------------------------------------------------------------------
# Read accounting data ####
# ---------------------------------------------------------------------------------------- 

# technico_dat <- read.csv(
#   # file="raw_data/technico_eco.csv",
#   file="raw_data/DB_technico_20201210.csv",
#   sep = ';', dec = ".",
#   header=TRUE, 
#   na.strings = c(".")
# )

# New data (from 2006 to 2020)
technico_dat <- read.table(
  file="raw_data/DB_technico_dairy_2020.txt",
  dec = ".",
  sep = "\t",
  header=TRUE,
  na.strings = c(".", "", " ", "\t") 
  # fill = T
)

technico_dat %<>% 
  dplyr::mutate(
    VACTRAHASF = ifelse(VACTRAHASF == .01 | VACTRAHASF == .02, 
                        VACTRAHASF * 100, 
                        VACTRAHASF),
    # UMOFAMIL transformation
    UMOFAMIL = ifelse(UMOFAMIL < 0.1, UMOFAMIL * 100, UMOFAMIL), 
    UMOFAMIL = ifelse(UMOFAMIL == 0, NA, UMOFAMIL),
    UMOFAMIL = ifelse(UMOFAMIL == 0.1, 1, UMOFAMIL),
    # UMOTOTAL transformation
    UMOTOTAL = ifelse(UMOTOTAL <= 0.1, UMOTOTAL * 100, UMOTOTAL),
    UMOTOTAL = ifelse(UMOTOTAL == 0.25, NA, UMOTOTAL),
    UMOTOTAL = ifelse(UMOTOTAL == 0.5, 1.25, UMOTOTAL), 
    # AREUGBPRE transformation
    AREUGBPRE = ifelse(AREUGBPRE < 1 & AREUGBPRE > 0, AREUGBPRE * 100, AREUGBPRE),
    # AREUGBMAI transformation
    AREUGBMAI = ifelse(is.na(AREUGBMAI), 0, AREUGBMAI),
    AREUGBMAI = ifelse(AREUGBMAI <= 0.1 & AREUGBMAI > 0, 
                       AREUGBMAI * 100,
                       AREUGBMAI),
    # CFERME 902121 filter
    PCENSAC = ifelse(CFERME == "902124", NA, PCENSAC),
    PCPRAIRISF = ifelse(CFERME == "902124", NA, PCPRAIRISF),
    PCFAUCH1C = ifelse(CFERME == "902124", NA, PCFAUCH1C),
    PCFAUCHAC = ifelse(CFERME == "902124", NA, PCFAUCHAC),
    PCENS1C = ifelse(CFERME == "902124", NA, PCENS1C),
    # Others misc transformations
    PCFAUCH1C = ifelse(PCFAUCH1C > 100, NA, PCFAUCH1C),
    PCENS1C = ifelse(PCENS1C > 100, NA, PCENS1C),
    BIO = ifelse(is.na(BIO), 0, BIO),
    LITLAIHASF = ifelse(LITLAIHASF == 0, NA, LITLAIHASF),
    UGBTOTHASF = ifelse(UGBTOTHASF %in% c(.01, .02, .03, .04), 
                        UGBTOTHASF *100, 
                        UGBTOTHASF),
    LITVACHEVT = ifelse(LITVACHEVT < 900, NA, LITVACHEVT),
    VACHHAPAT = ifelse(VACHHAPAT < .1, VACHHAPAT * 100, VACHHAPAT),
    VACHHAPAT = ifelse(VACHHAPAT == 0, NA, VACHHAPAT)
  ) %>% 
  dplyr::filter(exercice != 6)

technico_dat %<>% 
  dplyr::select(dplyr::all_of(
    c(tech_identification_var, tech_gradient_var)
  )) %>% 
  tibble::as_tibble()

# AC Dalcq filters

save(technico_dat, file = "data/technico_dat.rda")


