library(magrittr)

source("globals/global_variables.R")

  
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
  dplyr::mutate(dplyr::across(
    .cols = c("fat", "protein", "lactose"),
    .fns = function(x) x/10
  )) 

milk_month_dat %<>% 
  dplyr::mutate(
    lactose = ifelse(lactose>10, lactose / 10, lactose)
  ) 
 

milk_month_dat$fat %>% hist(breaks = 500)
milk_month_dat$fat[milk_month_dat$fat < 1] %>% hist(breaks = 100)
milk_month_dat$protein %>% hist(breaks = 500)
milk_month_dat$lactose %>% hist(breaks = 500)
milk_month_dat$pprotein_N %>% hist(breaks = 500)
milk_month_dat$pprotein_N %>% range(na.rm = T)
quantile(milk_month_dat$pprotein_N, na.rm = T)
sum(milk_month_dat$pprotein_N > 600, na.rm = T)

quantile(milk_month_dat$protein, na.rm = T)
quantile(milk_month_dat$fat, na.rm = T)
quantile(milk_month_dat$lactose, na.rm = T)

# MAT
milk_month_dat %<>% 
  tibble::as_tibble() %>% 
  dplyr::filter(
    fat > 1.5 & fat < 9, # ICAR + remove skim milk
    protein > 1 & protein < 7 # ICAR
  )

milk_month_dat %<>% 
  dplyr::filter(year < 2022)

# Quid des valeurs manquantes
naniar::gg_miss_upset(milk_month_dat)
 
# Impute missing lactose
working_month_dat <- milk_month_dat %>% 
  dplyr::select(-c("farmerID", "year", "month"), -dplyr::starts_with("n_"))

ncomp <- 20
lactose_pls <- pls::mvr(lactose ~ ., ncomp = ncomp, data = working_month_dat,
                        scale = T, method = pls::pls.options()$plsralg)

# no_na <- working_month_dat %>% 
#   tidyr::drop_na()
# no_na %>% 
#   dplyr::mutate(
#     .pred = predict(lactose_pls, newdata = no_na, ncomp = ncomp)
#   ) %>% 
#   yardstick::rsq(truth = lactose, estimate = .pred)

milk_month_dat_no_na <- milk_month_dat %>% 
  tidyr::drop_na(lactose)
milk_month_dat_na <- milk_month_dat %>% 
  dplyr::filter(is.na(lactose)) %>% 
  tidyr::drop_na(-c("lactose")) 
milk_month_dat_na %<>% 
  dplyr::mutate(lactose = predict(lactose_pls, newdata = milk_month_dat_na, ncomp = ncomp)) %>% 
  dplyr::mutate(n_lactose = 1)

milk_month_dat_na$lactose %>% hist(breaks = 100)

milk_month_dat <- dplyr::bind_rows(
  milk_month_dat_no_na, milk_month_dat_na
)
dim(milk_month_dat)

naniar::gg_miss_upset(milk_month_dat)
milk_month_dat %<>% 
  tidyr::drop_na()
milk_month_dat$n_lactose %>% table
milk_month_dat %>% 
  dplyr::filter(n_lactose > 30)


milk_month_dat$fat %>% hist(breaks = 100)
milk_month_dat$protein %>% hist(breaks = 100)
milk_month_dat$lactose %>% hist(breaks = 100)  

fat_norm <- bestNormalize::orderNorm(milk_month_dat$fat)
protein_norm <- bestNormalize::orderNorm(milk_month_dat$protein)
lactose_norm <- bestNormalize::orderNorm(milk_month_dat$lactose)

fat_norm$x.t %>% hist(breaks = 100)
protein_norm$x.t %>% hist(breaks = 100)
lactose_norm$x.t %>% hist(breaks = 100)

rm_tails <- function(dat, p = 0.001){ 
  up <- quantile(fat_norm$x.t, probs = 1 - p/2)
  down <- quantile(fat_norm$x.t, probs = 0 + p/2)
  down < dat & dat < up
}
fat_keep <- rm_tails(fat_norm$x.t)
prot_keep <- rm_tails(protein_norm$x.t)
lactose_keep <- rm_tails(lactose_norm$x.t)

keep <- fat_keep & prot_keep & lactose_keep

milk_month_dat <- milk_month_dat[keep, ]

milk_month_dat$fat %>% hist(breaks = 100)
milk_month_dat$protein %>% hist(breaks = 100)
milk_month_dat$lactose %>% hist(breaks = 100)  

dim(milk_month_dat)
save(milk_month_dat, file = "data/milk_month_dat.rda", compress = "xz")

  
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


#-------------
# AC filter
#-------------

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

#-------------
# Remove NAs
#-------------

naniar::gg_miss_upset(technico_dat) 
dim(technico_dat) 
technico_dat %<>% 
  tidyr::drop_na()

# milked cows per hectare of forage area
technico_dat$VACTRAHASF %>% hist(breaks = 100)
# milk yield per hectare of forage area
technico_dat$LITLAIHASF %>% hist(breaks = 100)
# livestock unit per hectare of forage area
technico_dat$UGBTOTHASF %>% hist(breaks = 100)
# percentage of grazed area  in the forage area
technico_dat$PCSCONSSF %>% hist(breaks = 100)
# Percentage of corn silage in the forage area
technico_dat$PCMAISSF %>% hist(breaks = 50)
# Percentage of silage grass in the forage area
technico_dat$PCPRAIRISF %>% hist(breaks = 50)
# percentage of first hay cut  in the forage area
technico_dat$PCFAUCH1C %>% hist(breaks = 100)
# Percentage of other hay cut in the forage area 
technico_dat$PCFAUCHAC %>% hist(breaks = 100)
# Percentage of first silage cut  in the forage area
technico_dat$PCENS1C %>% hist(breaks = 100)
# Percentage of other silage cut in the forage area
technico_dat$PCENSAC %>% hist(breaks = 100)
# N fertilizer per hectare of grazed area 
technico_dat$NHAPRE %>% hist(breaks = 100) 
# Amount of grazed area per livestock unit
technico_dat$AREUGBPRE %>% hist(breaks = 100) 
technico_dat$AREUGBPRE %>% range
# amount of corn silage per livestock unit
technico_dat$AREUGBMAI %>% hist(breaks = 100)  
# concentrate equivalents purchased per milked cow
technico_dat$EQCONCVT %>% hist(breaks = 100) 
# milk yield per milked cow
technico_dat$LITVACHEVT %>% hist(breaks = 100) 
   
save(technico_dat, file = "data/technico_dat.rda")


