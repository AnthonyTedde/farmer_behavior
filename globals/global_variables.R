# Milk columns
milk_col_names <- c(
  "farmerID", "year",
  "fat", "protein", "urea", "lactose", "pfat",
  "pmilk", "pch4_mir", "plactose", 
  "plactofer_EMR", "plactofer_no_EMR", "pmilk_Lactofer_EMR",
  "pch4_full",
  "pmilk_acidity_1",
  "pcheese_yield_fresh_1", "pcheese_yield_dry_1",
  "pprotein_N",
  "pBW",
  # protein
  "pprotein_N__old", "pcasein_alpha_s1", "pcasein_alpha_s2", "pcasein_beta",
  "pLBA", "pBLG", "pcasein_tot", "pwhey", "pprotein_N__old2", "pcasein_kjeldahl_tot",
  # FA
  "pC4", "pC6", "pC8", "pC10", "pC12", "pC14", "PC14.1", "pC16", "pC16.1_c", "pC17",
  "pC18", "pC18.1_trs__tot", "pC18.1_c9", "pC18.1_c9__tot", "pC18.2__tot", "pC18.2_c9_c12",
  "pC18.3_c9_c12_c15", "pC18.2_c9_trs11", "pFA_sat", "pFA_mono", "pFA_poly", "pFA_insat",
  "pFA_shrt", "pFA_med", "pFA_long", "pFA_iso", "pFA_o3", "PFA_o6", "pFA_odd__tot",
  "pFA_trs__tot", "pC18.1__tot", 
  "pNa", "pCa", "pP", "pMg", "pK",
  "pcitrate", "pacetone", "pBHB", "ppH", "pmilk_acidity_2",
  "pbotter_yield", "ppH_botter",
  "pRCT_jr", "pRCT_K20JG_log2", "pRCT_K20JR", "pK20JR_log2", "pA30",
  "pcheese_yield_dry_2", "pcheese_yield_fresh_2", "pcheese_yield_dry_3",
  "pcheese_yield_factory", "pcheese_yield_curd", "pcheese_yield_solid",
  "psolid_recover", "pfat_recover", "pprotein_recover",
  "pRCT_CRM", "pK20_CRM", "pRCT_K20_CRM", "pA30_CRM",
  "pyog_pH", "pyog_activity", "pyog_dry", "pyog_syn", "pyog_texture",
  "plactofer_test",
  "pEB",
  "pprotein_efficiency",
  "pglucose_6P_milk", "pglucose_free_milk", "pBOHB_milk", "pIsoC_milk", "purea_milk", 
  "pNAGase_milk", "pLDH_milk", "pUA_milk", "pprogesterone_milk",
  "pIGF.1_blood", "pglucose_blood", "purea_blood", "pcholesterol_blood", 
  "pfructosamine_blood", "pBOHB_log10_blood", "pNEFA_blood", "pprogesterone_blood",
  "pDMI", "pRFI1", "pRFI2"
)  

# AC-gradient variables
tech_gradient_var <- c(
  "VACTRAHASF", "LITLAIHASF", "UGBTOTHASF", "PCSCONSSF", "PCMAISSF", "PCPRAIRISF", 
  "PCFAUCH1C", "PCFAUCHAC", "PCENS1C", "PCENSAC", "NHAPRE", "AREUGBPRE", "AREUGBMAI", 
  "EQCONCVT", "LITVACHEVT"
)
tech_gradient_var_en <- c(
  "MkcowsHAFoA", "MilkHAFoA", "LUHAFoA", "PCgrassFoA", "PCcornsilageFoA",
  "PCsilagegrassFoA", "PC1haycutFoA", "PCohaycutFoA", "PC1silagecutFoA", 
  "PCosilagecutFoA", "NgrazedA", "grazedareaLU", "cornsilageLU", "concpurchasedMkCow", 
  "milkMkCow"
)
# Identification columns
tech_identification_var <- c("CFERME", "exercice")

# Matching table names
matching_col_names <- c("lot", "poslot2", "labo_dt", "farmerID", "sample_dt",
                 "CFERME", "fin")

# Potential predictors from milk analysis
milk_potential_predictors <- c(
  "fat", "protein", "urea", "lactose", "pfat",
  "pmilk", "pch4_mir", "plactose", 
  "plactofer_EMR", "plactofer_no_EMR", "pmilk_Lactofer_EMR",
  "pch4_full",
  "pmilk_acidity_1",
  "pcheese_yield_fresh_1", "pcheese_yield_dry_1",
  "pprotein_N",
  "pBW",
  # protein
  "pprotein_N__old", "pcasein_alpha_s1", "pcasein_alpha_s2", "pcasein_beta",
  "pLBA", "pBLG", "pcasein_tot", "pwhey", "pprotein_N__old2", "pcasein_kjeldahl_tot",
  # FA
  "pC4", "pC6", "pC8", "pC10", "pC12", "pC14", "PC14.1", "pC16", "pC16.1_c", "pC17",
  "pC18", "pC18.1_trs__tot", "pC18.1_c9", "pC18.1_c9__tot", "pC18.2__tot", "pC18.2_c9_c12",
  "pC18.3_c9_c12_c15", "pC18.2_c9_trs11", "pFA_sat", "pFA_mono", "pFA_poly", "pFA_insat",
  "pFA_shrt", "pFA_med", "pFA_long", "pFA_iso", "pFA_o3", "PFA_o6", "pFA_odd__tot",
  "pFA_trs__tot", "pC18.1__tot", 
  "pNa", "pCa", "pP", "pMg", "pK",
  "pcitrate", "pacetone", "pBHB", "ppH", "pmilk_acidity_2",
  "pbotter_yield", "ppH_botter",
  "pRCT_jr", "pRCT_K20JG_log2", "pRCT_K20JR", "pK20JR_log2", "pA30",
  "pcheese_yield_dry_2", "pcheese_yield_fresh_2", "pcheese_yield_dry_3",
  "pcheese_yield_factory", "pcheese_yield_curd", "pcheese_yield_solid",
  "psolid_recover", "pfat_recover", "pprotein_recover",
  "pRCT_CRM", "pK20_CRM", "pRCT_K20_CRM", "pA30_CRM",
  "pyog_pH", "pyog_activity", "pyog_dry", "pyog_syn", "pyog_texture",
  "plactofer_test",
  "pEB",
  "pprotein_efficiency",
  "pglucose_6P_milk", "pglucose_free_milk", "pBOHB_milk", "pIsoC_milk", "purea_milk", 
  "pNAGase_milk", "pLDH_milk", "pUA_milk", "pprogesterone_milk",
  "pIGF.1_blood", "pglucose_blood", "purea_blood", "pcholesterol_blood", 
  "pfructosamine_blood", "pBOHB_log10_blood", "pNEFA_blood", "pprogesterone_blood",
  "pDMI", "pRFI1", "pRFI2"
)  

y <- "gradient_axis1"

if ( grepl("[ate][Lalitude]", Sys.info()["nodename"]) ){
  # Then local
  ncpu <- 5
  niter <- 150
  initial_set_n <- 10
  early_stop <- 10
}else{ 
  ncpu <- 10
  niter <- 500
  initial_set_n <- 40
  early_stop <- 40
}
