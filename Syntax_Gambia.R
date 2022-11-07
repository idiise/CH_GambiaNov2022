
# Import Package ----------------------------------------------------------

library(dplyr)
library(haven)
library(expss)
library(openxlsx)
library(writexl)
library(labelled)

# import dataset ----------------------------------------------------------
dataset_GMB <- read_sav("Gambia_weight_and_calculation.sav")
dataset_GMB <- dataset_GMB %>% to_factor()

dataset_GMB <- dataset_GMB %>% mutate(
  FCSCat28  = case_when(
    FCSCat28 == "Acceptable" ~ "FCG_Acceptable",
    FCSCat28 == "Borderline" ~ "FCG_Borderline",
    FCSCat28 == "Poor" ~ "FCG_Poor"
  ),
  HDDS_CH = case_when(
    HDDS_CH == "Phase1" ~ "HDDS_Phase1",
    HDDS_CH == "Phase2" ~ "HDDS_Phase2",
    HDDS_CH == "Phase3" ~ "HDDS_Phase3",
    HDDS_CH == "Phase4" ~ "HDDS_Phase4",
    HDDS_CH == "Phase5" ~ "HDDS_Phase5",
    
  ),
  HHhS_CH = case_when(
    HHhS_CH == "Phase1" ~ "HHS_Phase1",
    HHhS_CH == "Phase2" ~ "HHS_Phase2",
    HHhS_CH == "Phase3" ~ "HHS_Phase3",
    HHhS_CH == "Phase4" ~ "HHS_Phase4",
    HHhS_CH == "Phase5" ~ "HHS_Phase5"
  ),
  LhCSICat = case_when(
    LhCSICat == "NoStrategies" ~ "LhHCSCat_NoStrategies",
    LhCSICat == "StressStrategies" ~ "LhHCSCat_StressStrategies",
    LhCSICat == "CrisisStrategies" ~ "LhHCSCat_CrisisStategies",
    LhCSICat == "EmergencyStrategies" ~ "LhHCSCat_EmergencyStrategies"
  ),
  rCSI_CH = case_when(
    rCSI_CH == "Phase1" ~ "rCSI_Phase1",
    rCSI_CH == "Phase2" ~ "rCSI_Phase2",
    rCSI_CH == "Phase3" ~ "rCSI_Phase3",
  )
)


test <- dataset_GMB %>% tab_cells(ADMIN1Name) %>% 
  tab_cols(CARI_Classification) %>% 
  tab_stat_cpct() %>% 
  tab_pivot()

test <- dataset_GMB %>% 
  cross_rpct(ADMIN1Name,CARI_Classification, total_row_position = "none")

test2 <- dataset_GMB %>% 
  cross_rpct(ADMIN1Name,FCSCat28, total_row_position = "none")

test5 <- dataset_GMB %>% 
  cross_rpct(ADMIN1Name,FCSCat28, total_row_position = "none",
             weight = Weight)

test3 <- dataset_GMB %>% 
  cross_rpct(ADMIN1Name,HDDS_CH, total_row_position = "none")

test4 <- test2 %>% left_join(test3, by =  "row_labels")

test6 <- dataset_GMB %>% 
  cross_rpct(ADMIN1Name,list(FCSCat28,HDDS_CH), total_row_position = "none")

test7 <- dataset_GMB %>% 
  cross_rpct(ADMIN1Name,list(FCSCat28,HDDS_CH,LhCSICat,rCSI_CH,  Foodexp_4pt,HHhS_CH), total_row_position = "none",
             weight = Weight)

test8 <- dataset_GMB %>% 
  cross_rpct(ADMIN1Name,list(FCSCat28,HDDS_CH,HHhS_CH,LhCSICat,rCSI_CH,shock_6M,shock_1,food_stock,
                             HHAsstTypeGov,HHAsstTypeComm,HHAsstTypeRelig,Foodexp_4pt), total_row_position = "none",
             weight = Weight) 

test9 <- as.data.frame(test8)
test9 <- test9 %>% mutate_if(
  is.numeric, round, 1
)

test10 <- as.data.frame(test8)
test10 <- test10 %>% mutate_if(
  is.numeric, round, 1
)


# saving ------------------------------------------------------------------
wb <- createWorkbook()
sh <- addWorksheet(wb, "matrice")

xl_write(test10, wb, sh)

saveWorkbook(wb, "Matrice_intermediaire_Gambia.xlsx", overwrite = TRUE)

# end of syntaxe ----------------------------------------------------------


