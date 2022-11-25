
# import package and codebook ---------------------------------------------

library(tidyverse)
library(labelled)
library(writexl)
library(haven)


setwd("C:\\Users\\USER MSI\\Documents\\WFP\\CH octobre Novembre 2022\\Gambia\\Result\\Final graphque Gambia/Total/")
data_gambia <- read_sav("C:\\Users\\USER MSI\\Documents\\WFP\\CH octobre Novembre 2022\\Gambia/Gambia_weight.sav")
data_gambia <- data_gambia %>% to_factor()
# codebook_LIB <- var_label(data_liberia)
# codebook_LIB <- as.data.frame(do.call(rbind,codebook_LIB))
# codebook_LIB <- codebook_LIB %>% rownames_to_column()
# write_xlsx(codebook_LIB, "C:\\Users\\USER MSI\\Documents\\WFP\\CH octobre Novembre 2022\\Gambia/codebook_GMB.xlsx")

# names(data_liberia) <- str_replace_all(string = names(data_liberia), pattern = "Section6",replacement = "")
# names(data_liberia) <- str_replace_all(string = names(data_liberia), pattern = "livelihoodcoping",replacement = "")
# names(data_liberia) <- str_replace_all(string = names(data_liberia), pattern = "Section12",replacement = "")


# FCS Liberia -------------------------------------------------------------
data_gambia <- data_gambia %>% mutate(
  FCSStap = replace_na(FCSStap, 0),
  FCSPulse = replace_na(FCSPulse, 0),
  FCSDairy = replace_na(FCSDairy, 0),
  FCSPr = replace_na(FCSPr, 0),
  FCSFat = replace_na(FCSFat,0),
  FCSVeg =  replace_na(FCSVeg, 0),
  FCSSugar = replace_na(FCSSugar, 0),
  FCSFruit = replace_na(FCSFruit, 0)
)

data_gambia <- data_gambia %>% mutate(
  FCS = (2*FCSStap) + (3 * FCSPulse) + (4 * FCSDairy) + (4 * FCSPr) + (FCSVeg) +
    (0.5 * FCSFat) + (0.5 * FCSSugar) + FCSFruit
) %>% mutate( FCSCat28 =
                case_when(
                  FCS <= 21 ~ "Poor",
                  between(FCS, 21.5, 35) ~ "Borderline",
                  FCS > 35 ~ "Acceptable"
                )
)

gfcs <- funModeling::freq(data_gambia, "FCSCat28")
gfcs
write_xlsx(gfcs, "fcstotal.xlsx")
# HDDS liberia ------------------------------------------------------------

data_gambia <- data_gambia %>% mutate(
  HDDSStapCer = case_when(HDDSStapCer == "Yes" ~ 1, TRUE ~ 0),
  HDDSStapRoot = case_when(HDDSStapRoot  == "Yes" ~ 1, TRUE ~ 0),
  HDDSVeg = case_when(HDDSVegOrg  == "Yes" | HDDSVegGre == "Yes" | HDDSVegOth == "Yes" ~ 1, TRUE ~ 0),
  HDDSFruit = case_when(HDDSFruitOrg == "Yes" | HDDSFruitOth == "Yes" ~ 1, TRUE ~ 0),
  HDDSPrMeat = case_when(HDDSPrMeatF == "Yes" | HDDSPrMeatO == "Yes" ~ 1, TRUE ~ 0),
  HDDSPrEgg = case_when(HDDSPrEgg  == "Yes" ~ 1, TRUE ~ 0),
  HDDSPrFish = case_when(HDDSPrFish == "Yes" ~ 1, TRUE ~ 0),
  HDDSPulse = case_when(HDDSPulse == "Yes" ~ 1, TRUE ~ 0),
  HDDSDairy = case_when(HDDSDairy == "Yes" ~ 1, TRUE ~ 0),
  HDDSFat = case_when(HDDSFat == "Yes" ~ 1, TRUE ~ 0),
  HDDSSugar = case_when(HDDSSugar == "Yes" ~ 1, TRUE ~ 0),
  HDDSCond = case_when(HDDSCond == "Yes"~ 1, TRUE ~ 0))

#Calculate HDDS and Cadre Harmonise Phases
data_gambia <- data_gambia %>% mutate(HDDS = HDDSStapCer +HDDSStapRoot +HDDSVeg +HDDSFruit +HDDSPrMeat +HDDSPrEgg +HDDSPrFish +HDDSPulse +HDDSDairy +HDDSFat +HDDSSugar +
                                          HDDSCond) %>% mutate( HDDS_CH = case_when(
                                            HDDS >= 5 ~ "Phase1",
                                            HDDS == 4 ~ "Phase2",
                                            HDDS == 3 ~ "Phase3",
                                            HDDS == 2 ~ "Phase4",
                                            HDDS < 2 ~ "Phase5"
                                          )
                                          )

ghdds <- funModeling::freq(data_gambia, "HDDS_CH")
ghdds
write_xlsx(ghdds, "hddstotal.xlsx")

# rCSI Liberia ------------------------------------------------------------
data_gambia <- data_gambia %>% mutate(
  rCSILessQlty = replace_na(rCSILessQlty, 0),
  rCSIBorrow = replace_na(rCSIBorrow, 0),
  rCSIMealAdult = replace_na(rCSIMealAdult, 0),
  rCSIMealNb = replace_na(rCSIMealNb,0),
  rCSIMealSize = replace_na(rCSIMealSize, 0)
)

data_gambia <- data_gambia %>% mutate(
  rCSI = rCSILessQlty + (2 * rCSIBorrow) + rCSIMealSize + (3 * rCSIMealAdult) + rCSIMealNb) %>% 
  mutate(rCSI_CH = case_when(rCSI <= 3 ~ "Phase1",
                             between(rCSI,4,18) ~ "Phase2",
                             rCSI >= 19 ~ "Phase3"
  )
  
  )

grCSI <- funModeling::freq(data_gambia, "rCSI_CH")  
grCSI
write_xlsx(grCSI, "rCSItotal.xlsx")

# lCS Liberia -------------------------------------------------------------

data_gambia <- data_gambia %>% mutate(
  stress_coping = case_when(
    LhCSIStress1 %in% c("Yes","No; because I already sold those assets or did this activity in the last 12 months and cannot continue to do it") ~ "Yes",
    LhCSIStress2 %in% c("Yes","No; because I already sold those assets or did this activity in the last 12 months and cannot continue to do it") ~ "Yes",
    LhCSIStress3 %in% c("Yes","No; because I already sold those assets or did this activity in the last 12 months and cannot continue to do it") ~ "Yes",
    LhCSIStress4 %in% c("Yes","No; because I already sold those assets or did this activity in the last 12 months and cannot continue to do it") ~ "Yes",
    TRUE ~ "No"))
# var_label(dataset$stress_coping) <- "Did the HH engage in stress coping strategies"

#Crisis
data_gambia <- data_gambia %>% mutate(crisis_coping = case_when(
  LhCSICrisis1 %in% c("Yes","No; because I already sold those assets or did this activity in the last 12 months and cannot continue to do it") ~ "Yes",
  LhCSICrisis2 %in% c("Yes","No; because I already sold those assets or did this activity in the last 12 months and cannot continue to do it") ~ "Yes",
  LhCSICrisis3 %in% c("Yes","No; because I already sold those assets or did this activity in the last 12 months and cannot continue to do it") ~ "Yes",
  TRUE ~ "No"))
# var_label(dataset$crisis_coping) <- "Did the HH engage in crisis coping strategies"

#Emergency
data_gambia <- data_gambia %>% mutate(emergency_coping = case_when(
  LhCSIEmergency1 %in% c("Yes","No; because I already sold those assets or did this activity in the last 12 months and cannot continue to do it") ~ "Yes",
  LhCSIEmergency2 %in% c("Yes","No; because I already sold those assets or did this activity in the last 12 months and cannot continue to do it") ~ "Yes",
  LhCSIEmergency3 %in% c("Yes","No; because I already sold those assets or did this activity in the last 12 months and cannot continue to do it") ~ "Yes",
  TRUE ~ "No"))
# var_label(dataset$emergency_coping) <- "Did the HH engage in emergency coping strategies"

data_gambia <- data_gambia %>% mutate(LhCSICat = case_when(
  emergency_coping == "Yes" ~ "EmergencyStrategies",
  crisis_coping == "Yes" ~ "CrisisStrategies",
  stress_coping == "Yes" ~ "StressStrategies",
  TRUE ~ "NoStrategies"))


gLCS <- funModeling::freq(data_gambia, "LhCSICat")
gLCS
write_xlsx(gLCS, "LCStotal.xlsx")

# HHS ---------------------------------------------------------------------

data_gambia  <- data_gambia  %>% mutate(HHhSNoFood_FR_r = case_when(
  HHhSNoFood_FR == "Rarely (1–2 times)" ~ 1,
  HHhSNoFood_FR == "Sometimes (3–10 times)" ~ 1,
  HHhSNoFood_FR == "Often (more than 10 times)" ~ 2,
  TRUE ~ 0),
  HHhSBedHung_FR_r = case_when(
    HHhSBedHung_FR == "Rarely (1–2 times)" ~ 1,
    HHhSBedHung_FR == "Sometimes (3–10 times)" ~ 1,
    HHhSBedHung_FR == "Often (more than 10 times)" ~ 2,
    TRUE ~ 0),
  HHhSNotEat_FR_r = case_when(
    HHhSNotEat_FR == "Rarely (1–2 times)" ~ 1,
    HHhSNotEat_FR == "Sometimes (3–10 times)" ~ 1,
    HHhSNotEat_FR == "Often (more than 10 times)" ~ 2,
    TRUE ~ 0))
# Calculate HHhS score
data_gambia  <- data_gambia  %>% mutate(HHhS = HHhSNoFood_FR_r + HHhSBedHung_FR_r + HHhSNotEat_FR_r)

data_gambia <- data_gambia %>% mutate(
  HHhS_CH = case_when(
    HHhS == 0 ~ "Phase1",
    HHhS == 1 ~ "Phase2",
    HHhS %in% c(2,3) ~ "Phase3",
    HHhS == 4 ~ "Phase4",
    HHhS >= 5 ~ "Phase5"
  )
)

ghhs <- funModeling::freq(data_gambia, "HHhS_CH")
ghhs
write_xlsx(ghhs, "HHStotal.xlsx")

# Expenses ----------------------------------------------------------------

# # Food Cash
# cash <- names(data_gambia)[which(str_detect(string = names(data_gambia), pattern = regex("^[[:alpha:]]+._MN_1M")) == "TRUE")]
# data_gambia$HHExpFood_MN_1M <- rowSums(data_gambia[,c(cash)], na.rm = TRUE)
# # Food Credit
# credit <-  names(data_gambia)[which(str_detect(string = names(data_gambia), pattern = regex("^[[:alpha:]]+._CRD_1M")) == "TRUE")]
# data_gambia$HHExp_Food_CRD_1M <- rowSums(data_gambia[,c(credit)], na.rm = TRUE)
# # Food Assistance
# GiftAid <-  names(data_gambia)[which(str_detect(string = names(data_gambia), pattern = regex("^[[:alpha:]]+._GiftAid_1M")) == "TRUE")]
# data_gambia$HHExp_Food_GiftAid_1M <- rowSums(data_gambia[,c(GiftAid)], na.rm = TRUE)
# # Food Own Production
# Own <-  names(data_gambia)[which(str_detect(string = names(data_gambia), pattern = regex("^[[:alpha:]]+._Own_1M")) == "TRUE")]
# data_gambia$HHExp_Food_Own_1M <- rowSums(data_gambia[,c(Own)], na.rm = TRUE)
# # food expenditure
# data_gambia <- data_gambia %>% mutate(
#   food_monthly = HHExpFood_MN_1M + HHExp_Food_CRD_1M +
#     HHExp_Food_GiftAid_1M + HHExp_Food_Own_1M
# )
# 
# # Non Food 30
# No_Food <- names(data_gambia)[which(str_detect(string = names(data_gambia), pattern = regex("\\bNonFoodConsumption")) == "TRUE")]
# # No food Cash
# cash_NFood <- No_Food[str_detect(string = No_Food,pattern = "MN_1M$")]
# data_gambia$HHExpNoFood_MN_1M <- rowSums(data_gambia[,c(cash_NFood)], na.rm = TRUE)
# # No Food Credit
# credit_NFood <- No_Food[str_detect(string = No_Food,pattern = "CRD_1M$")]
# data_gambia$HHExp_NoFood_CRD_1M <- rowSums(data_gambia[,c(credit_NFood)], na.rm = TRUE)
# # No Food Assistance
# GiftAid_NFood <- No_Food[str_detect(string = No_Food,pattern = "GiftAid_1M$")]
# data_gambia$HHExp_NoFood_GiftAid_1M <- rowSums(data_gambia[,c(GiftAid_NFood)], na.rm = TRUE)
# # Food Expenditure 30 Days
# data_gambia <- data_gambia %>% mutate(
#   nonfood1_monthly = HHExpNoFood_MN_1M + HHExp_NoFood_CRD_1M +
#     HHExp_NoFood_GiftAid_1M
# )
# 
# # Expenditure 6 month
# # No food Cash 6 months
# cash_NFood6 <- No_Food[str_detect(string = No_Food,pattern = "MN_6M$")]
# data_gambia$HHExpNoFood_MN_6M <- rowSums(data_gambia[,c(cash_NFood6)], na.rm = TRUE)
# data_gambia <- data_gambia %>% mutate(
#   HHExpNoFood_MN_6M = HHExpNoFood_MN_6M/6
# )
# # No Food Credit
# credit_NFood6 <- No_Food[str_detect(string = No_Food,pattern = "CRD_6M$")]
# data_gambia$HHExp_NoFood_CRD_6M <- rowSums(data_gambia[,c(credit_NFood6)], na.rm = TRUE)
# data_gambia <- data_gambia %>% mutate(
#   HHExp_NoFood_CRD_6M = HHExp_NoFood_CRD_6M/6
# )
# # No Food Assistance
# GiftAid_NFood6 <- No_Food[str_detect(string = No_Food,pattern = "GiftAid_6M$")]
# data_gambia$NonFoodConsumption_6MHHExpNFEduFee_GiftAid_6M <- as.numeric(data_gambia$NonFoodConsumption_6MHHExpNFEduFee_GiftAid_6M)
# data_gambia$HHExp_NoFood_GiftAid_6M <- rowSums(data_gambia[,c(GiftAid_NFood6)], na.rm = TRUE)
# data_gambia <- data_gambia %>% mutate(
#   HHExp_NoFood_GiftAid_6M = HHExp_NoFood_GiftAid_6M/6
# )
# 
# # No Food Expenditure 60 days
# data_gambia <- data_gambia %>% mutate(
#   nonfood2_monthly = HHExpNoFood_MN_6M + HHExp_NoFood_CRD_6M +HHExp_NoFood_GiftAid_6M
# )
# 
# 
# #calculate food expenditure share and food expenditure share categories
# 
# data_gambia  <- data_gambia  %>%  mutate(FoodExp_share = food_monthly / (food_monthly +nonfood1_monthly +nonfood2_monthly),
#                                            FoodExp_total = food_monthly + nonfood1_monthly + nonfood2_monthly)
# 
# data_gambia <- data_gambia %>% mutate(Foodexp_4pt = case_when(
#   FoodExp_share <= .4999999 ~ 1,
#   FoodExp_share >= .5 & FoodExp_share < .6555555 ~ 2,
#   FoodExp_share >= .6555555 & FoodExp_share < .7555555 ~ 3,
#   FoodExp_share > .7555555 ~ 4))
# 
# data_gambia <- data_gambia %>% mutate(Foodexp_4pt_class = case_when(
#   Foodexp_4pt == 1 ~ "less than 50%",
#   Foodexp_4pt == 2 ~ "Between 50 and 65%",
#   Foodexp_4pt == 3 ~ "Between 65 and 75%",
#   Foodexp_4pt == 4 ~ "GGreater than 75%"))
# 
# var_label(data_gambia$Foodexp_4pt) <- "Food Expenditure categories"
# val_labels(data_gambia$Foodexp_4pt) <- c("less than 50%" = 1, "Between 50 and 65%" = 2, "Between 65 and 75%" = 3, "GGreater than 75%" = 4)

# New part expenses -------------------------------------------------------
# No food Cash 6 months
cash_NFood6 <- names(data_gambia)[which(str_detect(string = names(data_gambia), pattern = regex("^[[:alpha:]]+._MN_6M")) == "TRUE")]
data_gambia$HHExpNoFood_MN_6M <- rowSums(data_gambia[,c(cash_NFood6)], na.rm = TRUE)
data_gambia <- data_gambia %>% mutate(
  HHExpNoFood_MN_1M = HHExpNoFood_MN_6M/6
)
# No Food Credit
credit_NFood6 <-  names(data_gambia)[which(str_detect(string = names(data_gambia), pattern = regex("^[[:alpha:]]+._CRD_6M")) == "TRUE")]
data_gambia$HHExp_NoFood_CRD_6M <- rowSums(data_gambia[,c(credit_NFood6)], na.rm = TRUE)
data_gambia <- data_gambia %>% mutate(
  HHExp_NoFood_CRD_1M = HHExp_NoFood_CRD_6M/6
)
# No Food Assistance
GiftAid_NFood6 <-  names(data_gambia)[which(str_detect(string = names(data_gambia), pattern = regex("^[[:alpha:]]+._GiftAid_6M")) == "TRUE")]
data_gambia$HHExp_NoFood_GiftAid_6M <- rowSums(data_gambia[,c(GiftAid_NFood6)], na.rm = TRUE)
data_gambia <- data_gambia %>% mutate(
  HHExp_NoFood_GiftAid_1M = HHExp_NoFood_GiftAid_6M/6
)
# # Food Own Production
# Own <-  names(data_liberia)[which(str_detect(string = names(data_liberia), pattern = regex("^[[:alpha:]]+._Own_6M")) == "TRUE")]
# data_liberia$HHExp_Food_Own_1M <- rowSums(data_liberia[,c(Own)], na.rm = TRUE)
# food expenditure
# No Food Expenditure 30 days
data_gambia <- data_gambia %>% mutate(
  nonfood_monthly = HHExpNoFood_MN_1M + HHExp_NoFood_CRD_1M +HHExp_NoFood_GiftAid_1M
)

data_gambia  <- data_gambia  %>%  mutate(HHExp_total = food_monthly + nonfood_monthly)
data_gambia$HHExp_total[data_gambia$HHExp_total == 0] <- 1
data_gambia <- data_gambia %>% mutate(
  FoodExp_share = food_monthly/HHExp_total
)

# data_liberia$FoodExp_share[data_liberia$HHExp_total == 0] <- 1

data_gambia <- data_gambia %>% mutate(Foodexp_4pt = case_when(
  FoodExp_share >= 0 &  FoodExp_share <0.5 ~ 1,
  FoodExp_share >= 0.5 & FoodExp_share < 0.65 ~ 2,
  FoodExp_share >= 0.65 & FoodExp_share < 0.75 ~ 3,
  FoodExp_share >= 0.75 & FoodExp_share <= 1.0 ~ 4  ))

data_gambia <- data_gambia %>% mutate(Foodexp_4pt_class = case_when(
  Foodexp_4pt == 1 ~ "less than 50%",
  Foodexp_4pt == 2 ~ "Between 50 and 65%",
  Foodexp_4pt == 3 ~ "Between 65 and 75%",
  Foodexp_4pt == 4 ~ "Greater than 75%"))

var_label(data_gambia$Foodexp_4pt) <- "Food Expenditure categories"
val_labels(data_gambia$Foodexp_4pt) <- c("less than 50%" = 1, "Between 50 and 65%" = 2, "Between 65 and 75%" = 3, "GGreater than 75%" = 4)
# View(data_liberia[which(is.na(data_liberia$FoodExp_share)),])


  
  # cari --------------------------------------------------------------------
  
  # https://www.wfp.org/publications/consolidated-approach-reporting-indicators-food-security-cari-guidelines
  # creates variable for FCS and rCSI CARI reclassification
data_gambia <- data_gambia %>% mutate(FCS_rCSI_CARI = case_when(FCSCat28 == "Acceptable" & rCSI < 4 ~ 1, FCSCat28 == "Acceptable" & rCSI >= 4 ~ 2, FCSCat28 == "Borderline" ~ 3, FCSCat28 == "Poor" ~ 4))
# creates variable for LCSI CARI reclassification
data_gambia <- data_gambia %>% mutate(LCSI_CARI = case_when(LhCSICat == "NoStrategies" ~ 1, LhCSICat == "StressStrategies" ~ 2, LhCSICat == "CrisisStrategies" ~ 3, LhCSICat == "EmergencyStrategies" ~ 4))
# creates variable for FES CARI reclassification
# data_liberia %>% mutate(FES_CARI = case_when(FES_Classification == "<50%" ~ 1, FES_Classification == "50%-65%" ~ 2, FES_Classification == "65%-75%" ~ 3, FES_Classification == ">75%" ~ 4))
# The variable above FES_CARI correspond to Foodexp_4pt

# creates variables for CARI Current Status (CS) and CARI Coping Capacity (CC)
data_gambia$CS_CARI <- data_gambia$FCS_rCSI_CARI
data_gambia$CC_CARI <- (data_gambia$LCSI_CARI + data_gambia$Foodexp_4pt)/2
# creates variables for CARI Score
data_gambia$CARI <- (data_gambia$CS_CARI + data_gambia$CC_CARI)/2
# creates variables for CARI Score Classification
data_gambia <- data_gambia %>% mutate(CARI_Classification = case_when(CARI >= 3.5 ~ "Severely Food Insecure", CARI < 3.5 & CARI >= 2.5 ~ "Moderately Food Insecure", CARI < 2.5 & CARI >= 1.5 ~ "Marginally Food Secure", CARI < 1.5 & CARI >= 0 ~ "Food Secure"))

gCARI <- funModeling::freq(data_gambia, "CARI_Classification")
gCARI
write_xlsx(gCARI, "CARItotal.xlsx")
gFES <- funModeling::freq(data_gambia, "Foodexp_4pt_class")
write_xlsx(gFES, "FEStotal.xlsx")



# Cross Tab and result by Amin2 Level -------------------------------------

# FCS
CH_FCSCat_table_wide <<- data_gambia %>%
  drop_na(FCSCat28) %>%
  group_by(ADMIN1Name) %>%
  count(FCSCat28,wt=Weight) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>%
  pivot_wider(names_from = FCSCat28,
              values_from = perc,
              values_fill = list(perc = 0)) %>%
  mutate_if(is.numeric, round, 1)

oldnamefcg <- c("Acceptable","Borderline","Poor" )
newnamefcg <- c("FCG_Acceptable", "FCG_Borderline","FCG_Poor")
data.table::setnames(CH_FCSCat_table_wide, old = oldnamefcg,new = newnamefcg)

# HDDS
# , wt = {{weightvar}}
CH_HDDS_table_wide  <<- data_gambia %>%
  drop_na(HDDS_CH) %>%
  group_by(ADMIN1Name) %>%
  count(HDDS_CH,wt=Weight) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>%
  pivot_wider(names_from = HDDS_CH,
              values_from = perc,
              values_fill = list(perc = 0)) %>%
  mutate_if(is.numeric, round, 1) 

oldnamehdds <- c("Phase1","Phase2","Phase3","Phase4", "Phase5" )
newnamehdds <- c("HDDS_Phase1", "HDDS_Phase2", "HDDS_Phase3", "HDDS_Phase4", "HDDS_Phase5")
data.table::setnames(CH_HDDS_table_wide, old = oldnamehdds,new = newnamehdds)


# rCSI
# , wt = {{weightvar}}
CH_rCSI_table_wide <- data_gambia %>%
  group_by(ADMIN1Name) %>%
  drop_na(rCSI_CH) %>%
  count(rCSI_CH,wt=Weight) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>%
  pivot_wider(names_from = rCSI_CH,
              values_from = perc,
              values_fill = list(perc = 0)) %>%
  mutate_if(is.numeric, round, 1)

oldnamercsi <- c("Phase1","Phase2","Phase3")
newnamercsi <- c("rCSI_Phase1","rCSI_Phase2","rCSI_Phase3")
data.table::setnames(CH_rCSI_table_wide, old = oldnamercsi,new = newnamercsi)


# LCS 
CH_LhCSICat_table_wide <- data_gambia %>%
  drop_na(LhCSICat) %>%
  group_by(ADMIN1Name) %>%
  count(LhCSICat,wt=Weight) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>%
  pivot_wider(names_from = LhCSICat,
              values_from = perc,
              values_fill = list(perc = 0)) %>%
  mutate_if(is.numeric, round, 1)

oldnamelcs <- c("CrisisStrategies","EmergencyStrategies","NoStrategies",     
                "StressStrategies")
newnamelcs<- c("LhHCSCat_CrisisStategies", "LhHCSCat_EmergencyStrategies","LhHCSCat_NoStrategies",
               "LhHCSCat_StressStrategies")

data.table::setnames(CH_LhCSICat_table_wide, old = oldnamelcs,new = newnamelcs)


# HHS
CH_HHS_tabe_wide <- data_gambia %>% 
  group_by(ADMIN1Name) %>%
  drop_na(HHhS_CH) %>%
  count(HHhS_CH,wt=Weight) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>%
  pivot_wider(names_from = HHhS_CH,
              values_from = perc,
              values_fill = list(perc = 0)) %>%
  mutate_if(is.numeric, round, 1) 

oldnamehhs <- c("Phase1","Phase2","Phase3","Phase4", "Phase5" )
newnamehhs <- c("HHS_Phase1", "HHS_Phase2", "HHS_Phase3", "HHS_Phase4", "HHS_Phase5")

data.table::setnames(CH_HHS_tabe_wide, old = oldnamehhs,new = newnamehhs)



# Jointure ----------------------------------------------------------------

matrice <- CH_FCSCat_table_wide %>%  left_join(
  CH_HDDS_table_wide, by = c("ADMIN1Name", "ADMIN2Name")
)

matrice_final <- purrr::reduce(list(CH_FCSCat_table_wide,CH_HDDS_table_wide,
                                    CH_HHS_tabe_wide,CH_LhCSICat_table_wide,
                                    CH_rCSI_table_wide), dplyr::left_join, by = c("ADMIN1Name", "ADMIN2Name"))

matrice_final <- matrice_final %>% mutate(
  Z1_DPME_C=NA, Z1_DPME_pop_C=NA, Z1_DS_C=NA, Z1_Pop_DS_C=NA, Z1_DPME_Pr=NA, Z1_Pop_DPME_Pr=NA, Z1_DS_Pr=NA, Z1_pop_DS_Pr=NA, Z2_DPME_C=NA, Z2_DPME_pop_C=NA, Z2_DS_C=NA, Z2_Pop_DS_C=NA, Z2_DPME_Pr=NA, Z2_Pop_DPME_Pr=NA, Z2_DS_Pr=NA, Z2_pop_DS_Pr=NA, Z3_DPME_C=NA, Z3_DPME_pop_C=NA, Z3_DS_C=NA, Z3_Pop_DS_C=NA, Z3_DPME_Pr=NA, Z3_Pop_DPME_Pr=NA, Z3_DS_Pr=NA, Z3_pop_DS_Pr=NA, Z4_DPME_C=NA, Z4_DPME_pop_C=NA, Z4_DS_C=NA, Z4_Pop_DS_C=NA, Z4_DPME_Pr=NA, Z4_Pop_DPME_Pr=NA, Z4_DS_Pr=NA, Z4_pop_DS_Pr=NA, Proxy_cal=NA, MAG_pt=NA, IPC_AMN_curt=NA, MAG_Pharv=NA, MAG_Soud=NA, IPC_AMN_prjt=NA, IMC=NA, MUAC=NA, TBM=NA, TMM5=NA
  
)

matrice_final <- matrice_final %>% mutate(
  Geocode = NA, Population = NA
) 

matrice_final <- matrice_final %>% relocate(c(Geocode,Population), .after = ADMIN2Name)

test <- data.frame(
  stringsAsFactors = FALSE,
  check.names = FALSE,
  Z1_DPME_C = c("Z1_DPME_pop_C",
                "Z1_DS_C","Z1_Pop_DS_C","Z1_DPME_Pr",
                "Z1_Pop_DPME_Pr","Z1_DS_Pr","Z1_pop_DS_Pr",
                "Z2_DPME_C","Z2_DPME_pop_C","Z2_DS_C","Z2_Pop_DS_C",
                "Z2_DPME_Pr","Z2_Pop_DPME_Pr","Z2_DS_Pr",
                "Z2_pop_DS_Pr","Z3_DPME_C","Z3_DPME_pop_C","Z3_DS_C",
                "Z3_Pop_DS_C","Z3_DPME_Pr","Z3_Pop_DPME_Pr",
                "Z3_DS_Pr","Z3_pop_DS_Pr","Z4_DPME_C",
                "Z4_DPME_pop_C","Z4_DS_C","Z4_Pop_DS_C","Z4_DPME_Pr",
                "Z4_Pop_DPME_Pr","Z4_DS_Pr","Z4_pop_DS_Pr",
                "Proxy_cal","MAG_pt","IPC_AMN_curt","MAG_Pharv",
                "MAG_Soud","IPC_AMN_prjt","IMC","MUAC","TBM",
                "TMM5"),
  `'` = c("'","'","'",
          "'","'","'","'","'","'","'","'","'","'",
          "'","'","'","'","'","'","'","'","'","'",
          "'","'","'","'","'","'","'","'","'",
          "'","'","'","'","'","'","'","'","'")
)
# dépense -----------------------------------------------------------------

test2 <- test %>% mutate(
  var3 = paste(var1, var2, sep = "=")
)

writexl::write_xlsx(test2, "test2.xlsx")
# Expenditure
CH_Expend_tabe_wide <- data_gambia %>% 
  group_by(ADMIN1Name) %>%
  drop_na(CARI_Classification) %>%
  count(CARI_Classification,,wt=Weight) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>%
  pivot_wider(names_from = CARI_Classification,
              values_from = perc,
              values_fill = list(perc = 0)) %>%
  mutate_if(is.numeric, round, 1) 

CH_Expend_tabe_wide <- CH_Expend_tabe_wide %>% relocate(
  `less than 50%`, .after = ADMIN2Name
)

CH_Expend_tabe_wide <- data_gambia %>% 
  group_by(ADMIN1Name) %>%
  drop_na(CARI_Classification) %>%
  count(CARI_Classification,,wt=Weight) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>%
  pivot_wider(names_from = CARI_Classification,
              values_from = perc,
              values_fill = list(perc = 0)) %>%
  mutate_if(is.numeric, round, 1) %>% 

CH_Expend_tabe_wide <- CH_Expend_tabe_wide %>% relocate(
  `less than 50%`, .after = ADMIN1Name
)
# Final Join --------------------------------------------------------------
CH_FCSCat_table_wide <- CH_FCSCat_table_wide[-17,]
CH_HDDS_table_wide <- CH_HDDS_table_wide[-17,]
CH_HHS_tabe_wide <- CH_HHS_tabe_wide[-17,]
CH_rCSI_table_wide <- CH_rCSI_table_wide[-17,]
CH_LhCSICat_table_wide <- CH_LhCSICat_table_wide[-17,]

oldname <- c("Community", "TeamLeaderID")
newnmae <- c("Admin4", "supervisor")

data.table::setnames(data_gambia, old = oldname, new = newnmae)


# Save result -------------------------------------------------------------

write_xlsx(CH_FCSCat_table_wide, "FCSAdmin1.xlsx")
write_xlsx(CH_HDDS_table_wide, "HDDSAdmin1.xlsx")
write_xlsx(CH_HHS_tabe_wide, "HHSAdmin1.xlsx")
write_xlsx(CH_rCSI_table_wide, "rCSIAdmin1.xlsx")
write_xlsx(CH_LhCSICat_table_wide, "LcsAdmin1.xlsx")
write_xlsx(CH_Expend_tabe_wide, "ExpenditureAdmin1 CARI Final.xlsx")
write_sav(data_gambia, "Gambia_weight_and_calculation.sav")

libele <- libele %>% 
  mutate( v3 = paste(V1, V2, sep = "="))
write_xlsx(libele, "libele.xlsx")
modalite <- c(1 = male, 
              2= Female)
modalite <- str_replace_all(modalite, pattern = '/""', replacement = "")
modalite

t1 <- data_gambia %>% tabyl(
  ADMIN1Name, FCSCat28
) %>% adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 1)
write_xlsx(t1, "test.xlsx")


# Tableau croisé spécifiquement pour la Gambie ----------------------------

# sexe et statut urbain ou rural du ménage
tabhharea <- data_gambia %>% tabyl(
  CARI_Classification, RESPStatut
) %>% adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 1)

write_xlsx(tabhharea, "statutCar.xlsx")

tabhharea <- data_gambia %>% tab_cells(
  CARI_Classification
) %>% tab_cols(Area,RESPSex) %>% 
  tab_stat_cpct() %>% tab_pivot()

data_liberia <- data_liberia %>% mutate(
  HHsizeCat  = case_when(
    HHSizeCalc < 5 ~ "Small < 5 members",
    HHSizeCalc >= 5 & HHSizeCalc <= 7 ~ "medium 5è7 members",
    HHSizeCalc > 7 ~ "big > 7 members"
  )
)
write_sav(data_liberia, "Gambia.sav")

data_Gambia <- read_sav("C:\\Users\\USER MSI\\Documents\\WFP\\CH octobre Novembre 2022\\Gambia/Updated Cleaned national food security survey 2022data.sav")
weight <- readxl::read_xlsx("C:\\Users\\USER MSI\\Documents\\WFP\\CH octobre Novembre 2022\\Gambia/Weight Gambia.xlsx")
setdiff(weight$EA, data_Gambia$ADMIN4Name)
setdiff(data_Gambia$ADMIN4Name, weight$EA)
class(data_Gambia$ADMIN4Name)
class(weight$EA)
data_Gambia <- as.data.frame(data_Gambia)
data_Gambia$ADMIN4Name <- as.double(data_Gambia$ADMIN4Name)
weight$EA <- as.character(weight$EA)
table(data_Gambia$ADMIN4Name)
data_Gambia <- data_Gambia %>% left_join(weight, by = c("ADMIN4Name" = "EA"))
class(data_Gambia$Weight)
data_test <- data_Gambia %>% filter(
  RESPName == "Samba Tamba")

data_Gambia <- data_Gambia %>% to_factor()
data_Gambia <- data_Gambia[-which(data_Gambia$RESPConsent == "No"),]
data_Gambia <- data_Gambia[-c(data_Gambia$RESPName == "Daddy Jallow" & data_Gambia$RESPRelationHHH == "Head of household"),]
data_Gambia <- data_Gambia[-c(data_Gambia$RESPName == "Samba Tamba" ),]
write_sav(data_Gambia, "C:\\Users\\USER MSI\\Documents\\WFP\\CH octobre Novembre 2022\\Gambia/Gambia_weight.sav")
# End