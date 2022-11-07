
# Import Package ----------------------------------------------------------

library(dplyr)
library(haven)
library(expss)
library(openxlsx)
library(writexl)
library(labelled)
library(readxl)

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

test9 <- dataset_GMB %>% 
  cross_rpct(ADMIN1Name,list(FCSCat28,HDDS_CH,HHhS_CH,LhCSICat,rCSI_CH,shock_6M,shock_1,
                             Foodexp_4pt), total_row_position = "none",
             weight = Weight) 


test10 <- as.data.frame(test9)
test10 <- test10 %>% mutate_if(
  is.numeric, round, 1
)


# Price data --------------------------------------------------------------

path <- "GAMIS 2022 Market Price by region.xlsx"


price_data <- path %>%
  excel_sheets() %>% 
  map_df(read_excel,
         path = path)

cereals <- c("Maize", "Millet","Sorghum", "Broken Rice","Beans","Undecorticated G/nut","	
Decorticated G/Nut")

price_data <- price_data %>% filter(
  CEREALS %in% cereals
)

price_data2yearly <- price_data %>% select(ADMIN1Name, CEREALS,
                                           `% Changes`) %>% pivot_wider(
    names_from = CEREALS, values_from = `% Changes`
) %>% mutate_if(is.numeric, round, 2)

names(price_data2yearly)[2:7] <- paste0("45_annual_variation_",names(price_data2yearly)[2:7])

price_data25year <-  price_data %>% select(ADMIN1Name, CEREALS,
                                                               `% Diff. 2022 compared to 5 years avg`) %>% pivot_wider(
                                                                 names_from = CEREALS, values_from = `% Diff. 2022 compared to 5 years avg`) %>% 
  mutate_if(is.numeric, round, 2)

names(price_data25year)[2:7] <- paste0("46_Five_year_variation_",names(price_data25year)[2:7])

final_price_data <- price_data2yearly %>% left_join(price_data25year, by = "ADMIN1Name")

matrice <- read_xlsx("Matrice_intermediaire.xlsx")
names(matrice)[33:50] <- paste0("02_",names(matrice)[33:50])
names(matrice)[51:54] <- paste0("47_",names(matrice)[51:54])

setdiff(matrice$ADMIN1Name, final_price_data$ADMIN1Name)
setdiff(final_price_data$ADMIN1Name, matrice$ADMIN1Name)

# production
path2 <- "2022 Regional Production data Forecast.xlsx"


production_data <- path2 %>%
  excel_sheets() %>% 
  map_df(read_excel,
         path = path2)

production_data <- production_data %>% select(
  -c(`5yr.Aver`)
)

production_datayearly <- production_data %>% select(c(- `% Change 2022-5yr.avg`)) %>%  pivot_wider(
  names_from = Years , values_from = `% Change 2022-2021`
)

# join
matrice_final <- matrice %>% left_join(final_price_data, by = "ADMIN1Name")
write_xlsx(matrice_final, "matrice_final.xlsx")

names(production_datayearly)[2:10] <- paste("15_difference Annual production",names(production_datayearly)[2:10])
# 5 year
production_datafiveyear <- production_data %>% select(c(- `% Change 2022-2021`)) %>%  pivot_wider(
  names_from = Years , values_from = `% Change 2022-5yr.avg`
)


names(production_datafiveyear)[2:10] <- paste("16_difference five year production",names(production_datafiveyear)[2:10])
production_datayearly <- production_datayearly %>% select(-c(`15_ Total Cash`))
production_datafiveyear <- production_datafiveyear %>% select(-c(`16_ Total Cash`))

final_production_data <- production_datayearly %>% left_join(production_datafiveyear, by = "ADMIN1Name")
# join production and matrice
setdiff(matrice$ADMIN1Name,final_production_data$ADMIN1Name)
setdiff(final_production_data$ADMIN1Name, matrice$ADMIN1Name)

matrice_final <- matrice %>% left_join(final_production_data, by = "ADMIN1Name" )
write_xlsx(matrice_final, "Matrice_final_gmb.xlsx")

# trade
trade <- read_xlsx("term_of_trade.xlsx")

trade <- trade %>% mutate(
  `Variations TOT` = 100 * `Variations TOT`
) %>% mutate_if(is.numeric, round, 1)

trade_final <- trade %>% pivot_wider(
  names_from = Procuct, values_from = `Variations TOT`
)

names(trade_final)[2:4] <- paste0("48_Terms of trade ",names(trade_final)[2:4])

matrice_final <- matrice_final %>% left_join(trade_final, by = "ADMIN1Name")
write_xlsx(matrice_final, "Matrice_final_gmb.xlsx")

# saving ------------------------------------------------------------------
wb <- createWorkbook()
sh <- addWorksheet(wb, "matrice")

xl_write(test10, wb, sh)

saveWorkbook(wb, "Matrice_intermediaire_Gambia.xlsx", overwrite = TRUE)

# end of syntaxe ----------------------------------------------------------


