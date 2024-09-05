library(tidyverse) # for data manipulation
library(arules) # for association analysis
library(arulesViz) # for rules vizualisation

## Import data and some data wrangling


G5_Sahel_2018_2023_Mali_data <- haven::read_dta("data/G5_Sahel_2018_2023_Mali_enhanced_DICHOTOMIZED.dta")
#View(G5_Sahel_2018_2023_Mali_data)
G5_Sahel_2018_2023_Mali_data = G5_Sahel_2018_2023_Mali_data %>% 
  labelled::to_factor()
#View(G5_Sahel_2018_2023_Mali_data)

#Create ID by using admin2PcodN, wave and generate values between 1 to number rows (123,382)
G5_Sahel_2018_2023_Mali_data = G5_Sahel_2018_2023_Mali_data %>%
  mutate(ID = paste0(admin2PcodN,"_",wave,"_",1:nrow(G5_Sahel_2018_2023_Mali_data)))

working_data = G5_Sahel_2018_2023_Mali_data

#converted into a set of transactions where each row (ID) represents a transaction
working_data <- working_data %>% tibble::column_to_rownames(var="ID")

working_data = working_data %>% 
  dplyr::select(c(gtsummary::starts_with("D1_"),gtsummary::starts_with("D_")))
#171 variables

working_data = working_data %>% janitor::remove_constant()
#165 variables

vars = working_data %>% names()
working_data <- working_data %>%
  mutate_at(vars, as.numeric)
working_data <- working_data %>% 
  dplyr::mutate_at(vars, ~labelled::labelled(., labels = c(
    `Yes` = 1,
    `No` = 0
  )))

working_data <- working_data %>% labelled::to_factor()


# "D1_zs_onion_inte","D1_zs_milkpowder_inte"
# "D1_zs_milkpowder_freq","D1_zs_sugar_freq"     
# "D1_zs_sugar_spell","D1_R_drought"
working_data = working_data %>% 
  dplyr::select(-c(12, 14, 27, 29, 42, 44))
# converted into a set of transactions where each row represents a transaction and each column is translated into items
trans <- arules::transactions(working_data)
trans
# transactions in sparse format with
# 123382 transactions (rows) and
# 318 items (columns)
colnames(trans)


#We use the APRIORI algorithm
minsup = 0.75
minconf = 0.9
rules <- apriori(trans, parameter = list(support = minsup, confidence = minconf))


arulesViz::ruleExplorer(rules)
