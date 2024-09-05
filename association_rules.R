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

weight <-working_data$weight

working_data = working_data %>% 
   dplyr::select(c(gtsummary::starts_with("D1_"),gtsummary::starts_with("D_")))
#171 variables

working_data = working_data %>% janitor::remove_constant()
#165 variables