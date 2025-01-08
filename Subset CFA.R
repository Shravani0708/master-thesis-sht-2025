rm(list = ls())  # Clear workspace
# CFA with subsets of constructs due to small sample size  
# Load required libraries

library(lavaan)
library(semTools)

# Load raw item data
my_data <- read.csv("C:\\Users\\ADMIN\\OneDrive - Universität des Saarlandes\\SoSe 24\\Master Thesis\\Data analysis\\Results_cleaned_wocomposite_forR.csv")
head(my_data)

# Split CFA Model 1 (Institutional Support and Lesson Preparation Time)
model_1 <- '
  Institutional_Support =~ INSPT1 + INSPT2 + INSPT3 + INSPT4 + INSPT5
  Lesson_Preparation_Time =~ LPT2 + LPT3 + LPT4 + LPT5
'
fit_1 <- cfa(model_1, data = my_data)
summary(fit_1, fit.measures = TRUE, standardized = TRUE)
semTools::reliability (fit_1)

# Split CFA Model 2 (Access to Tech and Self-Efficacy)
model_2 <- '
  Access_Use_Technology =~ TECH1 + TECH2 + TECH3 + TECH4 + TECH5
  Self_Efficacy =~ SEFF1 + SEFF2 + SEFF3 + SEFF4 + SEFF5
'
fit_2 <- cfa(model_2, data = my_data)
summary(fit_2, fit.measures = TRUE, standardized = TRUE)
semTools::reliability (fit_2)

# Split CFA Model 3 (OL, attitudes and behaviors)
model_3 <- '
  Orchestration_Load =~ TLX1 + TLX2 + TLX3 + TLX4 + TLX5 + TLX6
  Teacher_Attitudes =~ ATT1 + ATT2 + ATT3 + ATT4 + ATT5
  Teacher_Behaviors =~ BEH1 + BEH2 + BEH3 + BEH4 + BEH5
'
fit_3 <- cfa(model_3, data = my_data)
summary(fit_3, fit.measures = TRUE, standardized = TRUE)
semTools::reliability (fit_3)