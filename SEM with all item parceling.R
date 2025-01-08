# Load required libraries
library(lavaan)
library(psych)
# Load the dataset
my_data <- read.csv("C:\\Users\\ADMIN\\OneDrive\\Documents\\parcels_my_data.csv")
head(my_data)

# Load the raw item-level data saved from SPSS
my_data <- read.csv("C:\\Users\\ADMIN\\OneDrive - Universität des Saarlandes\\SoSe 24\\Master Thesis\\Data analysis\\Submission files\\Results_cleaned_wocomposite_forR.csv", stringsAsFactors = FALSE)

# check header of raw dataset
head(my_data)

# Compute parcels by averaging item scores for each construct
my_data$INSPT_COMP <- rowMeans(my_data[, c("INSPT1", "INSPT2", "INSPT3", "INSPT4", "INSPT5")], na.rm = TRUE)
my_data$LPT_COMP <- rowMeans(my_data[, c("LPT2", "LPT3", "LPT4", "LPT5")], na.rm = TRUE)
my_data$TECH_COMP <- rowMeans(my_data[, c("TECH1", "TECH2", "TECH3", "TECH4", "TECH5")], na.rm = TRUE)
my_data$SEFF_COMP <- rowMeans(my_data[, c("SEFF1", "SEFF2", "SEFF3", "SEFF4", "SEFF5")], na.rm = TRUE)
my_data$TLX_COMP <- rowMeans(my_data[, c("TLX1", "TLX2", "TLX3", "TLX5")], na.rm = TRUE)
my_data$ATT_COMP <- rowMeans(my_data[, c("ATT1", "ATT2", "ATT3", "ATT5")], na.rm = TRUE)
my_data$BEH_COMP <- rowMeans(my_data[, c("BEH1", "BEH2", "BEH3", "BEH4", "BEH5")], na.rm = TRUE)

head(my_data)

set.seed(123)

# Define the SEM model with parcels 
model <- '
  # Measurement model: Define latent variables using all item parcels
  Institutional_Support =~ 1*INSPT_COMP
  Lesson_Preparation_Time =~ 1*LPT_COMP
  Access_Use_Technology =~ 1*TECH_COMP
  Self_Efficacy =~ 1*SEFF_COMP
  Orchestration_Load =~ 1*TLX_COMP
  Teacher_Attitudes =~ 1*ATT_COMP
  Teacher_Behaviors =~ 1*BEH_COMP

  # Structural model
  Orchestration_Load ~ Institutional_Support + Lesson_Preparation_Time + Access_Use_Technology + Self_Efficacy + EXP1
  Teacher_Attitudes ~ Orchestration_Load
  Teacher_Behaviors ~ Orchestration_Load

'

# Fit the SEM model using ML estimator and bootstrap standard errors
fit_ml <- sem(
  model,
  data = my_data,
  estimator = "ML",
  se = "bootstrap",    # Use bootstrap standard errors
  bootstrap = 1000     # Number of bootstrap samples
)

# Summary of results with fit measures, standardized estimates, and R-squared
summary(fit_ml, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
