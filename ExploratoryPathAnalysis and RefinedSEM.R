# Load lavaan package
library(lavaan)

# Load the dataset
my_data <- read.csv("C:\\Users\\ADMIN\\OneDrive\\Documents\\parcels_my_data.csv")

# Check the structure of the data
head(my_data)

# Define path analysis models for each predictor variable

# Example 1: Institutional Support
model_IS <- '
  TLX_COMP ~ INSPT_COMP
  ATT_COMP ~ TLX_COMP
  BEH_COMP ~ TLX_COMP
'

# Example 2: Lesson Preparation Time
model_LPT <- '
  TLX_COMP ~ LPT_COMP
  ATT_COMP ~ TLX_COMP
  BEH_COMP ~ TLX_COMP
'

# Example 3: Access and Use of Technology
model_TECH <- '
  TLX_COMP ~ TECH_COMP
  ATT_COMP ~ TLX_COMP
  BEH_COMP ~ TLX_COMP
'

# Example 4: Self-Efficacy
model_SEFF <- '
  TLX_COMP ~ SEFF_COMP
  ATT_COMP ~ TLX_COMP
  BEH_COMP ~ TLX_COMP
'

# Example 5: Experience with SHT
model_EXP <- '
  TLX_COMP ~ EXP1
  ATT_COMP ~ TLX_COMP
  BEH_COMP ~ TLX_COMP
'
set.seed(123)
# Fit each path analysis model using ML estimator
fit_IS <- sem(model_IS, data = my_data, estimator = "ML", se = "bootstrap", bootstrap = 1000)
fit_LPT <- sem(model_LPT, data = my_data, estimator = "ML", se = "bootstrap", bootstrap = 1000)
fit_TECH <- sem(model_TECH, data = my_data, estimator = "ML", se = "bootstrap", bootstrap = 1000)
fit_SEFF <- sem(model_SEFF, data = my_data, estimator = "ML", se = "bootstrap", bootstrap = 1000)
fit_EXP <- sem(model_EXP, data = my_data, estimator = "ML", se = "bootstrap", bootstrap = 1000)

# View summaries for each path analysis model
summary(fit_IS, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(fit_LPT, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(fit_TECH, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(fit_SEFF, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(fit_EXP, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# SEM with removed/selected variables
model_balanced_SEM <- '
  # Measurement model
  Institutional_Support =~ 1*INSPT_COMP
  Lesson_Preparation_Time =~ 1*LPT_COMP
  Orchestration_Load =~ 1*TLX_COMP
  Teacher_Attitudes =~ 1*ATT_COMP
  Teacher_Behaviors =~ 1*BEH_COMP

  # Structural Model
  Orchestration_Load ~ Institutional_Support + Lesson_Preparation_Time + EXP1
  Teacher_Attitudes ~ Orchestration_Load
  Teacher_Behaviors ~ Orchestration_Load
  '

fit_balanced <- sem(model_balanced_SEM, data = my_data, estimator = "ML", se = "bootstrap", bootstrap = 1000)
summary(fit_balanced, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
