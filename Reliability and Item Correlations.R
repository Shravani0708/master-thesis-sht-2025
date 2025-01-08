install.packages("psych")    # For Cronbach's Alpha
install.packages("semTools") # For AVE and Discriminant Validity (with lavaan)
install.packages("moments")  # For skewness and kurtosis functions

# Load libraries
library(psych)
library(semTools)
library(lavaan)
library(moments)

# Load raw item data
my_data <- read.csv("C:\\Users\\ADMIN\\OneDrive - Universität des Saarlandes\\SoSe 24\\Master Thesis\\Data analysis\\Results_cleaned_wocomposite_forR.csv")

# Compute Cronbach's Alpha for each construct
alpha(my_data[, c("INSPT1", "INSPT2", "INSPT3", "INSPT4", "INSPT5")])  # Institutional Support
alpha(my_data[, c("LPT1", "LPT2", "LPT3", "LPT4", "LPT5")])  # Lesson Preparation Time
alpha(my_data[, c("TECH1", "TECH2", "TECH3", "TECH4", "TECH5")]) # Access and use of tech
alpha(my_data[, c("SEFF1", "SEFF2", "SEFF3", "SEFF4", "SEFF5")]) # Self-efficacy
alpha(my_data[, c("TLX1", "TLX2", "TLX3", "TLX4", "TLX5", "TLX6")]) # Orchestration Load
alpha(my_data[, c("ATT1", "ATT2", "ATT3", "ATT4", "ATT5")]) # Teacher attitudes
alpha(my_data[, c("BEH1", "BEH2", "BEH3", "BEH4", "BEH5")]) #Teacher behaviors

# Inter-Item Correlations for Institutional Support
lowerCor(my_data[, c("INSPT1", "INSPT2", "INSPT3", "INSPT4", "INSPT5")], use = "pairwise.complete.obs", cor="cor")

# Inter-Item Correlations for Lesson Preparation Time
lowerCor(my_data[, c("LPT2", "LPT3", "LPT4", "LPT5")], use = "pairwise.complete.obs", cor="cor")

# Inter-Item Correlations for Access and Use of Technology
lowerCor(my_data[, c("TECH1", "TECH2", "TECH3", "TECH4", "TECH5")], use = "pairwise.complete.obs", cor="cor")

# Inter-Item Correlations for Self-Efficacy
lowerCor(my_data[, c("SEFF1", "SEFF2", "SEFF3", "SEFF4", "SEFF5")], use = "pairwise.complete.obs", cor="cor")

# Inter-Item Correlations for Orchestration Load
lowerCor(my_data[, c("TLX1", "TLX2", "TLX3", "TLX4", "TLX5", "TLX6")], use = "pairwise.complete.obs", cor="cor")

# Inter-Item Correlations for Teacher Attitudes
lowerCor(my_data[, c("ATT1", "ATT2", "ATT3", "ATT4", "ATT5")], use = "pairwise.complete.obs", cor="cor")

# Inter-Item Correlations for Teacher Behaviors
lowerCor(my_data[, c("BEH1", "BEH2", "BEH3", "BEH4", "BEH5")], use = "pairwise.complete.obs", cor="cor")


library(psych)
# Item-total correlations

# 1. Institutional Support
my_data$IS_Total <- rowSums(my_data[, c("INSPT1", "INSPT2", "INSPT3", "INSPT4", "INSPT5")])

# Compute item-total correlations manually
correlations <- sapply(c("INSPT1", "INSPT2", "INSPT3", "INSPT4", "INSPT5"), function(item) {
  cor(my_data[[item]], my_data$IS_Total - my_data[[item]], use = "pairwise.complete.obs")
})

# Print item-total correlations
print(correlations)

# 2. Lesson preparation time
my_data$IS_Total <- rowSums(my_data[, c("LPT2", "LPT3", "LPT4", "LPT5")])

# Compute item-total correlations manually
correlations <- sapply(c("LPT2", "LPT3", "LPT4", "LPT5"), function(item) {
  cor(my_data[[item]], my_data$IS_Total - my_data[[item]], use = "pairwise.complete.obs")
})

# Print item-total correlations
print(correlations)

# 3. Access and use of technology
my_data$IS_Total <- rowSums(my_data[, c("TECH1", "TECH2", "TECH3", "TECH4", "TECH5")])

# Compute item-total correlations manually
correlations <- sapply(c("TECH1", "TECH2", "TECH3", "TECH4", "TECH5"), function(item) {
  cor(my_data[[item]], my_data$IS_Total - my_data[[item]], use = "pairwise.complete.obs")
})

# Print item-total correlations
print(correlations)

# 4. Teacher Self-efficacy
my_data$IS_Total <- rowSums(my_data[, c("SEFF1", "SEFF2", "SEFF3", "SEFF4", "SEFF5")])

# Compute item-total correlations manually
correlations <- sapply(c("SEFF1", "SEFF2", "SEFF3", "SEFF4", "SEFF5"), function(item) {
  cor(my_data[[item]], my_data$IS_Total - my_data[[item]], use = "pairwise.complete.obs")
})

# Print item-total correlations
print(correlations)

# 5. Orchestration Load
my_data$IS_Total <- rowSums(my_data[, c("TLX1", "TLX2", "TLX3", "TLX4", "TLX5", "TLX6")])

# Compute item-total correlations manually
correlations <- sapply(c("TLX1", "TLX2", "TLX3", "TLX4", "TLX5", "TLX6"), function(item) {
  cor(my_data[[item]], my_data$IS_Total - my_data[[item]], use = "pairwise.complete.obs")
})

# Print item-total correlations
print(correlations)

# 6. Teacher Attitudes
my_data$IS_Total <- rowSums(my_data[, c("ATT1", "ATT2", "ATT3", "ATT4", "ATT5")])

# Compute item-total correlations manually
correlations <- sapply(c("ATT1", "ATT2", "ATT3", "ATT4", "ATT5"), function(item) {
  cor(my_data[[item]], my_data$IS_Total - my_data[[item]], use = "pairwise.complete.obs")
})

# Print item-total correlations
print(correlations)

# 6. Teacher Behavior
my_data$IS_Total <- rowSums(my_data[, c("BEH1", "BEH2", "BEH3", "BEH4", "BEH5")])

# Compute item-total correlations manually
correlations <- sapply(c("BEH1", "BEH2", "BEH3", "BEH4", "BEH5"), function(item) {
  cor(my_data[[item]], my_data$IS_Total - my_data[[item]], use = "pairwise.complete.obs")
})

# Print item-total correlations
print(correlations)
