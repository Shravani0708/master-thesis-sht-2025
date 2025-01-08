# Compute square root of AVE
# AVE has already obtained through std. factor loadings in previous analyses 

# Load raw item data
my_data <- read.csv("C:\\Users\\ADMIN\\OneDrive - Universität des Saarlandes\\SoSe 24\\Master Thesis\\Data analysis\\Results_cleaned_wocomposite_forR.csv")

head(my_data)

AVE_INSPT <- 0.3460764
AVE_LPT <- 0.5106865
AVE_TECH <- 0.4848612
AVE_SEFF <- 0.3971452
AVE_TLX <- 0.342476
AVE_ATT <- 0.3920236
AVE_BEH <- 0.4004366

sqrt_AVE_INSPT <- sqrt(AVE_INSPT)
sqrt_AVE_LPT <- sqrt(AVE_LPT)
sqrt_AVE_TECH <- sqrt(AVE_TECH)
sqrt_AVE_SEFF <- sqrt(AVE_SEFF)
sqrt_AVE_TLX <- sqrt(AVE_TLX)
sqrt_AVE_ATT <- sqrt(AVE_ATT)
sqrt_AVE_BEH <- sqrt(AVE_BEH) 

print(sqrt_AVE_INSPT)
print(sqrt_AVE_LPT)
print(sqrt_AVE_TECH)
print(sqrt_AVE_SEFF)
print(sqrt_AVE_TLX)
print(sqrt_AVE_ATT)
print(sqrt_AVE_BEH)

# Compute composite scores for each construct
my_data$INSPT_Composite <- rowMeans(my_data[, c("INSPT1", "INSPT2", "INSPT3", "INSPT4", "INSPT5")])
my_data$LPT_Composite <- rowMeans(my_data[, c("LPT2", "LPT3", "LPT4", "LPT5")])
my_data$TECH_Composite <- rowMeans(my_data[, c("TECH1", "TECH2", "TECH3", "TECH4", "TECH5")])
my_data$SEFF_Composite <- rowMeans(my_data[, c("SEFF1", "SEFF2", "SEFF3", "SEFF4", "SEFF5")])
my_data$TLX_Composite <- rowMeans(my_data[, c("TLX1", "TLX2", "TLX3", "TLX4", "TLX5", "TLX6")])
my_data$ATT_Composite <- rowMeans(my_data[, c("ATT1", "ATT2", "ATT3", "ATT4", "ATT5")])
my_data$BEH_Composite <- rowMeans(my_data[, c("BEH1", "BEH2", "BEH3", "BEH4", "BEH5")])

# Subset the data to include only composite scores
composite_data <- my_data[, c("INSPT_Composite", "LPT_Composite", "TECH_Composite", "SEFF_Composite", "TLX_Composite", "ATT_Composite", "BEH_Composite")]

# Compute the correlation matrix
inter_construct_corr <- cor(composite_data, use = "pairwise.complete.obs")


# Print the correlation matrix
print(inter_construct_corr)

formatted_corr <- round(inter_construct_corr, 3)
print(formatted_corr)
