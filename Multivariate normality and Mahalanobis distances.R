rm(list = ls())  # Clear workspace
# Installing required packages
install.packages("MVN")
install.packages ("MASS")

# Accessing the relevant libraries
library(MASS)
library(MVN)

#loading composite score data saved from SPSS -- path is from local drive!
my_data <- read.csv("C:\\Users\\ADMIN\\OneDrive - Universität des Saarlandes\\SoSe 24\\Master Thesis\\Data analysis\\Submission files\\Result_CompositeScores_ForR.csv")

# make data numeric
str(my_data)
my_data_numeric <- my_data[sapply(my_data, is.numeric)]

# Mardia's test of multivariate normality
result <- mvn(data = my_data_numeric, mvnTest = "mardia")
print(result$multivariateNormality)

# Compute the Mahalanobis distances
mahal_dist <- mahalanobis(my_data_numeric, colMeans(my_data_numeric), cov(my_data_numeric))

# Define degrees of freedom (number of variables in the data)
df <- ncol(my_data_numeric)

# Create the QQ-plot
qqplot(qchisq(ppoints(length(mahal_dist)), df), mahal_dist,
       main = "QQ-Plot of Mahalanobis Distances",
       xlab = "Chi-square Theoretical Quantiles",
       ylab = "Observed Mahalanobis Distances")

# Add a 45-degree reference line
abline(0, 1, col = "red")

# Chi-square critical value
critical_value <- qchisq(0.95, df)

# Identify potential outliers
outliers <- which(mahal_dist > critical_value)

# Print outliers
print(outliers)
