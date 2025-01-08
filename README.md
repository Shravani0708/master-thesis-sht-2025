The repository contains all the R code files used for the research. All the code was written and executed in the RStudio software. 
It also contains two datasets: 
  'Results_cleaned_wocomposite_forR.csv' contains raw data
  'Result_CompositeScores_ForR.csv' and 'Result_CompositeScores_ForR.xlsx' contain aggregated data.
Each code file indicates and loads the relevant dataset. Please note that file paths are hardcoded and may need to be changed, if the code is needed to be run again.

Description of code files:

1. R code for assessing multivariate normality and QQ plots using Mahalanobis distances: file 'Multivariate normality and Mahalanobis distances'
2. Assessing reliability and item correlations: file 'Reliability and Item Correlations'
3. Confirmatory Factor Analyses (CFA) on subset of data, and calculating convergent validity with CR and AVE: file 'Subset CFA'
4. Assessing discriminant validity as per Fornell Larcker criterion: 'Discriminant Validity_Fornell Larcker'
5. Running SEM with all-item parceling approach using the ML estimator: 'SEM with all item parceling'
6. Conducting exploratory path analyses and subsequent refined SEM analysis with a simplified, refined model: 'ExploratoryPathAnalysis and RefinedSEM'
