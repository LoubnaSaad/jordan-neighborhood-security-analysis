
library(readxl)
library(epitools)
library(dplyr)

#data <- read_excel("data_used.xlsx")
#set.seed(123)
#df_train_cate <- sample_frac(data, 0.8)
#df_test_cate<- anti_join(data, df_train_cate, by = colnames(data)) 
#write.csv(df_train_cate, "traindata_cate.csv", row.names = FALSE)
#write.csv(df_test_cate, "testdata_cate.csv", row.names = FALSE)

df_train_categ<- read.csv("traindata_cate.csv")
new_names <- c(
  "Secure_in_neighborhood", 
  "Frequency_Robberies", 
  "Frequency_Alcohol_Consumption", 
  "Frequency_Police_Interference", 
  "Frequency_Racist_Behavior", 
  "Frequency_Drug_Sales", 
  "Frequency_Street_Violence", 
  "Frequency_Sexual_Harassment", 
  "Carried_Weapon", 
  "Victim_Crime_Last_Year", 
  "Family_Victim_Crime_Last_Year", 
  "Sex", 
  "Age", 
  "Citizen", 
  "Education_Level"
)

# Assign the new names to the dataset

colnames(df_train_categ) <- new_names
str(df_train_categ)
cols_to_fix <- c("Frequency_Street_Violence", "Frequency_Sexual_Harassment", "Parental_Education_Level", 
                    "Frequency_Drug_Sales", "Frequency_Racist_Behavior", 
                    "Frequency_Police_Interference","Frequency_Alcohol_Consumption","Frequency_Robberies")

cols_to_fix <- intersect(cols_to_fix, colnames(df_train_categ))


df_train_categ$Age <- ifelse(df_train_categ$Age <= 30, "Young", 
                          ifelse(df_train_categ$Age <= 50, "Middle-aged", "Old"))
df_train_categ[cols_to_fix] <- df_train_categ[cols_to_fix] %>%
  mutate(across(everything(), ~ ifelse(. %in% c("Not at all frequently", "Don't know"), 
                                       "not at all frequently", .)))

df_train_categ$Secure_in_neighborhood <- ifelse(df_train_categ$Secure_in_neighborhood == "Not at all secure" |df_train_categ$Secure_in_neighborhood == "Not very secure" | df_train_categ$Secure_in_neighborhood == "Don't know", 
                                      " Not secure", "Secure")

df_train_categ$Frequency_Sexual_Harassment <- ifelse(df_train_categ$Frequency_Sexual_Harassment == "Very Frequently" |df_train_categ$Frequency_Sexual_Harassment == "Quite frequently" , 
                                                " Rather Frequently", df_train_categ$Frequency_Sexual_Harassment)
df_train_categ <- lapply(df_train_categ,as.factor)
lapply(df_train_categ, function(x) unique(x)) # typo errors


# Define the response variable
response_var <- "Secure_in_neighborhood"

# Define explanatory variables
explanatory_vars <- setdiff(colnames(df_train_categ), response_var)

# Loop through each explanatory variable
for (var in explanatory_vars) {
  print(paste("Analyzing association between", response_var, "and", var))
  
  # Create a two-way contingency table
  contingency_table <- table(explanatory_vars[[response_var]], explanatory_vars[[var]])
  
  # Print the contingency table
  print(contingency_table)
  
  # Perform chi-squared test
  chi_test <- chisq.test(contingency_table)
  print("Chi-Squared Test:")
  print(chi_test)
  
 
  
}

# View the modified data
print(data)

str(data)

# Define the controlling variable
control_var <- "Sex" # Assuming this column is gender

# Create a three-way contingency table
three_way_table1 <- xtabs(~ data[[response_var]] + data[[explanatory_vars[1]]] + data[[control_var]])
three_way_table <- table( data[[response_var]]  ,data[[explanatory_vars[1]]] ,data[[control_var]])

# Print the table
print("Three-Way Contingency Table:")
print(three_way_table)

# Collapse across the control variable (sum over the 3rd dimension)
collapsed_table <- apply(three_way_table1, c(1, 2), sum)

# Print the collapsed table
print(collapsed_table)

# Perform chi-squared test on the collapsed table
chi_test1 <- chisq.test(collapsed_table)
print("Chi-Squared Test (Collapsed):")
print(chi_test)
# List of explanatory variables (excluding response variable)
explanatory_vars <- setdiff(colnames(data), "Secure_in_neighborhood")

# Loop through all explanatory variables and check expected frequencies
for (var in explanatory_vars) {
  # Create a contingency table for the current variable and the response variable
  contingency_table <- table(data$Secure_in_neighborhood, data[[var]])
  
  # Perform chi-squared test
  chi_test <- chisq.test(contingency_table)
  
  # Display the observed and expected counts
  cat("\nTable for Response vs", var, "\n")
  print("Observed Counts:")
  print(chi_test$observed)
  
  print("Expected Counts:")
  print(chi_test$expected)
  
  # Check if any expected frequencies are less than 5
  low_count_cells <- sum(chi_test$expected < 5)
  percent_low_counts <- (low_count_cells / length(chi_test$expected)) * 100
  print(paste("Number of cells with expected frequency < 5:", low_count_cells))
  print(paste("Percentage of cells with expected frequency < 5:", percent_low_counts, "%"))
  
  # Check if any cell has expected frequency = 0
  zero_expected <- sum(chi_test$expected == 0)
  print(paste("Number of cells with expected frequency = 0:", zero_expected))
}
install.packages("DescTools")
library(DescTools)

contingency_table <- table(data$Secure_in_neighborhood, data$Frequency_Robberies)
gamma_stat <- GoodmanKruskalGamma(contingency_table)
print(paste("Goodman and Kruskal's Gamma:", gamma_stat))
ordinal_vars <- c("Frequency_Robberies", "Frequency_Alcohol_Consumption", "Frequency_Police_Interference","Frequency_Racist_Behavior", 
                  "Frequency_Drug_Sales", 
                  "Frequency_Street_Violence", 
                  "Frequency_Sexual_Harassment","Age", 
  "Education_Level") 
for (var in explanatory_vars) {
  # Create a contingency table
  contingency_table <- table(data[[response_var]], data[[var]])
  
  # Calculate Goodman and Kruskal's Gamma
  gamma_stat <- GoodmanKruskalGamma(contingency_table,conf.level=0.95)
  
  # Print the result
  cat("\nGoodman and Kruskal's Gamma for", var, "vs", response_var, ":\n")
  print(gamma_stat)
}

# Mantel-Haenszel Test for Conditional Independence
mh_test <- mantelhaen.test(three_way_table)
table2 <- table(data$Secure_in_neighborhood, data$Frequency_Robberies)
DescTools::GoodmanKruskalGamma(table2, conf.level=0.95)

# Print results
cat("\nMantel-Haenszel Test for Conditional Independence:\n")
print(mh_test)
