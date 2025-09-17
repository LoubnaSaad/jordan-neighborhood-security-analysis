library(readxl)
library(epitools)
library(dplyr)
library(car)
library(caret)
library(pROC)
library(vcd)
library(pscl)
library(ResourceSelection)

#df <- read_excel("data_used.xlsx")
#df_train_cate <- sample_frac(df, 0.9)
#df_test_cate<- anti_join(df, df_train_cate, by = colnames(data)) 
#write.csv(df_train_cate, "traindata_cate_6.csv", row.names = FALSE)
#write.csv(df_test_cate, "testdata_cate_6.csv", row.names = FALSE)

df_train_categ<- read.csv("traindata_cate_6.csv")
df_test_categ<- read.csv("testdata_cate_6.csv")

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
df_train_categ$Education_Level<-NULL
str(df_train_categ)
cols_to_fix <- c("Frequency_Street_Violence", "Frequency_Sexual_Harassment", 
                 "Frequency_Drug_Sales", "Frequency_Racist_Behavior", 
                 "Frequency_Police_Interference","Frequency_Alcohol_Consumption","Frequency_Robberies")

cols_to_fix <- intersect(cols_to_fix, colnames(df_train_categ))



df_train_categ[cols_to_fix] <- df_train_categ[cols_to_fix] %>%
  mutate(across(everything(), ~ ifelse(. %in% c("Not at all frequently", "Don't know"), 
                                       "not at all frequently", .)))

df_train_categ$Secure_in_neighborhood <- ifelse(df_train_categ$Secure_in_neighborhood == "Not at all secure" |df_train_categ$Secure_in_neighborhood == "Not very secure" | df_train_categ$Secure_in_neighborhood == "Don't know", 
                                                " Not secure", "Secure")

df_train_categ$Frequency_Sexual_Harassment <- ifelse(df_train_categ$Frequency_Sexual_Harassment == "Very Frequently" |df_train_categ$Frequency_Sexual_Harassment == "Quite frequently" , 
                                                     " Rather Frequently", df_train_categ$Frequency_Sexual_Harassment)
df_train_categ[] <- apply(df_train_categ, 2, as.factor)
lapply(df_train_categ, function(x) unique(x)) # typo errors
df_train_categ$Age<-as.numeric(df_train_categ$Age)
str(df_train_categ)
# Define the response variable
response_var <- "Secure_in_neighborhood"
df_train_categ[[response_var]] <- ifelse(df_train_categ[[response_var]] == "Secure", 1, 0)

# Define explanatory variables
explanatory_vars <- setdiff(colnames(df_train_categ), response_var)
explanatory_vars <- explanatory_vars[sapply(df_train_categ[, explanatory_vars], function(x) length(unique(x)) > 1)]
explanatory_vars_2<-c(
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
  "Citizen"
)

full_model_with_interaction <- glm(
  Secure_in_neighborhood ~ Frequency_Robberies + Frequency_Alcohol_Consumption + Frequency_Police_Interference +
    Frequency_Racist_Behavior + Frequency_Drug_Sales + Frequency_Street_Violence + Frequency_Sexual_Harassment + 
    Carried_Weapon + Victim_Crime_Last_Year + Sex + Age  +Victim_Crime_Last_Year*Frequency_Police_Interference,,
  data = df_train_categ, family = binomial
)

summary(full_model_with_interaction)
final_model <- step(full_model_with_interaction, direction = "backward")
final_model2<- step(full_model_with_interaction, direction = "forward")
final_model_with_interaction<- step(full_model_with_interaction, direction = "both")
summary(final_model)
str(df_train_categ)
summary(final_model_with_interaction)
# Check for multicollinearity
library(car)
car::vif(final_model)


null_model <- glm(as.formula(paste(response_var, "~ 1")), 
                  data = df_train_categ, family = binomial)

anova(full_model_with_interaction,final_model, test = "LRT")

anova(full_model_with_interaction,final_model_with_interaction, test = "LRT")
# Hosmer-Lemeshow test
hoslem.test(df_train_categ[[response_var]], fitted(final_model))



# Assuming 'test_data' is your test dataset
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

colnames(df_test_categ) <- new_names
df_test_categ$Education_Level<-NULL

str(df_test_categ)
cols_to_fix <- c("Frequency_Street_Violence", "Frequency_Sexual_Harassment", 
                 "Frequency_Drug_Sales", "Frequency_Racist_Behavior", 
                 "Frequency_Police_Interference","Frequency_Alcohol_Consumption","Frequency_Robberies")

cols_to_fix <- intersect(cols_to_fix, colnames(df_test_categ))

df_test_categ[cols_to_fix] <- df_test_categ[cols_to_fix] %>%
  mutate(across(everything(), ~ ifelse(. %in% c("Not at all frequently", "Don't know"), 
                                       "not at all frequently", .)))

df_test_categ$Secure_in_neighborhood <- ifelse(df_test_categ$Secure_in_neighborhood == "Not at all secure" |df_test_categ$Secure_in_neighborhood == "Not very secure" | df_test_categ$Secure_in_neighborhood == "Don't know", 
                                                " Not secure", "Secure")

df_test_categ$Frequency_Sexual_Harassment <- ifelse(df_test_categ$Frequency_Sexual_Harassment == "Very Frequently" |df_test_categ$Frequency_Sexual_Harassment == "Quite frequently" , 
                                                     " Rather Frequently", df_train_categ$Frequency_Sexual_Harassment)
df_test_categ[] <- apply(df_test_categ, 2, as.factor)
lapply(df_test_categ, function(x) unique(x)) # typo errors
df_test_categ$Age<-as.numeric(df_test_categ$Age)

#3333#33###333###33###333333##333##33
# Define the response variable

# Define explanatory variables
explanatory_vars_test <- setdiff(colnames(df_test_categ), response_var_test)

predicted_probs <- predict(final_model, newdata = df_test_categ, type = "response")
actual_outcome <- factor(df_test_categ$Secure_in_neighborhood, levels = c(0, 1))  # Ensure binary factor
levels(actual_outcome)

# Create an ROC curve
roc_curve <- roc(actual_outcome, predicted_probs)


# Determine the best cut-off point
plot(roc_curve, main = "ROC curve - Logistic Regression")


sensitivity_oncut <- c()
specificity_oncut <- c()
cutoff_point <- c()
all_correctly <- c()

for (i in seq(0, 1, 0.1)) {
  predictclass <- ifelse(predicted_probs > i, 1, 0)
  CM <- table(actual_outcome, predictclass)
  
  sens <- sensitivity(factor(actual_outcome), factor(predictclass))
  spec <- specificity(factor(actual_outcome), factor(predictclass))
  
  sensitivity_oncut <- c(sensitivity_oncut, sens)
  specificity_oncut <- c(specificity_oncut, spec)
  cutoff_point <- c(cutoff_point, i)
  
  overall <- sum(diag(CM)) / sum(CM)
  all_correctly <- c(all_correctly, overall)
}
maximum <- which.max(sensitivity_oncut + specificity_oncut)
cat("The optimal cutoff point is:", cutoff_point[maximum], 
    "\n", "based on max sens & spec", "\n")

maxoverall <- which.max(all_correctly)
cat("The optimal cutoff point is:", cutoff_point[maxoverall], 
    "\n", "based on max overall correctly specified", "\n")

predicted_classes <- ifelse(predicted_probs > 0.3, 1, 0)

# Ensure predicted_classes is a factor with the same levels as actual_outcome
predicted_classes <- factor(predicted_classes, levels = c(0, 1))

# Ensure actual_outcome is a factor with the same levels as predicted_classes
actual_outcome <- factor(actual_outcome, levels = c(0, 1))

# Now, generate the confusion matrix
confusionMatrix(predicted_classes, actual_outcome)

# Plot the ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve")
abline(a = 0, b = 1, col = "red", lty = 2)  # Add diagonal line

plot(roc_curve_test, main = "ROC Curve for Test Data")
auc(roc_curve_test)
pscl::pR2(final_model)["McFadden"]









