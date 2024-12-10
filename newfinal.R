# Load necessary libraries
library(car)
library(e1071)
library(lmtest)
library(ggplot2)
library(mediation)
library(haven)
data <- read_sav("finalspss.sav")

# Create a new dataframe excluding the rows where empty is 1, (having only complete cases)
data_cleaned <- data[data$empty != 1, ]

# Create a new dataframe excluding the rows where `remove` is 1 (misinterpretation of intervention)
data_cleaned <- data_cleaned[data_cleaned$Remove != 1, ]

# Create a new dataframe excluding the rows where bogus is for misinterpretation of scales (SD of STAI 6 is 0)
data_cleaned <- data_cleaned[data_cleaned$bogus1 != 1, ]

# Create a new dataframe excluding the rows where bogus is for misinterpretation of scales (SD of STAI 6 is 0)
data_cleaned <- data_cleaned[data_cleaned$Bogus != 1, ]

# View the cleaned dataframe
print(data_cleaned)

table(data_cleaned$country, data_cleaned$experimental)
table(data_cleaned$experimental)
table(data_cleaned$country)


# Load necessary packages
library(psych)

# Define a function to calculate descriptive statistics
calculate_descriptive_stats <- function(variable, name) {
  stats <- describe(variable)
  
  data.frame(
    Variable = name,
    Mean = stats$mean,
    SD = stats$sd,
    Min = stats$min,
    Max = stats$max,
    Skewness = stats$skew,
    Kurtosis = stats$kurtosis
  )
}

# Create a named list of variables
variables <- list(
  FSMAS = data_cleaned$FSMAS,
  State_Anx_Pre = data_cleaned$STAI_S_Task_1,
  State_Anx_Post = data_cleaned$STAI_S_Task_2,
  Flow_Pre = data_cleaned$PFS_Task_1,
  Flow_Post = data_cleaned$PFS_Task_2,
  SIMA = data_cleaned$SIMA__1,
  Math_fluency_Pre = data_cleaned$math_fluency_Task_1_234,
  Math_fluency_Post = data_cleaned$math_fluency_Task_2_234
)

# Calculate descriptive statistics for each variable
descriptive_stats <- do.call(rbind, lapply(names(variables), function(name) {
  calculate_descriptive_stats(variables[[name]], name)
}))

# Display the descriptive statistics
print(descriptive_stats)

# Install necessary package (if not already installed)
if (!require("writexl")) install.packages("writexl")

# Load the package
library(writexl)

# Export the descriptive statistics dataframe to an Excel file
output_file <- "descriptive_statistics.xlsx"
write_xlsx(descriptive_stats, path = output_file)

cat("Table successfully exported to", output_file)

### Corelations in the Complete data set

# Install necessary packages (if not already installed)
if (!require("Hmisc")) install.packages("Hmisc")
if (!require("writexl")) install.packages("writexl")

# Load the packages
library(Hmisc)
library(writexl)

# Create a dataframe from the variables for correlation analysis
correlation_data <- data.frame(
  FSMAS = data_cleaned$FSMAS,
  State_Anx_Pre = data_cleaned$STAI_S_Task_1,
  State_Anx_Post = data_cleaned$STAI_S_Task_2,
  Flow_Pre = data_cleaned$PFS_Task_1,
  Flow_Post = data_cleaned$PFS_Task_2,
  SIMA = data_cleaned$SIMA__1,
  Math_fluency_Pre = data_cleaned$math_fluency_Task_1_234,
  Math_fluency_Post = data_cleaned$math_fluency_Task_2_234
)

# Calculate the correlation matrix and p-values
correlation_results <- rcorr(as.matrix(correlation_data), type = "pearson")

# Extract correlation coefficients and p-values
cor_matrix <- correlation_results$r
p_matrix <- correlation_results$P

# Format the results with p-values in brackets
formatted_matrix <- matrix(NA, nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
rownames(formatted_matrix) <- rownames(cor_matrix)
colnames(formatted_matrix) <- colnames(cor_matrix)

for (i in seq_len(nrow(cor_matrix))) {
  for (j in seq_len(ncol(cor_matrix))) {
    formatted_matrix[i, j] <- paste0(
      round(cor_matrix[i, j], 2),
      " (p=",
      signif(p_matrix[i, j], 2),
      ")"
    )
  }
}

# Convert the formatted matrix to a dataframe for export
formatted_df <- as.data.frame(formatted_matrix)

# Export the formatted matrix to an Excel file
output_file <- "correlation_matrix_completeData_with_p_values.xlsx"
write_xlsx(formatted_df, path = output_file)

cat("Correlation matrix with p-values successfully exported to", output_file)






# Perform independent samples t-test
t_test_result <- t.test(FSMAS ~ country, data = data_cleaned)

# Print the results
print(t_test_result)

# Perform independent samples t-test
t_test_result <- t.test(FSMAS ~ experimental, data = data_cleaned)

# Print the results
print(t_test_result)


# Perform independent samples t-test
t_test_result <- t.test(SIMA__1 ~ country, data = data_cleaned)

# Print the results
print(t_test_result)

# Perform independent samples t-test
t_test_result <- t.test(SIMA__1 ~ experimental, data = data_cleaned)

# Print the results
print(t_test_result)

# Mediation Analysis
# Mediator Models 

# Block 1: Pre-intervention state anxiety as control
block1_model <- lm(PFS_Task_2 ~ PFS_Task_1 + math_fluency_Task_1_234, data = data_cleaned)
summary(block1_model)

# Check for multicollinearity in cleaned data

cleaned_vif_values <- vif(block1_model, type = 'predictor')

print("VIF values after cleaning:")
print(cleaned_vif_values)

# Check for independence of errors in cleaned data
cleaned_dw_test <- dwtest(block1_model)
print("Durbin-Watson test after cleaning:")
print(cleaned_dw_test)

# Define the scope: specify the lower model and upper model
# Lock Change_Math_fluency in the minimum model
scope <- list(lower = ~ PFS_Task_1 + math_fluency_Task_1_234, 
              upper = ~ PFS_Task_1 + math_fluency_Task_1_234 + math_fluency_Task_2_234 * country * experimental)

block1_model <- lm(PFS_Task_2 ~ PFS_Task_1 + math_fluency_Task_1_234 + math_fluency_Task_2_234* country * experimental, data = data_cleaned)

# Perform stepwise model selection
mediator_model_step <- step(block1_model, scope = scope)

# Display the summary of the final model after stepwise selection
summary(mediator_model_step)

skewness(mediator_model_step$residuals)
kurtosis(mediator_model_step$residuals)

# Check for multicollinearity in cleaned data

cleaned_vif_values <- vif(mediator_model_step, type = 'predictor')

print("VIF values after cleaning:")
print(cleaned_vif_values)

# Check for independence of errors in cleaned data
cleaned_dw_test <- dwtest(mediator_model_step)
print("Durbin-Watson test after cleaning:")
print(cleaned_dw_test)


bptest(mediator_model_step)

# Simple Slope anaylsis
# Install and load required packages
if (!require("emmeans")) install.packages("emmeans")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
library(emmeans)
library(ggplot2)
library(dplyr)


# Fit the interaction model
interaction_model <- mediator_model_step

# Perform simple slope analysis
simple_slopes <- emmeans(interaction_model, ~ math_fluency_Task_2_234 | country * experimental)

# Display the simple slopes
print("Simple Slopes Analysis:")
print(simple_slopes)

# Extract slope estimates for plotting (optional)
slopes_df <- as.data.frame(simple_slopes)

# Create an interaction plot
interaction_plot <- data_cleaned %>%
  ggplot(aes(x = math_fluency_Task_2_234, y = PFS_Task_2, color = interaction(country, experimental))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Interaction of Math Fluency, Country, and Experimental Group",
    x = "Post-Intervention Math Fluency",
    y = "Post-Intervention Flow State",
    color = "Country × Experiment"
  ) +
  theme_minimal()

# Print the interaction plot
print(interaction_plot)

# Summary of the interaction model for reference
print("Summary of Interaction Model:")
summary_interaction <- summary(interaction_model)
print(summary_interaction)





# Outcome Models

# Block 1: Post-intervention controls
block2_model <- lm(STAI_S_Task_2 ~ PFS_Task_1 + STAI_S_Task_1 + math_fluency_Task_1_234, data = data_cleaned)
summary(block2_model)

# Check for multicollinearity in cleaned data

cleaned_vif_values <- vif(block2_model, type = 'predictor')

print("VIF values after cleaning:")
print(cleaned_vif_values)

# Check for independence of errors in cleaned data
cleaned_dw_test <- dwtest(block2_model)
print("Durbin-Watson test after cleaning:")
print(cleaned_dw_test)

# Define the scope: specify the lower model and upper model
# Lock Change_Math_fluency in the minimum model
scope <- list(lower = ~ PFS_Task_1 + STAI_S_Task_1 + math_fluency_Task_1_234 + PFS_Task_2 + math_fluency_Task_2_234, 
              upper = ~ PFS_Task_1 + STAI_S_Task_1 + math_fluency_Task_1_234 + math_fluency_Task_2_234 * country * experimental + PFS_Task_2 * country * experimental)

block2_model <- lm(STAI_S_Task_2 ~ STAI_S_Task_1 + PFS_Task_1 + math_fluency_Task_1_234 + PFS_Task_2 * country * experimental + math_fluency_Task_2_234 * country * experimental, data = data_cleaned)

# Perform stepwise model selection
outcome_model_step <- step(block2_model, scope = scope)

# Display the summary of the final model after stepwise selection
summary(outcome_model_step)

skewness(outcome_model_step$residuals)
kurtosis(outcome_model_step$residuals)

# Check for multicollinearity in cleaned data

cleaned_vif_values <- vif(outcome_model_step, type = 'predictor')

print("VIF values after cleaning:")
print(cleaned_vif_values)

# Check for independence of errors in cleaned data
cleaned_dw_test <- dwtest(outcome_model_step)
print("Durbin-Watson test after cleaning:")
print(cleaned_dw_test)


bptest(outcome_model_step)


#performing first time mediation...
# Perform mediation analysis
mediation_result <- mediation::mediate(mediator_model_step, outcome_model_step, treat = "math_fluency_Task_2_234", mediator = "PFS_Task_2", boot = TRUE, sims = 1000)

# Summarize the mediation results
summary(mediation_result)

# Plot the mediation results
plot(mediation_result)



##### ANOVA
### Anxiety
##Data Cleaned
# Load necessary libraries
library(dplyr)
library(tidyr)
library(car)

# Ensure factors are correctly set
data_cleaned$Country <- as.factor(data_cleaned$country)
data_cleaned$Experiment <- as.factor(data_cleaned$experimental)

# Convert the data to long format for repeated measures analysis
data_long <- data_cleaned %>%
  gather(key = "Time", value = "Anxiety", STAI_S_Task_1, STAI_S_Task_2) %>%
  mutate(Time = factor(Time, levels = c("STAI_S_Task_1", "STAI_S_Task_2")))

# Perform the parametric repeated measures ANOVA
anova_model <- aov(Anxiety ~ Country * Experiment * Time + Error(VAR00001 / Time), data = data_long)

# Summarize the ANOVA results
summary(anova_model)


##Data Cleaned
# Load necessary libraries
library(dplyr)
library(tidyr)
library(car)

# Ensure factors are correctly set
data_cleaned$Country <- as.factor(data_cleaned$country)
data_cleaned$Experiment <- as.factor(data_cleaned$experimental)

# Convert the data to long format for repeated measures analysis
data_long <- data_cleaned %>%
  gather(key = "Time", value = "Fluency", math_fluency_Task_1_234, math_fluency_Task_2_234) %>%
  mutate(Time = factor(Time, levels = c("math_fluency_Task_1_234", "math_fluency_Task_2_234")))

# Perform the parametric repeated measures ANOVA
anova_model <- aov(Fluency ~ Country * Experiment * Time + Error(VAR00001 / Time), data = data_long)

# Summarize the ANOVA results
summary(anova_model)

### Flow
##Data Cleaned
# Load necessary libraries
library(dplyr)
library(tidyr)
library(car)

# Ensure factors are correctly set
data_cleaned$Country <- as.factor(data_cleaned$country)
data_cleaned$Experiment <- as.factor(data_cleaned$experimental)

# Convert the data to long format for repeated measures analysis
data_long <- data_cleaned %>%
  gather(key = "Time", value = "Flow", PFS_Task_1, PFS_Task_2) %>%
  mutate(Time = factor(Time, levels = c("PFS_Task_1", "PFS_Task_2")))

# Perform the parametric repeated measures ANOVA
anova_model <- aov(Flow ~ Country * Experiment * Time + Error(VAR00001 / Time), data = data_long)

# Summarize the ANOVA results
summary(anova_model)



############ Internvention Analysis

cleaned_data_without_outliers <- data_cleaned
# Remove cases with missing values in Social_Rel_recoded
data_cleaned <- cleaned_data_without_outliers[!is.na(cleaned_data_without_outliers$Social_Rel_recoded), ]

# Ensure Social_Rel_recoded is a factor
data_cleaned$Social_Rel_recoded <- as.factor(data_cleaned$Social_Rel_recoded)


# Create a named list of variables
variables <- list(
  IOS = data_cleaned$IOS,
  State_Anx_Pre = data_cleaned$STAI_S_Task_1,
  State_Anx_Post = data_cleaned$STAI_S_Task_2,
  Flow_Pre = data_cleaned$PFS_Task_1,
  Flow_Post = data_cleaned$PFS_Task_2,
  Math_fluency_Pre = data_cleaned$math_fluency_Task_1_234,
  Math_fluency_Post = data_cleaned$math_fluency_Task_2_234
)

# Calculate descriptive statistics for each variable
descriptive_stats <- do.call(rbind, lapply(names(variables), function(name) {
  calculate_descriptive_stats(variables[[name]], name)
}))

# Display the descriptive statistics
print(descriptive_stats)



# Load the package
library(writexl)

# Export the descriptive statistics dataframe to an Excel file
output_file <- "descriptive_statisticsexp.xlsx"
write_xlsx(descriptive_stats, path = output_file)

cat("Table successfully exported to", output_file)




### Corelations in the Experimental data set

# Install necessary packages (if not already installed)
if (!require("Hmisc")) install.packages("Hmisc")
if (!require("writexl")) install.packages("writexl")

# Load the packages
library(Hmisc)
library(writexl)

# Create a dataframe from the variables for correlation analysis
correlation_data <- data.frame(
  IOS = data_cleaned$IOS,
  State_Anx_Pre = data_cleaned$STAI_S_Task_1,
  State_Anx_Post = data_cleaned$STAI_S_Task_2,
  Flow_Pre = data_cleaned$PFS_Task_1,
  Flow_Post = data_cleaned$PFS_Task_2,
  Math_fluency_Pre = data_cleaned$math_fluency_Task_1_234,
  Math_fluency_Post = data_cleaned$math_fluency_Task_2_234
)

# Calculate the correlation matrix and p-values
correlation_results <- rcorr(as.matrix(correlation_data), type = "pearson")

# Extract correlation coefficients and p-values
cor_matrix <- correlation_results$r
p_matrix <- correlation_results$P

# Format the results with p-values in brackets
formatted_matrix <- matrix(NA, nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
rownames(formatted_matrix) <- rownames(cor_matrix)
colnames(formatted_matrix) <- colnames(cor_matrix)

for (i in seq_len(nrow(cor_matrix))) {
  for (j in seq_len(ncol(cor_matrix))) {
    formatted_matrix[i, j] <- paste0(
      round(cor_matrix[i, j], 2),
      " (p=",
      signif(p_matrix[i, j], 2),
      ")"
    )
  }
}

# Convert the formatted matrix to a dataframe for export
formatted_df <- as.data.frame(formatted_matrix)

# Export the formatted matrix to an Excel file
output_file <- "correlation_matrix_exp_with_p_values.xlsx"
write_xlsx(formatted_df, path = output_file)

cat("Correlation matrix with p-values successfully exported to", output_file)


# Mediator Models 

# Block 1: Pre-intervention state anxiety as control
block1_model <- lm(PFS_Task_2 ~ PFS_Task_1 + math_fluency_Task_1_234, data = data_cleaned)
summary(block1_model)

# Check for multicollinearity in cleaned data

cleaned_vif_values <- vif(block1_model, type = 'predictor')

print("VIF values after cleaning:")
print(cleaned_vif_values)

# Check for independence of errors in cleaned data
cleaned_dw_test <- dwtest(block1_model)
print("Durbin-Watson test after cleaning:")
print(cleaned_dw_test)

# Define the scope: specify the lower model and upper model 
# Lock Change_Math_fluency in the minimum model
scope <- list(lower = ~ PFS_Task_1 + math_fluency_Task_1_234, 
              upper = ~ PFS_Task_1 + math_fluency_Task_1_234 + math_fluency_Task_2_234 * country*IOS)

block1_model <- lm(PFS_Task_2 ~ PFS_Task_1 + math_fluency_Task_1_234 + math_fluency_Task_2_234 * country*IOS, data = data_cleaned)

# Perform stepwise model selection
mediator_model_step <- step(block1_model, scope = scope)

# Display the summary of the final model after stepwise selection
summary(mediator_model_step)

skewness(mediator_model_step$residuals)
kurtosis(mediator_model_step$residuals)

# Check for multicollinearity in cleaned data

cleaned_vif_values <- vif(mediator_model_step, type = 'predictor')

print("VIF values after cleaning:")
print(cleaned_vif_values)

# Check for independence of errors in cleaned data
cleaned_dw_test <- dwtest(mediator_model_step)
print("Durbin-Watson test after cleaning:")
print(cleaned_dw_test)


bptest(mediator_model_step)

# Install and load required packages
if (!require("emmeans")) install.packages("emmeans")
library(emmeans)

# Fit the model (mediator_model_step already exists, so you don't need to refit it)
interaction_model <- mediator_model_step

# Perform simple slope analysis
simple_slopes <- emmeans(interaction_model, 
                         specs = ~ math_fluency_Task_2_234 | IOS,
                         at = list(IOS = c(
                           max(mean(data_cleaned$IOS, na.rm = TRUE) - sd(data_cleaned$IOS, na.rm = TRUE), 1), 
                           mean(data_cleaned$IOS, na.rm = TRUE), 
                           min(mean(data_cleaned$IOS, na.rm = TRUE) + sd(data_cleaned$IOS, na.rm = TRUE), 7)
                         ))
)

# Display simple slopes
print("Simple Slope Analysis:")
print(simple_slopes)


# Plot interaction effect (optional)
plot(simple_slopes, comparison = TRUE) +
  labs(title = "Interaction of Math Fluency and IOS on Flow State",
       x = "Post-Intervention Math Fluency",
       y = "Post-Intervention Flow State") +
  theme_minimal()

# Outcome Models

# Block 1: Post-intervention controls
block2_model <- lm(STAI_S_Task_2 ~ PFS_Task_1 + STAI_S_Task_1 + math_fluency_Task_1_234, data = data_cleaned)
summary(block2_model)

# Check for multicollinearity in cleaned data

cleaned_vif_values <- vif(block2_model, type = 'predictor')

print("VIF values after cleaning:")
print(cleaned_vif_values)

# Check for independence of errors in cleaned data
cleaned_dw_test <- dwtest(block2_model)
print("Durbin-Watson test after cleaning:")
print(cleaned_dw_test)

# Define the scope: specify the lower model and upper model
# Lock Change_Math_fluency in the minimum model
scope <- list(lower = ~ PFS_Task_1 + STAI_S_Task_1 + math_fluency_Task_1_234 + PFS_Task_2 + math_fluency_Task_2_234, 
              upper = ~ PFS_Task_1 + STAI_S_Task_1 + math_fluency_Task_1_234 + math_fluency_Task_2_234 * country*IOS  + PFS_Task_2 * country*IOS )

block2_model <- lm(STAI_S_Task_2 ~ STAI_S_Task_1 + PFS_Task_1 + math_fluency_Task_1_234 + PFS_Task_2 * country *IOS + math_fluency_Task_2_234  * country *IOS, data = data_cleaned)

# Perform stepwise model selection
outcome_model_step <- step(block2_model, scope = scope)

# Display the summary of the final model after stepwise selection
summary(outcome_model_step)

skewness(outcome_model_step$residuals)
kurtosis(outcome_model_step$residuals)

# Check for multicollinearity in cleaned data

cleaned_vif_values <- vif(outcome_model_step, type = 'predictor')

print("VIF values after cleaning:")
print(cleaned_vif_values)

# Check for independence of errors in cleaned data
cleaned_dw_test <- dwtest(outcome_model_step)
print("Durbin-Watson test after cleaning:")
print(cleaned_dw_test)


bptest(outcome_model_step)


#performing first time mediation...
# Perform mediation analysis
mediation_result <- mediation::mediate(mediator_model_step, outcome_model_step, treat = "math_fluency_Task_2_234", mediator = "PFS_Task_2", boot = TRUE, sims = 1000)

# Summarize the mediation results
summary(mediation_result)

# Plot the mediation results
plot(mediation_result)



########## Since we have a new model so calculating power
# Given R-squared value
R2 <- 0.3072

# Calculate f2
f2 <- R2 / (1 - R2)

print(paste("f2 value:", round(f2, 3)))

library(pwr)

# Number of predictors
k <- 11  

# Sample size
sample_size <- 90

# Significance level
alpha <- 0.05

# Effect size
f2 <- 0.443

# Calculate the power
power_analysis <- pwr.f2.test(u = k, v = sample_size - k - 1, sig.level = alpha, f2 = f2)
print(power_analysis)

