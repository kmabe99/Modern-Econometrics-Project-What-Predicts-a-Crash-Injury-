#Clear Global Environment 
rm(list = ls())

# ------------------------------------------------------
# 1. Import Dataset and Clean Dataset
# ------------------------------------------------------
#load necessary libraries
library(dplyr)
library(psych)  # for describe()
library(ggplot2)
library(readxl)   # For reading Excel files
library(tidyverse)
library(stats)
library(Hmisc)
library(kableExtra)
library(car)

# Read the CSV file
data <- read.csv('/Users/student/Documents/Spring Babson 2025/Econometrics/Project/Crash_Reporting_-_Drivers_Data.csv', stringsAsFactors = FALSE, na.strings = c("", "N/A"))

# View the first few rows of the data
head(data)

##CLEANING DATASET
sum(is.na(data))

# Keep only the relevant columns
data_subset <- data %>%
  select(Injury.Severity, Weather, Speed.Limit, Driver.Distracted.By, Vehicle.Year)

# Remove rows with any missing (NA) values
cleaned_data <- na.omit(data_subset)

#Create new varaible for age of car
cleaned_data <- cleaned_data %>%
  mutate(Vehicle.Year = as.numeric(Vehicle.Year)) %>%
  filter(!is.na(Vehicle.Year), Vehicle.Year >= 1950, Vehicle.Year <= 2025) %>%  # Keep only plausible years
  mutate(vehicle_age = 2025 - Vehicle.Year)


cleaned_data$Vehicle.Year <- NULL

#Group injury.severity into 1 = injury, 0 = no injury
cleaned_data <- cleaned_data %>%
  mutate(injury = case_when(
    Injury.Severity %in% c("No Apparent Injury", "NO APPARENT INJURY") ~ 0,
    Injury.Severity %in% c("Possible Injury", "POSSIBLE INJURY",
                           "Suspected Minor Injury", "SUSPECTED MINOR INJURY",
                           "Suspected Serious Injury", "SUSPECTED SERIOUS INJURY",
                           "Fatal Injury", "FATAL INJURY") ~ 1,
    TRUE ~ NA_real_
  ))
table(cleaned_data$injury)
sum(is.na(cleaned_data$injury))
cleaned_data$Injury.Severity = NULL

#clean up weather variable to group into similar groups
cleaned_data <- cleaned_data %>%
  filter(!Weather %in% c("Unknown", "UNKNOWN")) %>%
  mutate(weather = case_when(
    Weather %in% c("Clear", "CLEAR") ~ "Clear",
    Weather %in% c("Cloudy", "CLOUDY") ~ "Cloudy",
    Weather %in% c("Rain", "RAINING", "Freezing Rain Or Freezing Drizzle") ~ "Rain",
    Weather %in% c("Snow", "SNOW", "Blowing Snow", "BLOWING SNOW", 
                   "SLEET", "Sleet Or Hail", "WINTRY MIX") ~ "Snow",
    Weather %in% c("Fog, Smog, Smoke", "FOGGY") ~ "Fog/Smog",
    Weather %in% c("Severe Crosswinds", "SEVERE WINDS") ~ "Wind",
    Weather %in% c("BLOWING SAND, SOIL, DIRT") ~ "Sand/Dirt",
    Weather %in% c("OTHER") ~ "Other",
    TRUE ~ NA_character_
  ))

table(cleaned_data$weather)
sum(is.na(cleaned_data$weather))
cleaned_data$Weather= NULL

#clean up driver.distracted.by variable to group into similar groups
cleaned_data <- cleaned_data %>%
  filter(!Driver.Distracted.By %in% c("Unknown", "UNKNOWN")) %>%
  mutate(driver_distracted = case_when(
    Driver.Distracted.By %in% c("Not Distracted", "NOT DISTRACTED") ~ 0,
    TRUE ~ 1
  ))

table(cleaned_data$driver_distracted)
sum(is.na(cleaned_data$driver_distracted))
cleaned_data$Driver.Distracted.By= NULL

#convert variables into correct types
cleaned_data$driver_distracted = as.factor(cleaned_data$driver_distracted)
cleaned_data$injury = as.factor(cleaned_data$injury)
cleaned_data$weather = as.factor(cleaned_data$weather)
cleaned_data$Speed.Limit = as.numeric(cleaned_data$Speed.Limit)

#remove unecessary datasets
data = NULL
data_subset = NULL

# ------------------------------------------------------
# 2. Appendix 1: Mean, STDV, n, histograms, scatter plots
# ------------------------------------------------------

#Mean/Mode, STDV, and n for each observation
# Get mode for each factor variable
injury_mode <- cleaned_data %>%
  count(injury) %>%
  slice_max(n, n = 1) %>%
  pull(injury)

weather_mode <- cleaned_data %>%
  count(weather) %>%
  slice_max(n, n = 1) %>%
  pull(weather)

driver_mode <- cleaned_data %>%
  count(driver_distracted) %>%
  slice_max(n, n = 1) %>%
  pull(driver_distracted)

# Summary table using this improved mode method
summary_table <- data.frame(
  Variable = c("Speed.Limit", "vehicle_age", "injury", "weather", "driver_distracted"),
  
  Mean_or_Mode = c(
    round(mean(cleaned_data$Speed.Limit, na.rm = TRUE), 2),
    round(mean(cleaned_data$vehicle_age, na.rm = TRUE), 2),
    as.character(injury_mode),
    as.character(weather_mode),
    as.character(driver_mode)
  ),
  
  SD = c(
    round(sd(cleaned_data$Speed.Limit, na.rm = TRUE), 2),
    round(sd(cleaned_data$vehicle_age, na.rm = TRUE), 2),
    NA, NA, NA
  ),
  
  N = c(
    sum(!is.na(cleaned_data$Speed.Limit)),
    sum(!is.na(cleaned_data$vehicle_age)),
    sum(!is.na(cleaned_data$injury)),
    sum(!is.na(cleaned_data$weather)),
    sum(!is.na(cleaned_data$driver_distracted))
  )
)

print(summary_table)


#HISTOGRAMS

# Histogram for Speed.Limit
ggplot(cleaned_data, aes(x = Speed.Limit)) +
  geom_histogram(binwidth = 5, fill = "#2c7fb8", color = "white") +
  labs(title = "Distribution of Speed Limit", x = "Speed Limit (mph)", y = "Frequency") +
  theme_minimal()

# Histogram for Vehicle Age
ggplot(cleaned_data, aes(x = vehicle_age)) +
  geom_histogram(binwidth = 1, fill = "#f03b20", color = "white") +
  labs(title = "Distribution of Vehicle Age", x = "Vehicle Age (years)", y = "Frequency") +
  theme_minimal()

# Bar plot for Injury
ggplot(cleaned_data, aes(x = injury)) +
  geom_bar(fill = "#1b9e77") +
  labs(title = "Injury Count", x = "Injury (1 = injury, 0 = no injury)", y = "Count") +
  theme_minimal()

# Bar plot for Weather
ggplot(cleaned_data, aes(x = weather)) +
  geom_bar(fill = "#d95f02") +
  labs(title = "Weather Condition Count", x = "Weather", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for Driver Distraction
ggplot(cleaned_data, aes(x = driver_distracted)) +
  geom_bar(fill = "#7570b3") +
  labs(title = "Driver Distraction Count", x = "Driver Distracted (1 = Yes, 0 = No)", y = "Count") +
  theme_minimal()


# ------------------------------------------------------
# 3. Initial Regression
# ------------------------------------------------------
# Run logistic regression
logistic_model <- glm(injury ~ Speed.Limit + vehicle_age + weather + driver_distracted,
                      data = cleaned_data,
                      family = binomial(link = "logit"))

# View the summary
summary(logistic_model)

# Display odds ratios with confidence intervals
exp(cbind(OR = coef(logistic_model), confint(logistic_model)))

# Compute McFadden's Pseudo R^2
log_likelihood_model <- logLik(logistic_model)
log_likelihood_null <- logLik(glm(injury ~ 1, data = cleaned_data, family = binomial(link = "logit")))
pseudo_r2 <- 1 - (log_likelihood_model / log_likelihood_null)

#not like r squared in linear regression but ranges from 0 to around 1. compares it to null hypothesis or no preditors
#if there is an actual number the the model is better than if it had no predictors
cat("McFadden's Pseudo R^2:", round(as.numeric(pseudo_r2), 4), "\n")

# Compute LR Chi-Squared and p-value 
lr_chi2 <- -2 * (as.numeric(log_likelihood_null) - as.numeric(log_likelihood_model))
p_value <- pchisq(lr_chi2, df = length(coef(logistic_model)) - 1, lower.tail = FALSE)

#tells you that if theres something in this model that is worth looking at
cat("LR Chi^2:", round(lr_chi2, 4), "\n")
cat("Prob > chi^2:", round(p_value, 4), "\n")




# ------------------------------------------------------
# 4. Logistic Regression with interaction and transformed
# ------------------------------------------------------
#Square speed limit variable
cleaned_data$Speed.Squared <- cleaned_data$Speed.Limit^2

# Run logistic regression
logistic_model2 <- glm(injury ~ Speed.Limit + Speed.Squared + vehicle_age + weather + driver_distracted  + driver_distracted:vehicle_age ,
                      data = cleaned_data,
                      family = binomial(link = "logit"))

# View the summary
summary(logistic_model2)

# Display odds ratios with confidence intervals
exp(cbind(OR = coef(logistic_model2), confint(logistic_model2)))

# Compute McFadden's Pseudo R^2
log_likelihood_model2 <- logLik(logistic_model2)
log_likelihood_null2 <- logLik(glm(injury ~ 1, data = cleaned_data, family = binomial(link = "logit")))
pseudo_r2 <- 1 - (log_likelihood_model2 / log_likelihood_null2)

#not like r squared in linear regression but ranges from 0 to around 1. compares it to null hypothesis or no preditors
#if there is an actual number the the model is better than if it had no predictors
cat("McFadden's Pseudo R^2:", round(as.numeric(pseudo_r2), 4), "\n")

# Compute LR Chi-Squared and p-value 
lr_chi2 <- -2 * (as.numeric(log_likelihood_null2) - as.numeric(log_likelihood_model2))
p_value <- pchisq(lr_chi2, df = length(coef(logistic_model2)) - 1, lower.tail = FALSE)

#tells you that if theres something in this model that is worth looking at
cat("LR Chi^2:", round(lr_chi2, 4), "\n")
cat("Prob > chi^2:", round(p_value, 4), "\n")

# Check for multicollinearity using VIF
vif_values <- vif(logistic_model2)

# Display VIF results
print(vif_values)





