# Install and load necessary libraries if not already installed
install.packages("tidyverse")  # for data manipulation and visualization
install.packages("broom")      # for tidying model output
install.packages("car")        # for Chi-Square testing
install.packages("lubridate")  # for timeseries
install.packages("forecast")
install.packages("dplyr")
install.packages("survival")   # for Surval analysis
install.packages("ggplot2")

# Load libraries
library(readxl)
library(tidyverse)
library(broom)
library(car)
library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)
library(survival)

#Assign data
CapstoneData <- read_excel("C:/Users/rmadole/OneDrive - OmniTRAX, Inc/Documents/School/MIS581/CapstoneData.xlsx",
                           col_types = c("text", "skip", "text",
                                         "date", "date", "text", "text", "numeric"))
incident_data <- CapstoneData

#View Structure
str(incident_data)

#Summary of Dataset
summary(incident_data)

#cross-tabulation of Primary Cause and FRA Reportable
threshold <- 1
binary_vector <- as.integer(incident_data$FRA_Reportable >= threshold)
FRA_labels <- ifelse(binary_vector == 0,"No","Yes")
table(incident_data$PrimaryCause, FRA_labels)

#Visualize the relationship
ggplot(incident_data, aes(x = FRA_labels, fill = PrimaryCause)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of FRA Reportable Incidents by Primary Cause")

# Fit Logistic regression Model
logit_model <- glm(FRA_Reportable ~ PrimaryCause, data = incident_data, family = "binomial")

#summarize model
summary(logit_model)

#Interpret the coefficients
exp(coef(logit_model))  # Exponentiated coefficients as odds ratios

# Predict probabilities for specific scenarios (e.g., "human factors" as PrimaryCause)
predict(logit_model, newdata = data.frame(PrimaryCause = "Human Factor"), type = "response")

#Create Contingency Table
contingency_table <- table(incident_data$PrimaryCause,incident_data$FRA_Reportable)

#Chi-Square test
chisq_test <- chisq.test(contingency_table)
chisq_test

# calculate Cramer's V for effect size
cramer_v <- sqrt(chisq_test$statistic / sum(contingency_table))
cramer_v

#Convert Loss Date to date format
#incident_data$LossDate <- as.Date(incident_data$LossDate)

#aggregate incidents by month
monthlycounts <- incident_data %>%
  group_by(LossDate) %>%
  summarise(IncidentCount = n()) %>%
  ungroup()

#create time series

ts_data <- ts(monthlycounts$IncidentCount, frequency = 12, start = c(
  year(
    min
       (monthlycounts$LossDate)),
  month(min
        (monthlycounts$LossDate))
  ))


# Plot the timeseries
autoplot(ts_data) +
  labs(title = "Monthly Incident Frequency over Time")

# Decompose the time series to analyze trend, seasonality, and noise
decom <- decompose(ts_data) 
plot(decom)

# Compute Incident Frequency
incident_frequency <- incident_data %>%
  group_by(Claim) %>%
  summarise(IncidentCount = n_distinct(Claim))
print(incident_frequency)

# Assuming 'incidents_data' contains ClaimNum and LossDate (date of incident)

# Sort data by Claim and LossDate
incidents_data <- incident_data[order(incident_data$Claim, incident_data$LossDate), ]

# Calculate time difference between consecutive incidents for each Claim
incidents_data$TimeToNextIncident <- c(NA, diff(incidents_data$LossDate))

# Define a binary indicator for subsequent incidents within a certain timeframe
incidents_data$SubsequentIncident <- ifelse(incidents_data$TimeToNextIncident < 30, 1, 0)

# Example: Exploratory Data Analysis
# Assuming 'incident_frequency' is already calculated

# Visualize incident frequency distribution
hist(incident_frequency$IncidentCount, main = "Incident Frequency Distribution", xlab = "Incident Count")

# Explore relationships between IncidentCount and other variables
plot(incident_frequency$IncidentCount, incidents_data$SomeOtherVariable, 
     main = "Relationship between Incident Count and Some Other Variable", 
     xlab = "Incident Count", ylab = "Some Other Variable")

# Merge data
merged_data <- merge(incident_frequency, incidents_data, by = "Claim", all.x = TRUE)

# Check the structure of merged_data
str(merged_data)

# Handle missing values (if necessary)
merged_data1 <- na.omit(merged_data[, c("IncidentCount", "SubsequentIncident")])
str(merged_data1)

# Calculate correlation between IncidentCount and SubsequentIncidents
length(merged_data1$SubsequentIncident)
length(merged_data1$SubsequentIncident)
correlation_result <- cor(merged_data1$IncidentCount, merged_data1$SubsequentIncident)

# Print correlation coefficient
print(correlation_result)

# Survival Analysis
surv_object <- Surv(time = incidents_data$TimeToNextIncident, event = incidents_data$SubsequentIncident)

# Fit survival model 
cox_model <- coxph(surv_object ~ IncidentCount, data = merged_data)


cox_model1 <- coxph(surv_object ~ PrimaryCause, data = incidents_data)

#Summary of model
summary(cox_model)
summary(cox_model1)

# ANOVA Model
model_anova <- aov(IncidentCount ~ Road, data = merged_data)

# Results
summary(model_anova)

#Post-hoc analysis
posthoc<- TukeyHSD(model_anova)
print(posthoc)

# Kruskal-Wallis Test
kruskal_test <- kruskal.test(IncidentCount ~ Road, data = merged_data)
print(kruskal_test)
