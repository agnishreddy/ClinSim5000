# Simulating a large clinical trial dataset in R
# Load necessary libraries
library(tidyverse)

# Set seed for reproducibility
set.seed(123)

# Parameters for the dataset
n_patients <- 5000  # Number of patients

generate_clinical_trial_data <- function(n) {
  data.frame(
    PatientID = sprintf("P%05d", 1:n),  # Unique Patient IDs
    Age = sample(18:90, n, replace = TRUE),
    Gender = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.48, 0.52)),
    TreatmentGroup = sample(c("Drug A", "Drug B", "Placebo"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
    BaselineBP = round(rnorm(n, mean = 120, sd = 15), 1),
    FollowupBP = round(rnorm(n, mean = 118, sd = 15), 1),
    AdverseEvent = sample(c("None", "Mild", "Moderate", "Severe"), n, replace = TRUE, prob = c(0.7, 0.2, 0.08, 0.02)),
    Response = sample(c("Responder", "Non-Responder"), n, replace = TRUE, prob = c(0.6, 0.4))
  )
}

# Generate the dataset
clinical_trial_data <- generate_clinical_trial_data(n_patients)

# Save to CSV for external use
write.csv(clinical_trial_data, "clinical_trial_data.csv", row.names = FALSE)

# Summarize the dataset
summary(clinical_trial_data)

# Example analyses

# 1. Demographic Summary
demographics_summary <- clinical_trial_data %>%
  group_by(Gender) %>%
  summarise(
    Count = n(),
    MeanAge = mean(Age, na.rm = TRUE),
    .groups = "drop"
  )
print(demographics_summary)

# 2. Treatment Effect Analysis
treatment_effect <- clinical_trial_data %>%
  group_by(TreatmentGroup) %>%
  summarise(
    MeanBaselineBP = mean(BaselineBP, na.rm = TRUE),
    MeanFollowupBP = mean(FollowupBP, na.rm = TRUE),
    .groups = "drop"
  )
print(treatment_effect)

# 3. Adverse Event Distribution
adverse_events_summary <- clinical_trial_data %>%
  group_by(AdverseEvent) %>%
  summarise(
    Count = n(),
    Proportion = n() / n_patients,
    .groups = "drop"
  )
print(adverse_events_summary)

# Visualizations

# Age distribution by gender
library(ggplot2)
ggplot(clinical_trial_data, aes(x = Age, fill = Gender)) +
  geom_histogram(bins = 30, position = "dodge") +
  labs(title = "Age Distribution by Gender", x = "Age", y = "Count") +
  theme_minimal()

# Blood pressure change by treatment group
ggplot(clinical_trial_data, aes(x = TreatmentGroup, y = FollowupBP - BaselineBP, fill = TreatmentGroup)) +
  geom_boxplot() +
  labs(title = "Blood Pressure Change by Treatment Group", x = "Treatment Group", y = "Change in BP") +
  theme_minimal()

# Save visualizations
# ggsave("age_distribution_by_gender.png")
# ggsave("bp_change_by_treatment.png")
