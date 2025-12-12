print("Hello GitHub from RStudio!")
print("Hello GitHub from RStudio!")

library(dplyr)
setwd("Desktop/teamwork")


#q1
dat <- Complete_Data_Set  # 你的数据框名字按实际改

# 把 Y/N 变成 1/0
dat$belief_worse <- ifelse(dat$Belief == "Y", 1, 0)

# 定义 ongoing health issue：只要 PH 或 MH 有一个是 Y 就算有
dat$ongoing_health <- ifelse(dat$PH == "Y" | dat$MH == "Y", 1, 0)

# 快速检查
table(dat$Belief, useNA = "ifany")
table(dat$belief_worse, dat$ongoing_health, useNA = "ifany")

fit1 <- glm(belief_worse ~ ongoing_health,
            family = binomial(),
            data = dat)

summary(fit1)

#q2
data <- read.csv("Complete Data Set.csv")
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)  # For tidy model outputs
library(readr)  # For reading CSV with better handling


# Assuming the data is in a file named "Complete Data Set.csv"
# If the data is provided as text, you can use read_csv(text = "...") but here we assume file access
data <- read_csv("Complete Data Set.csv", col_types = cols(
  Age = col_character(),  # Age has numbers and NA, so read as char initially
  PH = col_character(),
  MH = col_character(),
  Smoker = col_character(),
  Belief = col_character(),
  Gender = col_character()
))

# Data cleaning
# Standardize PH and MH to binary: 1 for Yes/Y/YES, 0 for No/N/NO, NA for others/empty
data <- data %>%
  mutate(
    Age = as.numeric(ifelse(Age %in% c("NA", ""), NA, Age)),  # Convert Age to numeric, handle NA
    PH_binary = case_when(
      toupper(PH) %in% c("Y", "YES") ~ 1,
      toupper(PH) %in% c("N", "NO") ~ 0,
      TRUE ~ NA_real_
    ),
    MH_binary = case_when(
      toupper(MH) %in% c("Y", "YES") ~ 1,
      toupper(MH) %in% c("N", "NO") ~ 0,
      TRUE ~ NA_real_
    ),
    Smoker_binary = case_when(
      toupper(Smoker) %in% c("Y", "YES", "CURRENT") ~ 1,
      toupper(Smoker) %in% c("N", "NO") ~ 0,
      TRUE ~ NA_real_
    ),
    Belief_binary = case_when(
      toupper(Belief) %in% c("Y", "YES") ~ 1,
      toupper(Belief) %in% c("N", "NO") ~ 0,
      TRUE ~ NA_real_
    ),
    Gender_clean = case_when(
      toupper(Gender) %in% c("M") ~ "Male",
      toupper(Gender) %in% c("F") ~ "Female",
      toupper(Gender) %in% c("PNTS", "GD", "NA") ~ "Other/NA",
      TRUE ~ "Other/NA"
    )
  ) %>%
  filter(!is.na(Age))  # Optional: Remove rows with NA Age, adjust as needed

# Summary statistics for prevalence without adjustment
prevalence_summary <- data %>%
  summarise(
    PH_prevalence = mean(PH_binary, na.rm = TRUE) * 100,
    MH_prevalence = mean(MH_binary, na.rm = TRUE) * 100,
    Total_n = n(),
    PH_n = sum(!is.na(PH_binary)),
    MH_n = sum(!is.na(MH_binary))
  )

print("Unadjusted Prevalence:")
print(prevalence_summary)

# Stratified prevalence by key factors (e.g., Gender, Smoker)
stratified_ph <- data %>%
  group_by(Gender_clean, Smoker_binary) %>%
  summarise(PH_prevalence = mean(PH_binary, na.rm = TRUE) * 100, n = n()) %>%
  ungroup()

print("Stratified PH Prevalence:")
print(stratified_ph)

stratified_mh <- data %>%
  group_by(Gender_clean, Smoker_binary) %>%
  summarise(MH_prevalence = mean(MH_binary, na.rm = TRUE) * 100, n = n()) %>%
  ungroup()

print("Stratified MH Prevalence:")
print(stratified_mh)

# Adjusted prevalence using logistic regression
# Model for PH, adjusting for Age, Gender, Smoker, Belief
ph_model <- glm(PH_binary ~ Age + Gender_clean + Smoker_binary + Belief_binary, 
                data = data, family = binomial(link = "logit"))

print("Adjusted Model for PH:")
print(summary(ph_model))

# Tidy output for odds ratios
ph_tidy <- tidy(ph_model, exponentiate = TRUE, conf.int = TRUE)
print(ph_tidy)

# Model for MH
mh_model <- glm(MH_binary ~ Age + Gender_clean + Smoker_binary + Belief_binary, 
                data = data, family = binomial(link = "logit"))

print("Adjusted Model for MH:")
print(summary(mh_model))

mh_tidy <- tidy(mh_model, exponentiate = TRUE, conf.int = TRUE)
print(mh_tidy)
