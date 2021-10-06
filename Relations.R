library(tidyverse)
library(dplyr)
library(data.table)
library(tidyr)
options(digits = 9)

# Read datasets
setwd('/Users/Teacher/Desktop/group_project_1/data/')
all_data <- read_csv("all_years.csv", na = c("", "NULL", "PrivacySuppressed"))


# Collect columns of interest
student_loans <- select(all_data ,c(
                                    'REGION',
                                    'LOAN_COMP_ORIG_YR8_RT',
                                    'LOAN_COMP_4YR_TRANS_YR8_RT',
                                    'LOAN_COMP_2YR_TRANS_YR8_RT'))

no_student_loans <- select(all_data ,c(
                                    'REGION',
                                    'NOLOAN_COMP_ORIG_YR8_RT',
                                    'NOLOAN_COMP_4YR_TRANS_YR8_RT',
                                    'NOLOAN_COMP_2YR_TRANS_YR8_RT'))

completion_rate <- select(all_data, c(
                                      'REGION',
                                      'C150_4',
                                      'C150_L4'))
                           
                           
# Summarize all

student_loans %>%
  group_by(REGION) %>%
  summarize_all(.funs = mean, na.rm=TRUE)

no_student_loans %>%
  group_by(REGION) %>%
  summarize_all(.funs = mean, na.rm=TRUE)

completion_rate %>%
  group_by(REGION) %>%
  summarize_all(.funs = mean, na.rm=TRUE)


# Set up variable for plotting:
df_tab1 <- student_loans %>%
  group_by(REGION) %>%
  summarize_all(.funs = mean, na.rm=TRUE) %>%
  select('LOAN_COMP_ORIG_YR8_RT',
         'LOAN_COMP_4YR_TRANS_YR8_RT',
         'LOAN_COMP_2YR_TRANS_YR8_RT') %>%
  as.matrix()

df_tab2 <- no_student_loans %>%
  group_by(REGION) %>%
  summarize_all(.funs = mean, na.rm=TRUE) %>%
  select('NOLOAN_COMP_ORIG_YR8_RT',
         'NOLOAN_COMP_4YR_TRANS_YR8_RT',
         'NOLOAN_COMP_2YR_TRANS_YR8_RT') %>%
  as.matrix()

df_tab3 <- completion_rate %>%
  group_by(REGION) %>%
  summarize_all(.funs = mean, na.rm=TRUE) %>%
  select('C150_4',
         'C150_L4') %>%
  as.matrix()

# Plot
barplot(df_tab1, beside=TRUE)
barplot(df_tab2, beside=TRUE)
barplot(df_tab3, beside=TRUE)
