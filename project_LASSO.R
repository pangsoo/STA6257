library(tidyverse)
library(plyr)
library(readxl)
getwd()
setwd("E:/UWF/STA6257")

df <- read_excel("train_genetics.xlsx")
dim(df)
glimpse(df)
df <- df[, !names(df) %in% c("Patient Id", "Patient First Name", "Family Name", "Father's name", "Institute Name", "Location of Institute", "Test 1", "Test 2", "Test 3", "Test 4", "Test 5", "Symptom 1", "Symptom 2", "Symptom 3", "Symptom 4", "Symptom 5", "Parental consent", "Follow-up", "H/O radiation exposure (x-ray)", "H/O substance abuse", "Birth asphyxia")]
glimpse(df)

# Missing value check
install.packages("naniar")
library(naniar)
install.packages("VIM")

naniar::miss_var_summary(df)

VIM::aggr(df,prop=FALSE,numbers=TRUE)

#df <- df %>% filter(!is.na(df$`Mother's age`))
#df <- df %>% filter(!is.na(df$`Father's age`))
#VIM::aggr(df,prop=FALSE,numbers=TRUE)
#naniar::miss_var_summary(df)

dt <- na.omit(df)
dim(dt)
sum(is.na(dt))

is.data.table(dt) # to see if data.table
dt <- as.data.table(dt)

# Convert Categorical data into numerical data
install.packages("mltools")
library(mltools)
library(data.table)

# Function to convert categorical variables to numeric
convert_to_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  factor_x <- as.factor(x)
  as.numeric(factor_x)
}

# List of columns to convert (excluding already numeric columns and ID)

columns_to_convert <- c("Status", "Respiratory Rate (breaths/min)", "Heart Rate (rates/min", "Autopsy shows birth defect (if applicable)", "Place of birth", "Assisted conception IVF/ART", "History of anomalies in previous pregnancies","Birth defects", "Blood test result", "Genetic Disorder", "Disorder Subclass", "Gender")

# Convert specified columns to numeric
dt[, (columns_to_convert) := lapply(.SD, convert_to_numeric), .SDcols = columns_to_convert]

# Special handling for "Respiratory Rate" and "Heart Rate" columns
dt[, `Respiratory Rate (breaths/min)` := as.numeric(sub("Normal \\(30-60\\)", "Tachycardia", `Respiratory Rate (breaths/min)`))]
dt[, `Heart Rate (rates/min` := as.numeric(sub("Normal", "Tachycardia", `Heart Rate (rates/min`))]
dt[, `Gender` := as.numeric(sub("Male", "Female", `Gender`))]


# Convert Yes/No columns to 1/0
yes_no_columns <- c("Genes in mother's side", "Inherited from father", "Maternal gene", "Paternal gene", "Folic acid details (peri-conceptional)", "H/O serious maternal illness")

dt[, (yes_no_columns) := lapply(.SD, function(x) as.numeric(x == "Yes")), .SDcols = yes_no_columns]


VIM::aggr(dt,prop=FALSE,numbers=TRUE)

dt

# EDA
# graphs
library(ggplot2)
library(reshape2)
library(tidyr)
library(purr)


dt %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# creating correlation matrix
corr_mat <- round(cor(dt),2)
melted_corr_mat <- melt(corr_mat)

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + 
  geom_tile()

