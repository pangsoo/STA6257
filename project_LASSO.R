library(tidyverse)
library(plyr)
library(tidyverse)
library(magrittr)
#install.packages("forecast")
library(forecast)
library(zoo)
#install.packages("openxlsx")
#install.packages("xlsx")
library(readxl)
library(openxlsx) # Github
library(xlsx)
#install.packages("RCurl")
library(RCurl) # from Github
getwd()
setwd("E:/UWF/STA6257")

# File download from Github
dir.create("~/sub/")
url <- "https://github.com/pangsoo/STA6257/blob/ce961941a15da8a8115aa3e26d11d58f518f1d76/train_genetics.xlsx"
download.file(url, destfile = "~/train_genetics.xlsx")
df <- read_excel(url)

df <- read_excel("train_genetics.xlsx")
head(df)


dim(df)
glimpse(df)
# Erase irrelevant variables
df <- df[, !names(df) %in% c("Patient Id", "Patient First Name", "Family Name", "Father's name", "Institute Name", "Location of Institute", "Test 1", "Test 2", "Test 3", "Test 4", "Test 5", "Symptom 1", "Symptom 2", "Symptom 3", "Symptom 4", "Symptom 5", "Parental consent", "Follow-up", "H/O radiation exposure (x-ray)", "H/O substance abuse", "Birth asphyxia")]
glimpse(df)

# Missing value check
#install.packages("naniar")
library(naniar)
#install.packages("VIM")

naniar::miss_var_summary(df)

VIM::aggr(df,prop=FALSE,numbers=TRUE)

#df <- df %>% filter(!is.na(df$`Mother's age`))
#df <- df %>% filter(!is.na(df$`Father's age`))
#VIM::aggr(df,prop=FALSE,numbers=TRUE)
#naniar::miss_var_summary(df)

dt <- na.omit(df)
dim(dt)
sum(is.na(dt))

library(data.table)
is.data.table(dt) # to see if data.table
dt <- as.data.table(dt)

# Convert Categorical data into numerical data
#install.packages("mltools")
library(mltools)


# List of columns to convert (excluding already numeric columns and ID)

#columns_to_convert <- c("Status", "Respiratory Rate (breaths/min)", "Heart Rate (rates/min", "Autopsy shows birth defect (if applicable)", "Place of birth", "Assisted conception IVF/ART", "History of anomalies in previous pregnancies","Birth defects", "Blood test result", "Genetic Disorder", "Disorder Subclass", "Gender")

# 1. Convert Yes/No columns to 1/0
# Function to convert
convert_to_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  
  factor_x <- as.factor(x)
  numeric_x <- as.numeric(factor_x)
  
  # Check if it's a binary variable
  if (length(unique(numeric_x)) == 2) {
    # Subtract 1 to convert 1 and 2 to 0 and 1
    return(numeric_x - 1)
  }
  
  # For non-binary variables, return as is
  return(numeric_x)
}

yes_no_columns <- c("Genes in mother's side", "Inherited from father", "Maternal gene", "Paternal gene", "Folic acid details (peri-conceptional)", "H/O serious maternal illness")

dt[, (yes_no_columns) := lapply(.SD, function(x) as.numeric(x == "Yes")), .SDcols = yes_no_columns]


# 2. Special handling for "Respiratory Rate" and "Heart Rate" columns
# 2-1 Respiratory Rate of 2 category
dt[, `Respiratory Rate (breaths/min)` := 
     fcase(
       `Respiratory Rate (breaths/min)` == "Normal (30-60)", 0L,
       `Respiratory Rate (breaths/min)` == "Tachypnea", 1L,
       default = NA_integer_
     )]

# 2-2. Heart Rate
dt[, `Heart Rate (rates/min` := 
     fcase(
       `Heart Rate (rates/min` == "Normal", 0L,
       `Heart Rate (rates/min` == "Tachycardia", 1L,
       default = NA_integer_
     )]

# 2-3. Gender
dt[, `Gender` := 
     fcase(
       `Gender` == "Male", 1L,
       `Gender` == "Female", 2L,
       `Gender` == "Ambiguous", 3L,
       default = NA_integer_
     )]

# Convert the rest
rest_to_convert <- c("Status",  "Autopsy shows birth defect (if applicable)", "Place of birth", "Assisted conception IVF/ART", "History of anomalies in previous pregnancies","Birth defects", "Blood test result", "Genetic Disorder", "Disorder Subclass")

# Convert function
convert_to_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  
  factor_x <- as.factor(x)
  numeric_x <- as.numeric(factor_x)
  
  # Check if it's a binary variable
  if (length(unique(numeric_x)) == 2) {
    # Subtract 1 to convert 1 and 2 to 0 and 1
    return(numeric_x - 1)
  }
  
  # For non-binary variables, return as is
  return(numeric_x)
}

dt[, (rest_to_convert) := lapply(.SD, convert_to_numeric), .SDcols = rest_to_convert]


VIM::aggr(dt,prop=FALSE,numbers=TRUE)


# EDA
# graphs
library(ggplot2)
library(reshape2)
library(tidyr)
#install.packages("C:/temp/ggplot2_3.1.1.zip", repos = NULL, type = "source")
#library(purr)
#install.packages("purrr")
library(purrr)


dt %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# creating correlation matrix
corr_mat <- round(cor(dt),2)
melted_corr_mat <- melt(corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + 
  geom_tile()


# Setting the Variables
X <- as.matrix(dt[, 0:22])
X <- as.data.table(X) # to data table 
y <- dt$`Genetic Disorder`
#y <- as.matrix(dt[, -1]) # 2nd try 

#install.packages("BiocManager", repos = "https://cloud.r-project.org")

#install.packages("caret", dependencies=TRUE)
library(caret)

#install.packages("glmnet")
library(glmnet)

# Scaling the continuous variables
continuous_cols <- c("Patient Age", "Blood cell count (mcL)", "Mother's age", "Father's age", "White Blood cell count (thousand per microliter)")

X[, (continuous_cols) := lapply(.SD, scale), .SDcols = continuous_cols]



# Splitting the data in to training and test sets

X <- as.matrix(X)
set.seed(100)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
train_x <- X[train_index, ]
test_x <- X[-train_index,]
train_y <- y[train_index]
test_y <- y[-train_index]


# alpha value finding
lasso_1 <- glmnet(train_x, train_y, alpha=1)
lasso_0 <- glmnet(train_x, train_y, alpha=0)
lasso_05 <- glmnet(train_x, train_y, alpha=0.5)


# Prediction
prediction_1 <- predict(lasso_1, newx = test_x, s = 0.01)
prediction_0 <- predict(lasso_0, newx = test_x, s = 0.01)
prediction_05 <- predict(lasso_05, newx = test_x, s = 0.01)

# Evaluating (RMSE)
rmse_1 <- sqrt(mean((prediction_1 - test_y)^2))
rmse_0 <- sqrt(mean((prediction_0 - test_y)^2))
rmse_05 <- sqrt(mean((prediction_05 - test_y)^2))

# The results
cat("Lasso Regression RMSE:", rmse_1, "\n")
cat("Lasso Regression RMSE:", rmse_0, "\n")
cat("Lasso Regression RMSE:", rmse_05, "\n")
plot(lasso, xvar = "lambda", main = "Lasso Coefficients Path")

# Little difference in alpha value. Next, finding lambda value
#k-fold cross-validation to find an optimal lambda value
cv <- cv.glmnet(X, y, alpha = 1)

lambda_best <- cv$lambda.min
lambda_best #0.01646 

plot(cv)

# Find coefficient of a model

model <- glmnet(X, y, alpha=1, lambda = lambda_best)
coef(model) # result: status, autopsy, blood test are predictors

# The model
y_predicted <- predict(model, s = lambda_best, newx = X)
# RMSE, R2
rmse <- sqrt(mean((y_predicted - y)^2))
sst <- sum((y-mean(y))^2)
sse <- sum((y_predicted - y)^2)
r2 <- 1- sse/sst
cat("Lasso Regression RMSE:", rmse, "\n")
cat("Lasso Regression  R2:", r2, "\n")
plot(y_predicted, ols.)

# The model is not valid with the result. We turn to other method. 
# Linear regression

#X_lin <- subset(X, select = c("Status", "Autopsy shows birth defect (if applicable)", "Blood test result"))

#lin_df <- cbind(X_lin, y)

linear_model <- lm(formula = Genetic_Disorder ~ Status + Autopsy shows_birth_defect_(if applicable) + Blood test_result, data = lin_df)

X <- as.data.table(X)
linear_model <- lm(formula = y ~ Status + X$`Blood test result` + X$`Autopsy shows birth defect (if applicable)`, data = X)
linear_model

model_residuals <- linear_model$residuals
hist(model_residuals)

# Plot the residuals
qqnorm(model_residuals)
# Plot the Q-Q line
qqline(model_residuals) # not fit to linear model

# LASSO model with categorical variables
# Collect only binary datasets
binary_cols <- names(dt)[sapply(dt, function(x) all(x %in% c(0, 1)))]
X <- dt[, ..binary_cols]

# convert to matrix
X <- as.matrix(X)
y <- as.factor(dt$`Genetic Disorder`) 
combined_matrix <- cbind(X, y)

set.seed(100)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
train_X <- combined_matrix[train_index, ]
train_y <- combined_matrix[-train_index,]
test_X <- combined_matrix[-train_index, ]
test_y <- combined_matrix[-train_index]


# Fit the LASSO
lasso_bin <- cv.glmnet(train_X, train_y, family='multinomial', alpha=1)

head(combined_matrix)
tail(combined_matrix)
dim(combined_matrix)
