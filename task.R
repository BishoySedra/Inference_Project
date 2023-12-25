library(caret) # for linear regression model
library(rpart) # for decision tree model

# 1.935336e-14 => linear regression model rmse before scaling and encoding
# 2.516931 => decision tree model rmse before scaling and encoding

# 3.252172e-15   => linear regression model rmse after scaling and encoding
# 0.455449  => decision tree model rmse after scaling and encoding

file <- read.csv("StudentsPerformance.csv")

summary(file)

# count nulls for every column
colSums(is.na(file))

# START PREPROCESSING

# (1) Replacement 'M' -> 'Male' / 'F' -> 'Female'
file$sex[file$sex=="F"]="Female"
file$sex[file$sex=="f"]="Female"
file$sex[file$sex=="M"]="Male"
file$sex[file$sex=="m"]="Male"


# (2) Handle outliers function
handle_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  outlier_threshold <- 1.5 * IQR_value
  outliers <- column < (Q1 - outlier_threshold) | column > (Q3 + outlier_threshold)
  column[column < (Q1 - outlier_threshold)] <- Q1 - outlier_threshold
  column[column > (Q3 + outlier_threshold)] <- Q3 + outlier_threshold
  return(column)
}

file$age <- handle_outliers(file$age)
file$absences <- handle_outliers(file$absences)
file$goout <- handle_outliers(file$goout)
file$studytime <- handle_outliers(file$studytime)
file$failures <- handle_outliers(file$failures)
file$health <- handle_outliers(file$health)


# (3) replace missing values in internet column with the mode
file$internet <- ifelse(file$internet == "", names(sort(table(file$internet), decreasing = TRUE)[1]), file$internet)

# (4) Scale values of columns G1, G2, G3
file[, c("G1", "G2", "G3")] <- scale(file[, c("G1", "G2", "G3")])

# (5) feature engineering
file$Total_average <- (file$G1  + file$G2 + file$G3)/3

# (6) feature selection
# dropping failures -> after deleting outliers as it has the same value in all records
columns_to_delete <- c("G1", "G2", "G3", "X", "failures")
file <- file[, !names(file) %in% columns_to_delete]

# (7) Encode categorical variables
file$school <- as.factor(file$school)
file$sex <- as.factor(file$sex)
file$Fjob <- as.factor(file$Fjob)
file$Mjob <- as.factor(file$Mjob)
file$internet <- as.factor(file$internet)
file$romantic <- as.factor(file$romantic)

## Create dummy variables
file_dummies <- model.matrix(~ . - 1, data = file)

## Combine the dummy variables with the original dataset
file <- cbind(file, file_dummies)

## Remove the original categorical columns
file <- file[, !names(file) %in% c('school', 'sex', 'Fjob', 'Mjob', 'internet', 'romantic')]

# (8) delete duplicates
file <- unique(file)

# END PREPROCESSING

# display the statistics of the modified dataset
summary(file)

# ============================================================

# START visualization

# Scatterplot matrix
pairs(file[, c("age", "studytime", "absences", "goout", "health", "Total_average")], main = "Scatterplot Matrix")

# plot
plot(file$studytime, file$Total_average , main = "PERFORMANCE [Study Time-Total Average] " ,xlab = "Study Time" , ylab = "Total Average")
plot(file$absences, file$Total_average, main = "PERFORMANCE [Absences-Total Average] " ,xlab = "Absences" , ylab = "Total Average")
plot(file$age, file$Total_average, main = "PERFORMANCE [Age-Total Average] " ,xlab = "Absences" , ylab = "Total Average")
plot(file$goout, file$Total_average, main = "PERFORMANCE [Going out-Total Average] " ,xlab = "Absences" , ylab = "Total Average")
plot(file$health, file$Total_average, main = "PERFORMANCE [Health-Total Average] " ,xlab = "Absences" , ylab = "Total Average")

# histogram
hist(file$studytime, col="lightblue", xlab = "Study Time", main = "Study Time Histogram")
hist(file$absences, col="lightblue", xlab = "Absences", main = "Absences Histogram")
hist(file$age, col="lightblue", xlab = "Age", main = "Age Histogram")
hist(file$goout, col="lightblue", xlab = "Going out", main = "Going out Histogram")
hist(file$Total_average, col="lightblue", xlab = "Total Average", main = "Total Average Histogram")

# boxplot
boxplot(file$Total_average, col="lightblue", ylab = "Total Average", main = "Total Average Boxplot")
boxplot(file$absences, col="lightblue", ylab = "Absences", main = "Absences Boxplot")
boxplot(file$age, col="lightblue", ylab = "Age", main = "Age Boxplot")
boxplot(file$goout, col="lightblue", ylab = "Going out", main = "Going out Boxplot")
boxplot(file$Total_average, col="lightblue", ylab = "Total Average", main = "Total Average Boxplot")

# End Visualization

# ==============================================================

# START Models

#Start Linear Regression

# Prepare data for linear regression modeling

## for reproducibility and the same random sample to be trained and tested
set.seed(123)

## splitting into train and test data
splitIndex <- createDataPartition(file$Total_average, p = 0.7, list = FALSE)
train_data <- file[splitIndex, ]
test_data <- file[-splitIndex, ]

## Shuffle train_data to ensure that the data is randomly distributed across the training and testing sets
shuffled_train_data <- train_data[sample(nrow(train_data)), ]

## Train linear regression model
model <- lm(Total_average ~ ., data = shuffled_train_data)

## Print model summary
summary(model)

## Make predictions on the test set
predictions <- predict(model, newdata = test_data)

## Evaluate model performance
### NOTE: The lower the RMSE and MSE, the better the model and its predictions.

### RMSE
linear_rmse <- sqrt(mean((test_data$Total_average - predictions)^2))

### MSE
linear_mse <- mean((test_data$Total_average - predictions)^2)

cat("Root Mean Squared Error (RMSE) for linear regression model:", linear_rmse, "\n")
cat("Mean Squared Error (MSE) for linear regression model:", linear_mse, "\n")

#End Linear Regression

#==========================================================

# Start Decision Tree

## Assuming 'Total_average' is your target variable
target_class_label <- "Total_average"

## Split the data into training and testing sets (you can use a different method)

## for reproducibility and the same random sample to be trained and tested
set.seed(123)

## splitting into train and test data
train_indices <- sample(seq_len(nrow(file)), 0.8 * nrow(file))
train_data <- file[train_indices, ]
shuffled_train_data <- train_data[sample(nrow(train_data)), ]
test_data <- file[-train_indices, ]

## Shuffle test_data
shuffled_test_data <- test_data[sample(nrow(test_data)), ]

## Train the decision tree model
model <- rpart(formula = paste(target_class_label, "~ ."), data = shuffled_train_data)

## Make predictions on the test set
predictions <- predict(model, newdata = shuffled_test_data)

## Evaluate the model (you can use different metrics)

### RMSE
tree_rmse <- sqrt(mean((shuffled_test_data$Total_average - predictions)^2))

### MSE
tree_mse <- mean((shuffled_test_data$Total_average - predictions)^2)

cat("Root Mean Squared Error (RMSE) for decision tree model:", tree_rmse, "\n")
cat("Mean Squared Error (MSE) for decision tree model:", tree_mse, "\n")

## Visualize the decision tree
plot(model)
text(model, cex = 0.5)

# End Decision Tree
