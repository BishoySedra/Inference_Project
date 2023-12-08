library(caret) # for linear regression model
library(rpart) # for decision tree model

# 1.935336e-14 => linear regression model rmse before scaling and encoding
# 2.516931 => decision tree model rmse before scaling and encoding

# 3.252172e-15   => linear regression model rmse after scaling and encoding
# 0.455449  => decision tree model rmse after scaling and encoding

file <- read.csv("StudentsPerformance.csv")
summary(file)

# START PREPROCESSING

#replacement 'M' -> 'Male' / 'F' -> 'Female'
file$sex[file$sex=="F"]="Female"
file$sex[file$sex=="f"]="Female"
file$sex[file$sex=="M"]="Male"
file$sex[file$sex=="m"]="Male"
#file <- na.omit(file)


#data type convertion for age column as it was a char class before convertion
file$age <- as.numeric(file$age)


# Handle outliers function
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

# Handle outliars for 'age' and 'absences'
file$age <- handle_outliers(file$age)
file$absences <- handle_outliers(file$absences)


#replace with empty cells with 'no'

file$internet <- ifelse(file$internet == "", "no", file$internet)
## cat("Number of empty values in the 'Internet' column after replacement:", sum(file$internet == ""), "\n")

# Scale values of columns G1, G2, G3
file[, c("G1", "G2", "G3")] <- scale(file[, c("G1", "G2", "G3")])


#feature engineering
file$Total_of_grades <- file$G1  + file$G2 + file$G3

# Encode categorical variables
file$school <- as.factor(file$school)
file$sex <- as.factor(file$sex)
file$Fjob <- as.factor(file$Fjob)
file$Mjob <- as.factor(file$Mjob)
file$internet <- as.factor(file$internet)
file$romantic <- as.factor(file$romantic)

# Create dummy variables
file_dummies <- model.matrix(~ . - 1, data = file)

# Combine the dummy variables with the original dataset
file <- cbind(file, file_dummies)

# Remove the original categorical columns
file <- file[, !names(file) %in% c('school', 'sex', 'Fjob', 'Mjob', 'internet', 'romantic')]

# delete duplicates
file <- unique(file)

# END PREPROCESSING

# display the statistics of the modified dataset
summary(file)

# START visualization

# Scatterplot matrix
pairs(file[, c("age", "studytime", "failures", "absences", "Total_of_grades")], main = "Scatterplot Matrix")

plot(file$studytime, file$Total_of_grades , main = "PERFORMANCE [Study Time-Total Grades] " ,xlab = "Study Time" , ylab = "Total Grades")
plot(file$failures, file$Total_of_grades, main = "PERFORMANCE [Failures-Total Grades] " ,xlab = "Failures" , ylab = "Total Grades")
plot(file$absences, file$Total_of_grades, main = "PERFORMANCE [Absences-Total Grades] " ,xlab = "Absences" , ylab = "Total Grades")

# End Visualization

# Prepare data for linear regression modeling
set.seed(123)  # for reproducibility
splitIndex <- createDataPartition(file$Total_of_grades, p = 0.7, list = FALSE)
train_data <- file[splitIndex, ]
test_data <- file[-splitIndex, ]

# Train linear regression model
model <- train(Total_of_grades ~ ., data = train_data, method = "lm")

# Print model summary
# print('model information:')
# print(model)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Evaluate model performance
linear_rmse <- RMSE(predictions, test_data$Total_of_grades)
cat("Root Mean Squared Error (RMSE) for linear regression model:", linear_rmse, "\n")

# Assuming 'Total_of_grades' is your target variable
target_variable <- "Total_of_grades"

# Split the data into training and testing sets (you can use a different method)
set.seed(123)  # For reproducibility
train_indices <- sample(seq_len(nrow(file)), 0.8 * nrow(file))
train_data <- file[train_indices, ]
test_data <- file[-train_indices, ]

# Train the decision tree model
model <- rpart(formula = paste(target_variable, "~ ."), data = train_data)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Evaluate the model (you can use different metrics)
tree_rmse <- sqrt(mean((test_data$Total_of_grades - predictions)^2))
cat("Root Mean Squared Error (RMSE) for decision tree model:", tree_rmse, "\n")

# Visualize the decision tree
plot(model)
text(model, cex = 0.5)