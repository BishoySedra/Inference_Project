library(readxl)
file <- read.csv("StudentsPerformance.csv")

# START PREPROCESSING

#replacment 'M' -> 'Male' / 'F' -> 'Female'
file$sex[file$sex=="F"]="Female"
file$sex[file$sex=="f"]="Female"
file$sex[file$sex=="M"]="Male"
file$sex[file$sex=="m"]="Male"
#file <- na.omit(file)


#data type convertion for age column as it was a char class before convertion
file$age <- as.numeric(file$age)


# Handle outliars function
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


#feature engineering
file$Total_of_grades <- file$G1  + file$G2 + file$G3

# delete dublicates
file <- unique(file)

# END PREPROCESSING
summary(file)


# START visualization

plot(file$studytime, file$Total_of_grades , main = "PERFORMANCE [Study Time-Total Grades] " ,xlab = "Study Time" , ylab = "Total Grades")
plot(file$failures, file$Total_of_grades, main = "PERFORMANCE [Failures-Total Grades] " ,xlab = "Failures" , ylab = "Total Grades")
plot(file$absences, file$Total_of_grades, main = "PERFORMANCE [Absences-Total Grades] " ,xlab = "Absences" , ylab = "Total Grades")


hist(file$age, col = "lightblue", xlab = "Age", main = "Age Histogram")
hist(file$goout, col = "blue", xlab = "Goout", main = "Goout Histogram")
hist(file$studytime, col = "red", xlab = "Study Time", main = "Study Time Histogram")
hist(file$failures, col = "cyan", xlab = "Failures", main = "Failures Histogram")
hist(file$health, col = "brown", xlab = "Health", main = "Health Histogram")
hist(file$absences , col = "gray" , xlab = "Absences", main = "Absences Histogram")
hist(file$Total_of_grades, col = "green", xlab = "Total Grades", main = "Total Grades Histogram")

boxplot(file$age, col = "lightblue", xlab = "Age", main = "Age boxplot")
boxplot(file$goout, col = "blue", xlab = "Goout", main = "Goout boxplot")
boxplot(file$studytime, col = "red", xlab = "Study Time", main = "Study Time boxplot")
boxplot(file$failures, col = "cyan", xlab = "Failures", main = "Failures boxplot")
boxplot(file$health, col = "brown", xlab = "Health", main = "Health boxplot")
boxplot(file$absences , col = "gray" , xlab = "Absences", main = "Absences boxplot")
boxplot(file$Total_of_grades, col = "green", xlab = "Total Grades", main = "Total Grades boxplot")

barplot(table(file$sex),main = "Sex Bar chart")
barplot(table(file$Fjob),main = "Father Job Bar chart")
barplot(table(file$Mjob),main = "Mother Job Bar chart")
barplot(table(file$internet),main = "Internet Bar chart")
barplot(table(file$romantic),main = "Romantic Bar chart")






