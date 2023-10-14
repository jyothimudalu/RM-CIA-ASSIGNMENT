setwd("E://")
getwd()

# Calculate mean for 'satisfaction_level'
mean_satisfaction <- mean(hr_data$satisfaction_level, na.rm = TRUE)
cat("Mean for Satisfaction Level:", mean_satisfaction, "\n")

# Calculate median for 'satisfaction_level'
median_satisfaction <- median(hr_data$satisfaction_level, na.rm = TRUE)
cat("Median for Satisfaction Level:", median_satisfaction, "\n")

# Custom function to find mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate mode for 'satisfaction_level'
mode_satisfaction <- get_mode(hr_data$satisfaction_level)
cat("Mode for Satisfaction Level:", mode_satisfaction, "\n")

# Frequency distribution for 'dept'
table_dept <- table(hr_data$dept)
cat("Frequency distribution for Department:\n", table_dept, "\n")

# Frequency distribution for 'salary'
table_salary <- table(hr_data$salary)
cat("Frequency distribution for Salary:\n", table_salary, "\n")

# Calculate standard deviation for 'satisfaction_level'
sd_satisfaction <- sd(hr_data$satisfaction_level)
cat("Standard Deviation for Satisfaction Level:", sd_satisfaction, "\n")

# Calculate quartiles for 'satisfaction_level'
quartiles_satisfaction <- quantile(hr_data$satisfaction_level, probs = c(0, 0.25, 0.5, 0.75, 1))
cat("Minimum (Q0):", quartiles_satisfaction[1], "\n")
cat("1st Quartile (Q1):", quartiles_satisfaction[2], "\n")
cat("Median (Q2):", quartiles_satisfaction[3], "\n")
cat("3rd Quartile (Q3):", quartiles_satisfaction[4], "\n")
cat("Maximum (Q4):", quartiles_satisfaction[5], "\n")


# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Read the CSV file
hr_data=read.csv("HR.csv")
View(hr_data)

# Create a histogram for the satisfaction_level column
ggplot(data = hr_data, aes(x = satisfaction_level)) +
geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
labs(title = "Histogram of Satisfaction Level", x = "Satisfaction Level", y = "Frequency")

#Scatter Plot
ggplot(data = hr_data) +
  geom_point(aes(x = satisfaction_level, y = last_evaluation, color = factor(left))) +
  labs(
    title = "Employee Satisfaction vs. Last Evaluation",
    x = "Satisfaction Level",
    y = "Last Evaluation",
    color = "Left"
  ) +
  theme_minimal()

#Bar Plot: Number of Employees in each Department
ggplot(hr_data, aes(x = dept, fill = factor(left))) +
  geom_bar() +
  labs(
    title = "Number of Employees in each Department",
    x = "Department",
    y = "Count",
    fill = "Left"
  ) +
  theme_minimal()

#Boxplot: Distribution of Average Monthly Hours by Left Status
ggplot(hr_data, aes(x = factor(left), y = average_montly_hours, fill = factor(left))) +
  geom_boxplot() +
  labs(
    title = "Distribution of Average Monthly Hours by Left Status",
    x = "Left",
    y = "Average Monthly Hours",
    fill = "Left"
  ) +
  theme_minimal()

#Density Plot: Distribution of Satisfaction Level
ggplot(hr_data, aes(x = satisfaction_level, fill = factor(left))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribution of Satisfaction Level",
    x = "Satisfaction Level",
    fill = "Left"
  ) +
  theme_minimal()

#Faceted Scatter Plot: Satisfaction Level vs. Last Evaluation by Left Status
ggplot(hr_data, aes(x = satisfaction_level, y = last_evaluation, color = factor(left))) +
  geom_point() +
  facet_grid(. ~ left) +
  labs(
    title = "Satisfaction Level vs. Last Evaluation by Left Status",
    x = "Satisfaction Level",
    y = "Last Evaluation",
    color = "Left"
  ) +
  theme_minimal()

#Violin Plot: Distribution of Number of Projects by Left Status
ggplot(hr_data, aes(x = factor(left), y = number_project, fill = factor(left))) +
  geom_violin() +
  labs(
    title = "Distribution of Number of Projects by Left Status",
    x = "Left",
    y = "Number of Projects",
    fill = "Left"
  ) +
  theme_minimal()

s