#Project - FritoLay DDSAnalytics

#Video Link: https://www.youtube.com/watch?v=3dSzMTUk5xI

library(tidyverse)

data <- read.csv(file.choose(), header = TRUE)

# View the first few rows
head(data)

# Check dimensions
dim(data)

# Check structure
str(data)

# Count employees in each attrition category
table(data$Attrition)

# Calculate percentages
prop.table(table(data$Attrition)) * 100

table(data$Attrition)

prop.table(table(data$Attrition))

#Initial Employee Attrition Visualization

ggplot(data, aes(x = Attrition)) +
  geom_bar(color = "black",
           fill = "steelblue") +
  labs(
    title = "Employee Attrition",
    x = "Attrition",
    y = "Number of Employees"
  ) 

#Overtime

table(data$OverTime)
table(data$OverTime, data$Attrition)

data %>%
  group_by(OverTime) %>%
  summarise(
    AttritionRate = mean(Attrition == "Yes") * 100
  )


overtime_attrition <- data %>%
  group_by(OverTime) %>%
  summarise(
    TotalEmployees = length(Attrition),
    EmployeesLeft = sum(Attrition == "Yes")
  )

overtime_attrition

table(data$OverTime)

no_overtime_rate <- 60 / 618 * 100
yes_overtime_rate <- 80 / 252 * 100

no_overtime_rate
yes_overtime_rate

overtime_data <- data.frame(
  OverTime = c("No", "Yes"),
  AttritionRate = c(9.71, 31.75)
)

overtime_data

library(ggplot2)

ggplot(overtime_data,
       aes(x = OverTime, y = AttritionRate, fill = OverTime)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = paste0(round(AttritionRate, 1), "%")),
    vjust = -0.5,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "No" = "#2F75B5",   # steel blue
      "Yes" = "#ED7D31"   # Orange/highlight
    )
  ) +
  labs(
    title = "Employees Working Overtime Have Higher Attrition",
    subtitle = "Attrition rate by overtime status",
    x = "Overtime",
    y = "Attrition Rate (%)"
  ) +
  ylim(0, 40) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "gray40"
    ),
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    axis.text = element_text(
      size = 11
    ),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


#Job Role

table(data$JobRole, data$Attrition)

#Calculate Attrition Rates

jobrole_table <- table(data$JobRole, data$Attrition)

jobrole_attrition <- data.frame(
  JobRole = rownames(jobrole_table),
  TotalEmployees = rowSums(jobrole_table),
  EmployeesLeft = jobrole_table[, "Yes"]
)

jobrole_attrition$AttritionRate <-
  (jobrole_attrition$EmployeesLeft /
     jobrole_attrition$TotalEmployees) * 100

jobrole_attrition

#Visualization
ggplot(jobrole_attrition,
       aes(x = AttritionRate,
           y = reorder(JobRole, AttritionRate),
           fill = AttritionRate)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = paste0(round(AttritionRate, 1), "%")),
    hjust = -0.15,
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "#2F75B5",
    high = "#ED7D31"
  ) +
  labs(
    title = "Attrition Varies Dramatically by Job Role",
    subtitle = "Sales Representatives have the highest attrition rate",
    x = "Attrition Rate (%)",
    y = "Job Role"
  ) +
  xlim(0, 50) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "gray40"
    ),
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    axis.text = element_text(
      size = 10
    ),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

#Work-Life Balance

table(data$WorkLifeBalance, data$Attrition)

#Attrition Rates

worklife_table <- table(data$WorkLifeBalance, data$Attrition)

worklife_attrition <- data.frame(
  WorkLifeBalance = rownames(worklife_table),
  TotalEmployees = rowSums(worklife_table),
  EmployeesLeft = worklife_table[, "Yes"]
)

worklife_attrition$AttritionRate <-
  (worklife_attrition$EmployeesLeft /
     worklife_attrition$TotalEmployees) * 100

worklife_attrition

#Visualization

ggplot(worklife_attrition,
       aes(x = factor(WorkLifeBalance),
           y = AttritionRate,
           fill = AttritionRate)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = paste0(round(AttritionRate, 1), "%")),
    vjust = -0.5,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "#2F75B5",
    high = "#ED7D31"
  ) +
  labs(
    title = "Poor Work-Life Balance Is Associated With Higher Attrition",
    subtitle = "Attrition rate by work-life balance rating",
    x = "Work-Life Balance Rating",
    y = "Attrition Rate (%)"
  ) +
  ylim(0, 40) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "gray40"
    ),
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    axis.text = element_text(
      size = 11
    ),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

#Prepping Data by removing unnecessary variables

# Make sure Attrition is a factor
data$Attrition <- factor(data$Attrition,
                         levels = c("No", "Yes"))

# Remove variables that should not be analyzed as predictors
analysis_data <- data[, !(names(data) %in% c(
  "ID",
  "EmployeeNumber",
  "EmployeeCount",
  "Over18",
  "StandardHours"
))]

# remaining variables
names(analysis_data)

#Screening Categorical Variables
categorical_results <- data.frame(
  Variable = character(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

for (var in names(analysis_data)) {
  
  if (var != "Attrition" &&
      (is.character(analysis_data[[var]]) ||
       is.factor(analysis_data[[var]]) ||
       length(unique(analysis_data[[var]])) <= 10)) {
    
    test_table <- table(analysis_data[[var]],
                        analysis_data$Attrition)
    
    test_result <- chisq.test(test_table)
    
    categorical_results <- rbind(
      categorical_results,
      data.frame(
        Variable = var,
        P_Value = test_result$p.value
      )
    )
  }
}

categorical_results <- categorical_results[
  order(categorical_results$P_Value),
]

#P-Values of each Categorical Variable
categorical_results

#Screening Numerical Variables
numeric_results <- data.frame(
  Variable = character(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

for (var in names(analysis_data)) {
  
  if (var != "Attrition" &&
      is.numeric(analysis_data[[var]]) &&
      length(unique(analysis_data[[var]])) > 10) {
    
    test_result <- wilcox.test(
      analysis_data[[var]] ~ analysis_data$Attrition
    )
    
    numeric_results <- rbind(
      numeric_results,
      data.frame(
        Variable = var,
        P_Value = test_result$p.value
      )
    )
  }
}

numeric_results <- numeric_results[
  order(numeric_results$P_Value),
]

#P-Values of each Numerical Variable
numeric_results

#Combining Both Results
all_results <- rbind(
  categorical_results,
  numeric_results
)

all_results <- all_results[
  order(all_results$P_Value),
]

all_results

#Variables that pass <0.05
significant_results <- all_results[
  all_results$P_Value < 0.05,
]

significant_results

# Select the strongest statistically significant factors
stat_plot_data <- all_results[
  all_results$P_Value < 0.05 &
    all_results$Variable != "Attrition",
]

# Keep the 10 strongest
stat_plot_data <- head(
  stat_plot_data[order(stat_plot_data$P_Value), ],
  10
)

# Calculate -log10(p-value)
stat_plot_data$MinusLog10P <- -log10(stat_plot_data$P_Value)

# Plot for top 10 attrition variables
ggplot(
  stat_plot_data,
  aes(
    x = MinusLog10P,
    y = reorder(Variable, MinusLog10P)
  )
) +
  geom_col(
    fill = "#2F75B5",
    width = 0.65
  ) +
  geom_text(
    aes(
      label = ifelse(
        P_Value < 0.001,
        "p < 0.001",
        paste0("p = ", signif(P_Value, 2))
      )
    ),
    hjust = -0.1,
    size = 3.8,
    fontface = "bold"
  ) +
  labs(
    title = "Several Factors Show Statistically Significant Associations",
    subtitle = "Top 10 variables ranked by statistical significance",
    x = expression(-log[10](p-value)),
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "gray40"
    ),
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    axis.text = element_text(
      size = 10
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

#Attrition Rates for Strongest Categorical Variables

#Stock-Option
stockoption_table <- table(data$StockOptionLevel,
                           data$Attrition)

stockoption_attrition <- data.frame(
  StockOptionLevel = rownames(stockoption_table),
  TotalEmployees = rowSums(stockoption_table),
  EmployeesLeft = stockoption_table[, "Yes"]
)

stockoption_attrition$AttritionRate <-
  (stockoption_attrition$EmployeesLeft /
     stockoption_attrition$TotalEmployees) * 100

stockoption_attrition

ggplot(
  stockoption_attrition,
  aes(
    x = StockOptionLevel,
    y = AttritionRate,
    fill = StockOptionLevel
  )
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(
      label = paste0(round(AttritionRate, 1), "%")
    ),
    vjust = -0.5,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "0" = "#ED7D31",
      "1" = "#70AD47",
      "2" = "#2F75B5",
      "3" = "#F4B183"
    )
  ) +
  labs(
    title = "Attrition Varies Across Stock Option Levels",
    x = "Stock Option Level",
    y = "Attrition Rate (%)"
  ) +
  ylim(0, 32) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "gray40"
    ),
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    axis.text = element_text(
      size = 11
    ),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

#Job-Involvement
jobinvolvement_table <- table(data$JobInvolvement,
                              data$Attrition)

jobinvolvement_attrition <- data.frame(
  JobInvolvement = rownames(jobinvolvement_table),
  TotalEmployees = rowSums(jobinvolvement_table),
  EmployeesLeft = jobinvolvement_table[, "Yes"]
)

jobinvolvement_attrition$AttritionRate <-
  (jobinvolvement_attrition$EmployeesLeft /
     jobinvolvement_attrition$TotalEmployees) * 100

jobinvolvement_attrition

ggplot(
  jobinvolvement_attrition,
  aes(
    x = JobInvolvement,
    y = AttritionRate,
    fill = JobInvolvement
  )
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(
      label = paste0(round(AttritionRate, 1), "%")
    ),
    vjust = -0.5,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "1" = "#ED7D31",
      "2" = "#F4B183",
      "3" = "#70AD47",
      "4" = "#2F75B5"
    )
  ) +
  labs(
    title = "Lower Job Involvement Is Associated With Higher Attrition",
    x = "Job Involvement Level",
    y = "Attrition Rate (%)"
  ) +
  ylim(0, 55) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "gray40"
    ),
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    axis.text = element_text(
      size = 11
    ),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

#Job Level
joblevel_table <- table(data$JobLevel,
                        data$Attrition)

joblevel_attrition <- data.frame(
  JobLevel = rownames(joblevel_table),
  TotalEmployees = rowSums(joblevel_table),
  EmployeesLeft = joblevel_table[, "Yes"]
)

joblevel_attrition$AttritionRate <-
  (joblevel_attrition$EmployeesLeft /
     joblevel_attrition$TotalEmployees) * 100

joblevel_attrition

#Summary of Numerical Variables
numeric_summary <- data.frame(
  Variable = character(),
  Median_Stayed = numeric(),
  Median_Left = numeric(),
  Mean_Stayed = numeric(),
  Mean_Left = numeric(),
  Difference_in_Medians = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

numeric_vars <- c(
  "Age",
  "MonthlyIncome",
  "TotalWorkingYears",
  "YearsAtCompany",
  "YearsInCurrentRole",
  "YearsWithCurrManager",
  "YearsSinceLastPromotion",
  "DistanceFromHome",
  "NumCompaniesWorked"
)

for (var in numeric_vars) {
  
  stayed <- data[data$Attrition == "No", var]
  left <- data[data$Attrition == "Yes", var]
  
  test_result <- wilcox.test(stayed, left)
  
  numeric_summary <- rbind(
    numeric_summary,
    data.frame(
      Variable = var,
      Median_Stayed = median(stayed),
      Median_Left = median(left),
      Mean_Stayed = mean(stayed),
      Mean_Left = mean(left),
      Difference_in_Medians =
        median(left) - median(stayed),
      P_Value = test_result$p.value
    )
  )
}

numeric_summary

#Logistic Regression Model
attrition_model_1 <- glm(
  Attrition ~ OverTime +
    StockOptionLevel +
    JobRole +
    JobInvolvement +
    Age +
    MonthlyIncome +
    TotalWorkingYears +
    YearsAtCompany +
    YearsInCurrentRole +
    YearsWithCurrManager,
  data = data,
  family = binomial
)

summary(attrition_model_1)


#70/30 Train Validation Split

library(caret)

set.seed(123)

train_index <- createDataPartition(
  data$Attrition,
  p = 0.70,
  list = FALSE
)

train_data <- data[train_index, ]
validation_data <- data[-train_index, ]

# Check the split
prop.table(table(train_data$Attrition)) * 100
prop.table(table(validation_data$Attrition)) * 100

#Fit Model 2 on the training data
attrition_model_validation <- glm(
  Attrition ~ OverTime +
    StockOptionLevel +
    JobRole +
    JobInvolvement +
    Age +
    MonthlyIncome +
    TotalWorkingYears +
    YearsAtCompany +
    YearsInCurrentRole +
    YearsWithCurrManager,
  data = train_data,
  family = binomial
)

summary(attrition_model_validation)

#Generate Validation Probablities
validation_prob <- predict(
  attrition_model_validation,
  newdata = validation_data,
  type = "response"
)

head(validation_prob)

#50% Threshold (50< = yes)

validation_pred <- ifelse(
  validation_prob >= 0.50,
  "Yes",
  "No"
)

validation_pred <- factor(
  validation_pred,
  levels = c("No", "Yes")
)

actual <- factor(
  validation_data$Attrition,
  levels = c("No", "Yes")
)

#Confusion Matrix
confusionMatrix(
  validation_pred,
  actual,
  positive = "Yes"
)

#Testing Multiple Tresholds from 20 to 70%
thresholds <- seq(0.20, 0.70, by = 0.05)

threshold_results <- data.frame(
  Threshold = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  Accuracy = numeric()
)

for (t in thresholds) {
  
  pred <- ifelse(validation_prob >= t, "Yes", "No")
  
  pred <- factor(pred, levels = c("No", "Yes"))
  
  cm <- confusionMatrix(
    pred,
    actual,
    positive = "Yes"
  )
  
  threshold_results <- rbind(
    threshold_results,
    data.frame(
      Threshold = t,
      Sensitivity = cm$byClass["Sensitivity"],
      Specificity = cm$byClass["Specificity"],
      Accuracy = cm$overall["Accuracy"]
    )
  )
}

threshold_results

#only keep thresholds that have above 60% for both sensitivity and specificity
threshold_results[
  threshold_results$Sensitivity >= 0.60 &
    threshold_results$Specificity >= 0.60,
]

#KNN model to test

knn_formula <- ~ OverTime +
  StockOptionLevel +
  JobRole +
  JobInvolvement +
  Age +
  MonthlyIncome +
  TotalWorkingYears +
  YearsAtCompany +
  YearsInCurrentRole +
  YearsWithCurrManager

#Creating Dummy Variable
knn_dummies <- dummyVars(
  knn_formula,
  data = train_data,
  fullRank = TRUE
)

train_knn <- predict(
  knn_dummies,
  newdata = train_data
)

validation_knn <- predict(
  knn_dummies,
  newdata = validation_data
)

#Scaling the Variables
knn_preprocess <- preProcess(
  train_knn,
  method = c("center", "scale")
)

train_knn_scaled <- predict(
  knn_preprocess,
  train_knn
)

validation_knn_scaled <- predict(
  knn_preprocess,
  validation_knn
)

#testing different K values from 3 to 21
k_values <- seq(3, 21, by = 2)

knn_results <- data.frame(
  K = numeric(),
  Accuracy = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric()
)

for (k in k_values) {
  
  knn_model <- knn3(
    x = train_knn_scaled,
    y = train_data$Attrition,
    k = k
  )
  
  knn_pred <- predict(
    knn_model,
    newdata = validation_knn_scaled,
    type = "prob"
  )
  
  knn_class <- factor(
    ifelse(knn_pred[, "Yes"] >= 0.50, "Yes", "No"),
    levels = c("No", "Yes")
  )
  
  cm <- confusionMatrix(
    knn_class,
    actual,
    positive = "Yes"
  )
  
  knn_results <- rbind(
    knn_results,
    data.frame(
      K = k,
      Accuracy = cm$overall["Accuracy"],
      Sensitivity = cm$byClass["Sensitivity"],
      Specificity = cm$byClass["Specificity"]
    )
  )
}

knn_results

knn_model_final <- knn3(
  x = train_knn_scaled,
  y = train_data$Attrition,
  k = 5
)

knn_prob <- predict(
  knn_model_final,
  newdata = validation_knn_scaled,
  type = "prob"
)[, "Yes"]

knn_threshold_results <- data.frame(
  Threshold = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  Accuracy = numeric()
)


for (t in thresholds) {
  
  pred <- ifelse(knn_prob >= t, "Yes", "No")
  pred <- factor(pred, levels = c("No", "Yes"))
  
  cm <- confusionMatrix(
    pred,
    actual,
    positive = "Yes"
  )
  
  knn_threshold_results <- rbind(
    knn_threshold_results,
    data.frame(
      Threshold = t,
      Sensitivity = cm$byClass["Sensitivity"],
      Specificity = cm$byClass["Specificity"],
      Accuracy = cm$overall["Accuracy"]
    )
  )
}

knn_threshold_results

knn_threshold_results[
  knn_threshold_results$Sensitivity >= 0.60 &
    knn_threshold_results$Specificity >= 0.60,
]


knn_results_long <- knn_results %>%
  pivot_longer(
    cols = c(Accuracy, Sensitivity, Specificity),
    names_to = "Metric",
    values_to = "Percentage"
  )

ggplot(
  knn_results_long,
  aes(x = K, y = Percentage, color = Metric)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(
    labels = function(x) paste0(x * 100, "%")
  ) +
  labs(
    title = "KNN Performance Across Different K Values",
    x = "Number of Neighbors (K)",
    y = "Performance",
    color = "Metric"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "bottom"
  )

#Confusion Matrix-KNN
knn_threshold <- 0.20

knn_prob <- attr(knn_pred, "prob")

knn_class <- ifelse(
  knn_prob >= knn_threshold,
  "Yes",
  "No"
)

knn_class <- factor(
  knn_class,
  levels = c("Yes", "No")
)

knn_cm <- confusionMatrix(
  knn_class,
  validation_data$Attrition,
  positive = "Yes"
)

knn_cm

#NB Model
install.packages("klaR")
library(klaR)

train_data$Attrition <- factor(
  train_data$Attrition,
  levels = c("No", "Yes")
)

validation_data$Attrition <- factor(
  validation_data$Attrition,
  levels = c("No", "Yes")
)

train_data$OverTime <- factor(train_data$OverTime)
validation_data$OverTime <- factor(validation_data$OverTime)

train_data$StockOptionLevel <- factor(train_data$StockOptionLevel)
validation_data$StockOptionLevel <- factor(validation_data$StockOptionLevel)

train_data$JobRole <- factor(train_data$JobRole)
validation_data$JobRole <- factor(validation_data$JobRole)

train_data$JobInvolvement <- factor(train_data$JobInvolvement)
validation_data$JobInvolvement <- factor(validation_data$JobInvolvement)

nb_model <- NaiveBayes(
  Attrition ~ OverTime +
    StockOptionLevel +
    JobRole +
    JobInvolvement +
    Age +
    MonthlyIncome +
    TotalWorkingYears +
    YearsAtCompany +
    YearsInCurrentRole +
    YearsWithCurrManager,
  data = train_data
)

nb_model

#NB Model Predictions
nb_prob <- predict(
  nb_model,
  newdata = validation_data
)$posterior[, "Yes"]


head(nb_prob)

#Evaluating NB at different thresholds

# Test classification thresholds
thresholds <- seq(0.10, 0.70, by = 0.05)

nb_threshold_results <- data.frame(
  Threshold = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  Accuracy = numeric(),
  BalancedAccuracy = numeric()
)

for (t in thresholds) {
  
  nb_pred <- ifelse(nb_prob >= t, "Yes", "No")
  
  nb_pred <- factor(
    nb_pred,
    levels = c("No", "Yes")
  )
  
  cm <- confusionMatrix(
    nb_pred,
    actual,
    positive = "Yes"
  )
  
  nb_threshold_results <- rbind(
    nb_threshold_results,
    data.frame(
      Threshold = t,
      Sensitivity = unname(cm$byClass["Sensitivity"]),
      Specificity = unname(cm$byClass["Specificity"]),
      Accuracy = unname(cm$overall["Accuracy"]),
      BalancedAccuracy = unname(cm$byClass["Balanced Accuracy"])
    )
  )
}

# Display results
round(nb_threshold_results, 3)
head(nb_prob)

#Identifying qualified thresholds
nb_qualified <- subset(
  nb_threshold_results,
  Sensitivity >= 0.60 &
    Specificity >= 0.60
)

round(nb_qualified, 3)

#Best Threshold
if (nrow(nb_qualified) > 0) {
  
  nb_best <- nb_qualified[
    which.max(nb_qualified$BalancedAccuracy),
  ]
  
  print(round(nb_best, 3))
  
} else {
  
  print("No thresholds met both requirements.")
  
}

#NB Model Visualization
nb_predictions <- predict(
  nb_model,
  newdata = validation_data
)

nb_prob <- nb_predictions$posterior[, "Yes"]

nb_plot_data <- data.frame(
  Actual = validation_data$Attrition,
  Probability = nb_prob
)

ggplot(
  nb_plot_data,
  aes(x = Probability, fill = Actual)
) +
  geom_density(alpha = 0.5) +
  geom_vline(
    xintercept = 0.25,
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Naive Bayes Predicted Attrition Probabilities",
    x = "Predicted Probability of Attrition",
    y = "Density",
    fill = "Actual Outcome"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "bottom"
  )


#Confusion Matrix for Regression Model
# Predict probabilities on validation data
validation_prob <- predict(
  attrition_model_validation,
  newdata = validation_data,
  type = "response"
)

# Classify using final 0.20 threshold
validation_pred_20 <- ifelse(
  validation_prob >= 0.20,
  "Yes",
  "No"
)

# Convert to factors
validation_pred_20 <- factor(
  validation_pred_20,
  levels = c("No", "Yes")
)

actual_20 <- factor(
  validation_data$Attrition,
  levels = c("No", "Yes")
)

cm_20 <- confusionMatrix(
  validation_pred_20,
  actual_20,
  positive = "Yes"
)

cm_20

cm_table <- as.data.frame(cm_20$table)

cm_table

#Prediction Model

#Since we changed the factor order of Attrition to [Yes, No]
#AttritionProbability = 1 - predict(...) 


competition_data <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

dim(competition_data)
head(competition_data)

#making sure Categorical Variables match the final model
competition_data$OverTime <- factor(
  competition_data$OverTime,
  levels = final_model$xlevels$OverTime
)

competition_data$JobRole <- factor(
  competition_data$JobRole,
  levels = final_model$xlevels$JobRole
)

competition_data$StockOptionLevel <- factor(
  competition_data$StockOptionLevel,
  levels = final_model$xlevels$StockOptionLevel
)

colSums(
  is.na(
    competition_data[, c(
      "OverTime",
      "StockOptionLevel",
      "JobRole",
      "JobInvolvement",
      "Age",
      "MonthlyIncome",
      "TotalWorkingYears",
      "YearsAtCompany",
      "YearsInCurrentRole",
      "YearsWithCurrManager"
    )]
  )
)

#Generating Attrition Probabilites
competition_prob_no <- predict(
  final_model,
  newdata = competition_data,
  type = "response"
)

competition_prob_yes <- 1 - competition_prob_no

head(competition_prob_yes)

#Competition Predictors

competition_prediction <- ifelse(
  competition_prob_yes >= 0.20,
  "Yes",
  "No"
)
#Table of how many employees are predicted to leave
table(competition_prediction)

#Percentages
prop.table(
  table(competition_prediction)
) * 100


#Model for Flagged vs. Not Flagged
# Create summary data
prediction_summary <- data.frame(
  Status = c(
    "Flagged for Retention",
    "Not Flagged"
  ),
  Employees = c(
    sum(competition_prediction == "Yes"),
    sum(competition_prediction == "No")
  )
)

# Calculate percentages
prediction_summary$Percentage <-
  prediction_summary$Employees /
  sum(prediction_summary$Employees) * 100

ggplot(
  prediction_summary,
  aes(
    x = Status,
    y = Employees,
    fill = Status
  )
) +
  geom_col(
    width = 0.6
  ) +
  geom_text(
    aes(
      label = paste0(
        Employees,
        " (",
        round(Percentage, 1),
        "%)"
      )
    ),
    vjust = -0.5,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Flagged for Retention" = "#ED7D31",
      "Not Flagged" = "#2F75B5"
    )
  ) +
  labs(
    title = "Flagged vs. Not Flagged Employees",
    subtitle = "Employees classified using a 20% attrition probability threshold",
    x = NULL,
    y = "Number of Employees"
  ) +
  ylim(
    0,
    max(prediction_summary$Employees) * 1.15
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "gray40"
    ),
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    axis.text = element_text(
      size = 11
    ),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

#Comparing Regression vs. KNN vs. NB Graphs

model_comparison <- data.frame(
  Model = c(
    "Logistic Regression",
    "KNN",
    "Naive Bayes"
  ),
  Sensitivity = c(
    69.05,
    71.43,
    61.90
  ),
  Specificity = c(
    76.26,
    63.47,
    69.41
  ),
  BalancedAccuracy = c(
    72.65,
    67.45,
    65.66
  )
)


model_comparison_long <- model_comparison %>%
  pivot_longer(
    cols = c(
      Sensitivity,
      Specificity,
      BalancedAccuracy
    ),
    names_to = "Metric",
    values_to = "Percentage"
  )

ggplot(
  model_comparison_long,
  aes(
    x = Model,
    y = Percentage,
    fill = Metric
  )
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  geom_text(
    aes(
      label = paste0(
        round(Percentage, 1),
        "%"
      )
    ),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 3.8,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Sensitivity" = "black",
      "Specificity" = "#2F75B5",
      "BalancedAccuracy" = "darkgreen"
    ),
    labels = c(
      "Sensitivity",
      "Specificity",
      "Balanced Accuracy"
    )
  ) +
  labs(
    title = "Validation-set performance across candidate models",
    x = NULL,
    y = "Performance (%)",
    fill = NULL
  ) +
  ylim(0, 100) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "gray40"
    ),
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    axis.text = element_text(
      size = 10
    ),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


#Creating Submission File
competition_results <- data.frame(
  ID = competition_data$ID,
  Attrition = competition_prediction
)

#Sort by ID
competition_results <- competition_results[
  order(competition_results$ID),
]

#Check no missing submissions
sum(is.na(competition_results$Attrition))

#Export CSV
write.csv(
  competition_results,
  "FritoLay_Attrition_Predictions.csv",
  row.names = FALSE
)


#financial impact dataset
# Add model predictions to the competition dataset
financial_data <- competition_data

financial_data$PredictedProbability <- competition_prob_yes
financial_data$PredictedAttrition <- competition_prediction

# Keep only employees flagged by the model
flagged_employees <- financial_data[
  financial_data$PredictedAttrition == "Yes",
]

# Check how many employees were flagged
nrow(flagged_employees)

# Look at the flagged employees
head(flagged_employees)

#calculate annual salary
flagged_employees$AnnualSalary <-
  flagged_employees$MonthlyIncome * 12

summary(flagged_employees$AnnualSalary)

mean(flagged_employees$AnnualSalary)

#Calculate Replacement Costs
flagged_employees$ReplacementCost_50 <-
  flagged_employees$AnnualSalary * 0.50

flagged_employees$ReplacementCost_100 <-
  flagged_employees$AnnualSalary * 1.00

flagged_employees$ReplacementCost_200 <-
  flagged_employees$AnnualSalary * 2.00

flagged_employees$ReplacementCost_400 <-
  flagged_employees$AnnualSalary * 4.00

total_replacement_costs <- data.frame(
  Scenario = c(
    "50%",
    "100%",
    "200%",
    "400%"
  ),
  
  TotalReplacementCost = c(
    sum(flagged_employees$ReplacementCost_50),
    sum(flagged_employees$ReplacementCost_100),
    sum(flagged_employees$ReplacementCost_200),
    sum(flagged_employees$ReplacementCost_400)
  )
)

total_replacement_costs

#Account for $200 intervention
intervention_cost <- nrow(flagged_employees) * 200

intervention_cost

#Net Savings
retention_rates <- c(0.25, 0.50, 0.75, 1.00)
replacement_rates <- c(0.50, 1.00, 2.00, 4.00)

financial_impact <- expand.grid(
  RetentionRate = retention_rates,
  ReplacementRate = replacement_rates
)

financial_impact$EmployeesRetained <-
  round(nrow(flagged_employees) * financial_impact$RetentionRate)

# Calculate gross savings
financial_impact$GrossSavings <- mapply(
  function(retention, replacement) {
    total_salary <- sum(flagged_employees$AnnualSalary) * retention
    total_salary * replacement
  },
  financial_impact$RetentionRate,
  financial_impact$ReplacementRate
)

# Intervention cost
financial_impact$InterventionCost <- intervention_cost

# Net savings
financial_impact$NetSavings <-
  financial_impact$GrossSavings -
  financial_impact$InterventionCost

financial_impact
