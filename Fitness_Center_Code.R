# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(ggpubr)

# Load dataset
df <- read_csv("/Users/pranup/Downloads/Analytics_Practicum/Project2/Fitness.csv") 
# View dataset structure
str(df)
# Summary statistics for numerical variables
summary(df)
# View column names
print(colnames(df))
# Replace spaces and parentheses with underscores
colnames(df) <- gsub(" ", "_", colnames(df))
colnames(df) <- gsub("[()]", "", colnames(df))
# Replace slash with underscore
colnames(df) <- gsub("/", "_", colnames(df))
# View updated column names
print(colnames(df))

# 3. Data Preprocessing --------
library(knitr)
library(dplyr)
# Select only numerical variables
num_summary <- df %>%
  dplyr::select(where(is.numeric)) %>%
  summary()
# Convert to a data frame for kable
kable(as.data.frame(num_summary), caption = "Summary Statistics for Numerical Variables")

library(dplyr)
# Generate frequency tables for categorical variables
gender_count <- as.data.frame(table(df$Gender))
workout_type_count <- as.data.frame(table(df$Workout_Type))
# Print tables
kable(gender_count, col.names = c("Gender", "Count"), caption = "Frequency of Gender Categories")
kable(workout_type_count, col.names = c("Workout Type", "Count"), caption = "Frequency of Workout Types")

library(tidyr)
# Get all categorical variable names
cat_vars <- c("Gender", "Workout_Type")
# Create a combined frequency table
cat_summary <- df %>%
  dplyr::select(all_of(cat_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Category") %>%
  count(Variable, Category)

# Print the summary in a nice format
kable(cat_summary, col.names = c("Variable", "Category", "Count"), caption = "Summary of Categorical Variables")

# Display missing values in a structured format
missing_values <- data.frame(Variable = names(df), Missing_Count = colSums(is.na(df)))
kable(missing_values, caption = "Missing Values per Column")

# Display zero values in numerical columns
zero_values <- data.frame(Variable = names(df), Zero_Count = colSums(df == 0, na.rm = TRUE))
kable(zero_values, caption = "Zero Values per Column")

# Confirming the column names in the dataset
kable(names(df), caption = "Column names in the dataset:")

# Checking Outliers ----
# Updated list of numerical columns
num_cols_corrected <- c("Calories_Burned", "BMI", "Session_Duration_hours", 
                        "Max_BPM", "Avg_BPM", "Resting_BPM", "Water_Intake_liters", 
                        "Fat_Percentage", "Workout_Frequency_days_week")

# Generate boxplots for all numerical variables
par(mfrow=c(3,3))  # Arrange plots in a 3x3 grid
for (col in num_cols_corrected) {
  boxplot(df[[col]], main=paste("Boxplot of", col), col="lightgray", horizontal=TRUE)
}

# Detect outliers using Z-score method (Threshold = 3)
df_outliers_corrected <- df %>% 
  mutate(across(all_of(num_cols_corrected), ~ abs(scale(.)), .names = "z_{.col}")) %>% 
  filter(rowSums(across(starts_with("z_"), ~ . > 3)) > 0)

# Display detected outliers
print("Outliers detected in multiple numerical attributes:")
print(df_outliers_corrected)

# Relationship analysis between variables
library(GGally)
library(corrplot)

# Reset and adjust plotting parameters
# Ensure single plot layout
par(mfrow = c(1,1))
# Increase margins to allow space for text labels
par(mar = c(7, 7, 4, 4))
# Add extra outer margins
par(oma = c(2, 2, 2, 2))

# Define numeric columns for correlation matrix
numeric_cols <- c("Calories_Burned", "BMI", "Session_Duration_hours", 
                  "Max_BPM", "Avg_BPM", "Resting_BPM", "Weight_kg")
# Compute correlation matrix
cor_matrix <- cor(df[, numeric_cols], use="complete.obs")
# Generate correlation heatmap with improved text spacing
corrplot(cor_matrix, method="color", type="upper", 
         tl.col="black", tl.srt=45, tl.cex=0.9)

# Scatter Plots: Key Relationships
ggplot(df, aes(x=Session_Duration_hours, y=Calories_Burned)) + 
  geom_point(color="blue", alpha=0.6) + geom_smooth(method="lm", col="red") + 
  ggtitle("Session Duration vs Calories Burned") + theme_minimal()

ggplot(df, aes(x=Avg_BPM, y=Max_BPM)) + 
  geom_point(color="green", alpha=0.6) + geom_smooth(method="lm", col="red") + 
  ggtitle("Avg BPM vs Max BPM") + theme_minimal()

# Boxplots for Categorical vs. Continuous
ggplot(df, aes(x=Workout_Type, y=Calories_Burned, fill=Workout_Type)) + 
  geom_boxplot() + ggtitle("Calories Burned by Workout Type") + theme_minimal()

ggplot(df, aes(x=factor(Experience_Level), y=Resting_BPM, fill=factor(Experience_Level))) + 
  geom_boxplot() + ggtitle("Resting BPM by Experience Level") + theme_minimal()

# Feature Engineering ----
library(dplyr)
# Create new features
df <- df %>%
  mutate(
    # BMI Category
    BMI_CAT = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI >= 18.5 & BMI < 25 ~ "Normal",
      BMI >= 25 & BMI < 30 ~ "Overweight",
      BMI >= 30 ~ "Obese"
    ),
    
    # Heart Rate Metrics
    heart_rate_range = Max_BPM - Resting_BPM,
    heart_rate_reserve = Max_BPM - Avg_BPM,
    heart_intensity_ratio = Avg_BPM / Max_BPM,
    
    # Caloric Efficiency Metrics
    calories_per_kg = Calories_Burned / Weight_kg,
    workout_intensity_score = (Avg_BPM / Resting_BPM) * Session_Duration_hours * Workout_Frequency_days_week,
    
    # Hydration Ratio
    hydration_need_ratio = Water_Intake_liters / Session_Duration_hours,
    
    # Age Groups
    age_group = case_when(
      Age >= 18 & Age <= 25 ~ "18-25",
      Age > 25 & Age <= 35 ~ "26-35",
      Age > 35 & Age <= 45 ~ "36-45",
      Age > 45 ~ "46+"
    )
  )

# View modified dataset structure
kable(names(df), caption = "Modified column names in the dataset:")

# Encoding Categorical variables for modeling ----
library(dplyr)
# Encode Experience_Level as an ordered numeric variable
df <- df %>%
  mutate(Experience_Level = case_when(
    Experience_Level == 1 ~ 1,
    Experience_Level == 2 ~ 2,
    Experience_Level == 3 ~ 3
  ))

# Convert categorical variables (Workout_Type, BMI_CAT, age_group) using one-hot encoding
df <- df %>%
  mutate(
    Workout_Yoga = ifelse(Workout_Type == "Yoga", 1, 0),
    Workout_HIIT = ifelse(Workout_Type == "HIIT", 1, 0),
    Workout_Strength = ifelse(Workout_Type == "Strength", 1, 0),
    Workout_Cardio = ifelse(Workout_Type == "Cardio", 1, 0),
    
    BMI_Underweight = ifelse(BMI_CAT == "Underweight", 1, 0),
    BMI_Normal = ifelse(BMI_CAT == "Normal", 1, 0),
    BMI_Overweight = ifelse(BMI_CAT == "Overweight", 1, 0),
    BMI_Obese = ifelse(BMI_CAT == "Obese", 1, 0),
    
    Age_18_25 = ifelse(age_group == "18-25", 1, 0),
    Age_26_35 = ifelse(age_group == "26-35", 1, 0),
    Age_36_45 = ifelse(age_group == "36-45", 1, 0),
    Age_46_plus = ifelse(age_group == "46+", 1, 0)
  )

# Drop the original categorical columns
df <- df %>%
  dplyr::select(-Workout_Type, -BMI_CAT, -age_group)
# View modified dataset structure
kable(names(df), caption = "Modified column names in the dataset:")
# View modified dataset structure
str(df)

# 4. Predictor analysis and relevancy ------------
library(dplyr)
library(corrplot)
library(ggplot2)
#install.packages('factoextra')
#install.packages('ggfortify')
library(factoextra)  
library(ggfortify)

# Re-check Correlation for Feature Redundancy
numeric_cols <- c("Calories_Burned", "BMI", "Session_Duration_hours", 
                  "Max_BPM", "Avg_BPM", "Resting_BPM", "Weight_kg", 
                  "Fat_Percentage", "Water_Intake_liters", "Workout_Frequency_days_week", 
                  "heart_rate_range", "heart_rate_reserve", "heart_intensity_ratio", 
                  "calories_per_kg", "workout_intensity_score", "hydration_need_ratio")

cor_matrix <- cor(df[, numeric_cols], use = "complete.obs")
# Reset plot layout
par(mfrow = c(1,1))
# Increase margins
par(mar = c(7, 7, 4, 4))

# Generate correlation heatmap
corrplot(cor_matrix, method="color", type="upper", 
         tl.col="black", tl.srt=45, tl.cex=0.9)

library(dplyr)
# Check class distribution in BMI_Obese
class_distribution <- df %>%
  group_by(BMI_Obese) %>%
  summarise(Count = n(), Percentage = (n() / nrow(df)) * 100)
# Print class distribution
print(class_distribution)
# Visualize class imbalance
ggplot(class_distribution, aes(x = factor(BMI_Obese), y = Count, fill = factor(BMI_Obese))) +
  geom_bar(stat = "identity") +
  labs(title = "Class Distribution of BMI Obese", x = "BMI Obese (0 = Non-Obese, 1 = Obese)", y = "Count") +
  theme_minimal()

# Feature Importance using Random Forest for Regression Task ----
library(randomForest)
# Define predictor variables (excluding target variable)
predictors_reg <- df %>% dplyr::select(-Calories_Burned)

# Fit Random Forest model for feature importance
rf_model_reg <- randomForest(x = predictors_reg, y = df$Calories_Burned, importance = TRUE, ntree = 500)

# Extract feature importance
importance_df_reg <- as.data.frame(importance(rf_model_reg))

# Sort by importance
importance_df_reg <- importance_df_reg[order(-importance_df_reg$`%IncMSE`),]

# Plot Feature Importance
ggplot(importance_df_reg, aes(x = reorder(row.names(importance_df_reg), `%IncMSE`), y = `%IncMSE`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance - Random Forest (Calories Burned)",
       x = "Features", y = "% Increase in MSE") +
  theme_minimal()

# Feature Importance using Random Forest for Classification Task -----
# Convert BMI_Obese to factor for classification
df$BMI_Obese <- as.factor(df$BMI_Obese)

# Define predictor variables (excluding target variable)
predictors_class <- df %>% dplyr::select(-BMI_Obese)

# Fit Random Forest model for classification
rf_model_class <- randomForest(x = predictors_class, y = df$BMI_Obese, importance = TRUE, ntree = 500)

# Extract feature importance
importance_df_class <- as.data.frame(importance(rf_model_class))

# Sort by importance
importance_df_class <- importance_df_class[order(-importance_df_class$MeanDecreaseGini),]

# Plot Feature Importance
ggplot(importance_df_class, aes(x = reorder(row.names(importance_df_class), MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Feature Importance - Random Forest (BMI Obesity Classification)",
       x = "Features", y = "Mean Decrease in Gini") +
  theme_minimal()

# 7. Data partitioning -------------
# Select features for regression (Calories Burned prediction)
df_regression <- df %>%
  dplyr::select(Calories_Burned, Avg_BPM, Session_Duration_hours, Age, 
         heart_intensity_ratio, heart_rate_reserve, Gender, 
         workout_intensity_score, calories_per_kg, Fat_Percentage, Experience_Level)

# Check structure of the partitioned regression dataset
str(df_regression)

# Select features for classification (BMI Obese prediction)
df_classification <- df %>%
  dplyr::select(BMI_Obese, BMI, Weight_kg, BMI_Overweight, calories_per_kg, 
         BMI_Normal, Gender, Calories_Burned, BMI_Underweight, 
         hydration_need_ratio, Water_Intake_liters)

# Convert BMI_Obese to factor for classification modeling
df_classification$BMI_Obese <- as.factor(df_classification$BMI_Obese)

# Check structure of the partitioned classification dataset
str(df_classification)

library(car)
library(dplyr)
# Fit a linear model for regression variables to compute VIF
lm_reg <- lm(Calories_Burned ~ ., data = df_regression)
vif(lm_reg)

# Remove the highly collinear variable
df_regression <- df_regression %>% dplyr::select(-heart_intensity_ratio)

# Fit a new linear model for VIF calculation
lm_reg_updated <- lm(Calories_Burned ~ ., data = df_regression)

# Compute VIF again
vif(lm_reg_updated)

# For reproducibility
set.seed(42)
library(caret)

# Split for Regression
trainIndex_reg <- createDataPartition(df_regression$Calories_Burned, p = 0.8, list = FALSE)
train_regression <- df_regression[trainIndex_reg, ]
test_regression <- df_regression[-trainIndex_reg, ]

# Split for Classification
trainIndex_class <- createDataPartition(df_classification$BMI_Obese, p = 0.8, list = FALSE)
train_classification <- df_classification[trainIndex_class, ]
test_classificationification <- df_classification[-trainIndex_class, ]

# Check dimensions
dim(train_regression); dim(test_regression)
dim(train_classification); dim(test_classificationification)

# Segmentation -----------------------------
# Load required libraries
# K-Prototypes clustering for mixed data
library(clustMixType)
library(dplyr)

# Convert categorical variables to factors
df_clustering <- df %>%
  mutate(across(c(Gender, Workout_Yoga, Workout_HIIT, Workout_Strength, Workout_Cardio,
                  BMI_Underweight, BMI_Normal, BMI_Overweight, BMI_Obese,
                  Age_18_25, Age_26_35, Age_36_45, Age_46_plus), as.factor))

# Run K-Prototypes for k = 2
set.seed(123)
kproto_2 <- kproto(df_clustering, k = 2)

# Run K-Prototypes for k = 3 (alternative segmentation)
set.seed(123)
kproto_3 <- kproto(df_clustering, k = 3)

# Assign cluster labels to dataset
df$Cluster_2 <- as.factor(kproto_2$cluster)
df$Cluster_3 <- as.factor(kproto_3$cluster)

# Check cluster distribution
table(df$Cluster_2)
table(df$Cluster_3)

# Summarize cluster characteristics
df %>%
  group_by(Cluster_2) %>%
  summarise(across(where(is.numeric), mean))

df %>%
  group_by(Cluster_3) %>%
  summarise(across(where(is.numeric), mean))

library(dplyr)
# Assign the selected k=3 clustering results to the main dataframe
df$Cluster <- as.factor(kproto_3$cluster)
# Convert Cluster to a factor for better visualization
df$Cluster <- as.factor(df$Cluster)

# 1. Scatter Plot: Calories Burned vs. Session Duration by Cluster
ggplot(df, aes(x = Session_Duration_hours, y = Calories_Burned, color = Cluster)) +
  geom_point(alpha = 0.7) +
  labs(title = "Calories Burned vs. Session Duration by Cluster",
       x = "Session Duration (hours)", y = "Calories Burned") +
  theme_minimal()

# 2. Scatter Plot: Calories Burned vs. BMI by Cluster
ggplot(df, aes(x = BMI, y = Calories_Burned, color = Cluster)) +
  geom_point(alpha = 0.7) +
  labs(title = "Calories Burned vs. BMI by Cluster",
       x = "BMI", y = "Calories Burned") +
  theme_minimal()

# 3. Boxplot: Calories Burned Distribution by Cluster
ggplot(df, aes(x = Cluster, y = Calories_Burned, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Calories Burned Distribution by Cluster", 
       x = "Cluster", y = "Calories Burned") +
  theme_minimal()

# 4. Boxplot: BMI Distribution by Cluster
ggplot(df, aes(x = Cluster, y = BMI, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "BMI Distribution by Cluster",
       x = "Cluster", y = "BMI") +
  theme_minimal()

# 5. Boxplot: Session Duration by Cluster
ggplot(df, aes(x = Cluster, y = Session_Duration_hours, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Session Duration Distribution by Cluster",
       x = "Cluster", y = "Session Duration (hours)") +
  theme_minimal()

# 8. Model Selection --------
# Model Selection Criteria:
# - Linear Regression (Baseline)
# - Polynomial Regression (for potential non-linearity)
# - Random Forest Regression (non-parametric)

# 9. Model Fitting, Validation Accuracy, and Test Accuracy ------
library(caret)
library(dplyr)
library(Metrics)
library(knitr)
library(kableExtra)
# Scaling numeric features (except categorical ones)
numeric_features <- c("Avg_BPM", "Session_Duration_hours", "Age", 
                      "heart_rate_reserve", "calories_per_kg", "Fat_Percentage")

# Apply scaling on train and test sets
train_regression[numeric_features] <- scale(train_regression[numeric_features])
test_regression[numeric_features] <- scale(test_regression[numeric_features])

# Convert Experience Level to a categorical variable
train_regression$Experience_Level <- as.factor(train_regression$Experience_Level)
test_regression$Experience_Level <- as.factor(test_regression$Experience_Level)

# Model 1: Linear Regression ----
# Fit Linear Regression
lm_model <- lm(Calories_Burned ~ ., data = train_regression)
summary(lm_model)
# Predict on test set
pred_lm <- predict(lm_model, newdata = test_regression)

# Compute error metrics
lm_mse <- mean((test_regression$Calories_Burned - pred_lm)^2)
lm_rmse <- sqrt(lm_mse)
lm_mae <- mean(abs(test_regression$Calories_Burned - pred_lm))
lm_mape <- mean(abs((test_regression$Calories_Burned - pred_lm) / test_regression$Calories_Burned)) * 100
lm_r2 <- summary(lm_model)$r.squared
lm_adj_r2 <- summary(lm_model)$adj.r.squared

# Model 2: Polynomial Regression (Adding Quadratic Terms) -----
lm_poly_model <- lm(Calories_Burned ~ Session_Duration_hours + I(Session_Duration_hours^2) + 
                      Age + I(Age^2) + Avg_BPM + heart_rate_reserve + Gender +
                      workout_intensity_score + calories_per_kg + Fat_Percentage + Experience_Level, 
                    data = train_regression)
summary(lm_poly_model)
# Predict on test set
pred_lm_poly <- predict(lm_poly_model, newdata = test_regression)

# Compute error metrics
lm_poly_mse <- mean((test_regression$Calories_Burned - pred_lm_poly)^2)
lm_poly_rmse <- sqrt(lm_poly_mse)
lm_poly_mae <- mean(abs(test_regression$Calories_Burned - pred_lm_poly))
lm_poly_mape <- mean(abs((test_regression$Calories_Burned - pred_lm_poly) / test_regression$Calories_Burned)) * 100
lm_poly_r2 <- summary(lm_poly_model)$r.squared
lm_poly_adj_r2 <- summary(lm_poly_model)$adj.r.squared

# Model 3: Random Forest Regression ----
# Fit Random Forest Model
set.seed(123)
rf_model <- randomForest(Calories_Burned ~ ., data = train_regression, ntree = 100, importance = TRUE)

# Predict on test set
pred_rf <- predict(rf_model, newdata = test_regression)

# Compute error metrics
rf_mse <- mean((test_regression$Calories_Burned - pred_rf)^2)
rf_rmse <- sqrt(rf_mse)
rf_mae <- mean(abs(test_regression$Calories_Burned - pred_rf))
rf_mape <- mean(abs((test_regression$Calories_Burned - pred_rf) / test_regression$Calories_Burned)) * 100
rf_r2 <- 1 - (sum((test_regression$Calories_Burned - pred_rf)^2) / sum((test_regression$Calories_Burned - mean(test_regression$Calories_Burned))^2))
rf_adj_r2 <- 1 - ((1 - rf_r2) * (nrow(test_regression) - 1) / (nrow(test_regression) - ncol(test_regression) - 1))

# 10. Report Model Performance -----
# Combine results into a dataframe
model_results <- data.frame(
  Model = c("Linear Regression", "Polynomial Regression", "Random Forest"),
  MSE = c(lm_mse, lm_poly_mse, rf_mse),
  RMSE = c(lm_rmse, lm_poly_rmse, rf_rmse),
  MAE = c(lm_mae, lm_poly_mae, rf_mae),
  MAPE = c(lm_mape, lm_poly_mape, rf_mape),
  R_squared = c(lm_r2, lm_poly_r2, rf_r2),
  Adjusted_R_squared = c(lm_adj_r2, lm_poly_adj_r2, rf_adj_r2)
)

# Display the results in a structured table
kable(model_results, caption = "Model Performance Comparison")

# Classification - Cluster assignment --------------------
# Load required libraries
library(clustMixType)
library(dplyr)

# Sample new member data (structured like the original dataset)
new_member_data <- data.frame(
  Age = 35, Weight_kg = 85, Height_m = 1.75, 
  Avg_BPM = 140, Session_Duration_hours = 1.2, 
  Calories_Burned = 800, BMI = 27, Experience_Level = 2,
  Gender = "Male", Workout_Yoga = 0, Workout_HIIT = 1,
  Workout_Strength = 0, Workout_Cardio = 1,
  BMI_Underweight = 0, BMI_Normal = 1, BMI_Overweight = 0, BMI_Obese = 0,
  Age_18_25 = 0, Age_26_35 = 1, Age_36_45 = 0, Age_46_plus = 0
)

# Ensure it's a single row for prediction
new_member_data <- new_member_data[1, , drop = FALSE]

# Convert categorical variables to factors (matching the original clustering data)
categorical_columns <- c("Gender", "Workout_Yoga", "Workout_HIIT", "Workout_Strength", 
                         "Workout_Cardio", "BMI_Underweight", "BMI_Normal", "BMI_Overweight", 
                         "BMI_Obese", "Age_18_25", "Age_26_35", "Age_36_45", "Age_46_plus")

new_member_data[categorical_columns] <- lapply(new_member_data[categorical_columns], as.factor)

# Check for missing columns & add them
missing_columns <- setdiff(colnames(df_clustering), colnames(new_member_data))
for (col in missing_columns) {
  new_member_data[[col]] <- NA  # Assign NA to missing columns
}

# Ensure correct column order
new_member_data <- new_member_data[, colnames(df_clustering), drop = FALSE]

# Ensure numerical columns are correctly formatted
numeric_columns <- setdiff(colnames(new_member_data), categorical_columns)
new_member_data[numeric_columns] <- lapply(new_member_data[numeric_columns], as.numeric)

# Predict cluster assignment using K-Prototypes model
new_cluster <- predict(kproto_3, new_member_data)

# Extract and print the assigned cluster
new_cluster <- new_cluster[1]
print(paste("New member assigned to Cluster:", new_cluster))


# 11. Model Evaluation ----
# Evaluate Feature Importance for Random Forest
importance_rf <- importance(rf_model)
print(importance_rf)

# Plot Feature Importance
varImpPlot(rf_model, main = "Feature Importance - Random Forest")

# Expand Plot Window
# Residual Analysis for Linear Regression
par(mfrow=c(2,2))
plot(lm_model)

# Residual Analysis for Polynomial Regression
par(mfrow=c(2,2))
plot(lm_poly_model)

# Interpretation: ----
# - Linear Regression performs the best with the highest Adjusted R² and lowest error metrics.
# - Random Forest has high variance and does not generalize well.
# - Polynomial Regression does not significantly improve over Linear Regression.

# Final Model Selected: Linear Regression



