# --- 1. Import Libraries ---
library(tidyverse)  # For data manipulation and plotting
library(gridExtra)  # To display plots side-by-side

# --- 2. Import Dataset ---
df <- read_csv("D:\\DMA LAB\\lab project\\student_finance_dataset_balanced.csv")
head(df)
tail(df)
str(df)
# --- 3. Data Preprocessing ---

# A. Handle Missing Values (Fill with Mean)
df_clean <- df %>%
  mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm = TRUE))))

# --- 4. Outlier Removal with "Before vs After" Visualization ---

# Let's focus on 'Monthly_Allowance' as our target variable for this example

# PLOT 1: BEFORE Removing Outliers
plot_before <- ggplot(df_clean, aes(y = Monthly_Allowance)) +
  geom_boxplot(fill = "tomato", color = "black") +
  labs(title = "Before: With Outliers", y = "Monthly Allowance") +
  theme_minimal()

# FUNCTION: Remove Outliers using IQR
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  return(ifelse(x < lower | x > upper, NA, x))
}

# Apply removal to the dataset
df_no_outliers <- df_clean %>%
  mutate(Monthly_Allowance = remove_outliers(Monthly_Allowance)) %>%
  na.omit() # Remove rows that became NA

# PLOT 2: AFTER Removing Outliers
plot_after <- ggplot(df_no_outliers, aes(y = Monthly_Allowance)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "After: Outliers Removed", y = "Monthly Allowance") +
  theme_minimal()

# DISPLAY SIDE-BY-SIDE
grid.arrange(plot_before, plot_after, ncol = 2)

# --- 5. Encoding & Normalization ---

# A. Encoding Categorical Variables
# Gender: Female=0, Male=1 | Budget: No=0, Yes=1
df_final <- df_no_outliers %>%
  mutate(
    Gender_Encoded = ifelse(Gender == "Male", 1, 0),
    Budget_Encoded = ifelse(Budget == "Yes", 1, 0)
  )

# B. Normalization (Min-Max Scaling to 0-1 range)
# We apply this to Age, Income, and Expenses
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_final <- df_final %>%
  mutate(across(c(Age, Monthly_Allowance, Month_End_Leftover), normalize))

# --- 6. Final Inspection ---
# View the first few rows of your fully processed data
head(df_final)
summary(df_final)

ggplot(df_clean, aes(x = Month_End_Leftover)) +
  geom_histogram(binwidth = 500, fill = "cornflowerblue", color = "black") +
  labs(title = "Distribution of Money Left at Month End",
       x = "Amount Leftover",
       y = "Count of Students") +
  theme_minimal()


# =========================================
# --- 7. DATA VISUALIZATION & PATTERNS ---
# =========================================

# A. Distribution of Month End Leftover
# (Using df_no_outliers so we see real currency amounts)
ggplot(df_no_outliers, aes(x = Month_End_Leftover)) +
  geom_histogram(binwidth = 500, fill = "cornflowerblue", color = "black") +
  labs(title = "Distribution of Money Left at Month End",
       subtitle = "Most students have between 2000-4000 left",
       x = "Amount Leftover",
       y = "Count of Students") +
  theme_minimal()

# B. Income vs. Leftover (colored by Budget)
# Pattern: Do students who budget save more?
ggplot(df_no_outliers, aes(x = Monthly_Allowance, y = Month_End_Leftover, color = Budget)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) + # Adds trend lines
  labs(title = "Relationship: Allowance vs. Leftover Money",
       subtitle = "Trend lines show if budgeting affects savings",
       x = "Monthly Allowance",
       y = "Month End Leftover") +
  theme_minimal()

# C. Spending Habits by Gender (Box Plots)
# Pattern: Compare spending categories between genders
df_long <- df_no_outliers %>%
  select(Gender, ends_with("Expenses")) %>%
  pivot_longer(cols = -Gender, names_to = "Expense_Category", values_to = "Amount")

ggplot(df_long, aes(x = Expense_Category, y = Amount, fill = Gender)) +
  geom_boxplot() +
  coord_flip() + # Makes labels readable
  labs(title = "Expense Distribution by Category and Gender",
       x = "Expense Category",
       y = "Amount Spent") +
  theme_minimal()

# D. Correlation Matrix (Heatmap)
# Pattern: See which variables are linked (using df_final to include encoded cols)
num_cols <- df_final %>% select_if(is.numeric)
cor_matrix <- cor(num_cols)

corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.7, # Make text smaller to fit
         title = "Correlation Heatmap", mar=c(0,0,1,0))

# --- 8. Export Data to CSV ---

# Option A: Export the fully processed data (Normalized 0-1 values, Encoded)
# Use this if you are feeding the data into a Machine Learning model.
write_csv(df_final, "D:\\DMA LAB\\lab project\\student_finance_final_dataset.csv")

print("Files exported successfully!")