# ============================================================================
#  Student Academic Performance - Statistical Analysis in R
# ============================================================================
#  This script performs comprehensive statistical analysis on 100 students'
#  academic data loaded from a CSV file.
# ============================================================================

# --- 1. Setup & Load Data ---------------------------------------------------

# Install required packages (uncomment if not installed)
# install.packages(c("ggplot2", "dplyr", "corrplot", "psych", "gridExtra"))

library(ggplot2)
library(dplyr)
library(corrplot)
library(psych)
library(gridExtra)

# Set working directory to the location of this script (adjust if needed)
# setwd("C:/Users/MCA 2/.gemini/antigravity/scratch/student-analysis")

# Load the CSV data
data <- read.csv("student_data.csv", header = TRUE, stringsAsFactors = FALSE)

# Display basic structure
cat("=====================================================\n")
cat("   STUDENT ACADEMIC PERFORMANCE - STATISTICAL REPORT\n")
cat("=====================================================\n\n")

cat("--- 1. DATA OVERVIEW ---\n\n")
cat("Number of Students:", nrow(data), "\n")
cat("Number of Variables:", ncol(data), "\n\n")
cat("Structure of Data:\n")
str(data)
cat("\nFirst 6 Records:\n")
print(head(data))


# --- 2. Descriptive Statistics -----------------------------------------------

cat("\n\n--- 2. DESCRIPTIVE STATISTICS ---\n\n")

# Summary statistics for all numeric columns
cat("Summary Statistics (All Variables):\n")
print(summary(data[, c("Age", "Attendance_Pct", "Study_Hours_Per_Week",
                         "Math", "Science", "English", "Hindi",
                         "Computer_Science", "Total_Marks", "Percentage")]))

# Detailed descriptive stats using psych::describe
cat("\n\nDetailed Descriptive Statistics:\n")
desc_stats <- describe(data[, c("Attendance_Pct", "Study_Hours_Per_Week",
                                  "Math", "Science", "English", "Hindi",
                                  "Computer_Science", "Total_Marks", "Percentage")])
print(desc_stats)


# --- 3. Subject-wise Analysis ------------------------------------------------

cat("\n\n--- 3. SUBJECT-WISE ANALYSIS ---\n\n")

subjects <- c("Math", "Science", "English", "Hindi", "Computer_Science")
subject_stats <- data.frame(
  Subject     = subjects,
  Mean        = sapply(data[subjects], mean),
  Median      = sapply(data[subjects], median),
  Std_Dev     = sapply(data[subjects], sd),
  Min         = sapply(data[subjects], min),
  Max         = sapply(data[subjects], max),
  Variance    = sapply(data[subjects], var),
  Skewness    = sapply(data[subjects], function(x) psych::skew(x)),
  Kurtosis    = sapply(data[subjects], function(x) psych::kurtosi(x))
)
rownames(subject_stats) <- NULL
cat("Subject-wise Statistics:\n")
print(subject_stats)


# --- 4. Gender-wise Analysis -------------------------------------------------

cat("\n\n--- 4. GENDER-WISE ANALYSIS ---\n\n")

gender_stats <- data %>%
  group_by(Gender) %>%
  summarise(
    Count               = n(),
    Avg_Percentage      = round(mean(Percentage), 2),
    Avg_Attendance      = round(mean(Attendance_Pct), 2),
    Avg_Study_Hours     = round(mean(Study_Hours_Per_Week), 2),
    Avg_Math            = round(mean(Math), 2),
    Avg_Science         = round(mean(Science), 2),
    Avg_English         = round(mean(English), 2),
    Avg_Hindi           = round(mean(Hindi), 2),
    Avg_Computer_Science = round(mean(Computer_Science), 2)
  )
cat("Gender-wise Performance:\n")
print(as.data.frame(gender_stats))


# --- 5. Grade Distribution ---------------------------------------------------

cat("\n\n--- 5. GRADE DISTRIBUTION ---\n\n")

grade_dist <- data %>%
  group_by(Grade) %>%
  summarise(
    Count      = n(),
    Percentage = round(n() / nrow(data) * 100, 1)
  ) %>%
  arrange(desc(Count))
cat("Grade Distribution:\n")
print(as.data.frame(grade_dist))


# --- 6. Correlation Analysis -------------------------------------------------

cat("\n\n--- 6. CORRELATION ANALYSIS ---\n\n")

cor_vars <- data[, c("Attendance_Pct", "Study_Hours_Per_Week",
                      "Math", "Science", "English", "Hindi",
                      "Computer_Science", "Percentage")]
cor_matrix <- cor(cor_vars)
cat("Correlation Matrix:\n")
print(round(cor_matrix, 3))


# --- 7. Hypothesis Testing ---------------------------------------------------

cat("\n\n--- 7. HYPOTHESIS TESTING ---\n\n")

# 7a. T-Test: Is there a significant difference in percentage between genders?
cat("T-Test: Gender Difference in Overall Percentage\n")
cat("H0: No significant difference in mean percentage between Male and Female\n")
cat("H1: Significant difference exists\n\n")

male_pct   <- data$Percentage[data$Gender == "Male"]
female_pct <- data$Percentage[data$Gender == "Female"]
t_result   <- t.test(male_pct, female_pct)
print(t_result)

if (t_result$p.value < 0.05) {
  cat("\nConclusion: REJECT H0 — Significant gender difference exists (p < 0.05)\n")
} else {
  cat("\nConclusion: FAIL TO REJECT H0 — No significant gender difference (p >= 0.05)\n")
}

# 7b. ANOVA: Is there a significant difference in percentage across age groups?
cat("\n\nANOVA: Percentage Difference Across Age Groups\n")
cat("H0: Mean percentage is the same across all age groups\n")
cat("H1: At least one age group has a different mean\n\n")

anova_result <- aov(Percentage ~ factor(Age), data = data)
print(summary(anova_result))
anova_p <- summary(anova_result)[[1]]$`Pr(>F)`[1]
if (anova_p < 0.05) {
  cat("Conclusion: REJECT H0 — Significant difference across age groups (p < 0.05)\n")
} else {
  cat("Conclusion: FAIL TO REJECT H0 — No significant difference across ages (p >= 0.05)\n")
}

# 7c. Chi-Square Test: Association between Gender and Grade
cat("\n\nChi-Square Test: Association Between Gender and Grade\n")
cat("H0: Gender and Grade are independent\n")
cat("H1: Gender and Grade are associated\n\n")

chi_table  <- table(data$Gender, data$Grade)
chi_result <- chisq.test(chi_table)
print(chi_result)

if (chi_result$p.value < 0.05) {
  cat("\nConclusion: REJECT H0 — Gender and Grade are significantly associated\n")
} else {
  cat("\nConclusion: FAIL TO REJECT H0 — No significant association\n")
}


# --- 8. Regression Analysis --------------------------------------------------

cat("\n\n--- 8. REGRESSION ANALYSIS ---\n\n")

# 8a. Simple Linear Regression: Percentage ~ Study_Hours_Per_Week
cat("Simple Linear Regression: Percentage ~ Study Hours\n")
model_simple <- lm(Percentage ~ Study_Hours_Per_Week, data = data)
print(summary(model_simple))

# 8b. Multiple Linear Regression: Percentage ~ Attendance + Study Hours
cat("\nMultiple Linear Regression: Percentage ~ Attendance + Study Hours\n")
model_multi <- lm(Percentage ~ Attendance_Pct + Study_Hours_Per_Week, data = data)
print(summary(model_multi))


# --- 9. Top & Bottom Performers ----------------------------------------------

cat("\n\n--- 9. TOP 10 AND BOTTOM 10 PERFORMERS ---\n\n")

cat("Top 10 Students:\n")
top10 <- data %>% arrange(desc(Percentage)) %>% head(10)
print(top10[, c("Student_ID", "Name", "Percentage", "Grade")])

cat("\nBottom 10 Students:\n")
bottom10 <- data %>% arrange(Percentage) %>% head(10)
print(bottom10[, c("Student_ID", "Name", "Percentage", "Grade")])


# ============================================================================
# --- 10. DATA VISUALIZATIONS ------------------------------------------------
# ============================================================================

cat("\n\n--- 10. GENERATING VISUALIZATIONS ---\n\n")

# 10a. Histogram: Overall Percentage Distribution
p1 <- ggplot(data, aes(x = Percentage)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(Percentage)), color = "red",
             linetype = "dashed", size = 1) +
  labs(title = "Distribution of Student Percentage",
       subtitle = paste("Mean =", round(mean(data$Percentage), 1),
                        "| SD =", round(sd(data$Percentage), 1)),
       x = "Percentage", y = "Frequency") +
  theme_minimal()
ggsave("plot_01_percentage_distribution.png", p1, width = 8, height = 5, dpi = 150)
cat("Saved: plot_01_percentage_distribution.png\n")

# 10b. Boxplot: Subject-wise Marks
subject_long <- tidyr::pivot_longer(data, cols = all_of(subjects),
                                     names_to = "Subject", values_to = "Marks")
p2 <- ggplot(subject_long, aes(x = Subject, y = Marks, fill = Subject)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red") +
  labs(title = "Subject-wise Marks Distribution (Boxplot)",
       x = "Subject", y = "Marks") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("plot_02_subject_boxplot.png", p2, width = 8, height = 5, dpi = 150)
cat("Saved: plot_02_subject_boxplot.png\n")

# 10c. Bar Chart: Grade Distribution
p3 <- ggplot(data, aes(x = factor(Grade, levels = c("A", "B", "C", "D", "F")),
                        fill = Grade)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Grade Distribution of Students",
       x = "Grade", y = "Number of Students") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("plot_03_grade_distribution.png", p3, width = 8, height = 5, dpi = 150)
cat("Saved: plot_03_grade_distribution.png\n")

# 10d. Scatter Plot: Study Hours vs Percentage with Regression Line
p4 <- ggplot(data, aes(x = Study_Hours_Per_Week, y = Percentage)) +
  geom_point(aes(color = Gender), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue", linetype = "dashed") +
  labs(title = "Study Hours vs Percentage (with Regression Line)",
       x = "Study Hours Per Week", y = "Percentage (%)") +
  theme_minimal()
ggsave("plot_04_studyhours_vs_percentage.png", p4, width = 8, height = 5, dpi = 150)
cat("Saved: plot_04_studyhours_vs_percentage.png\n")

# 10e. Scatter Plot: Attendance vs Percentage
p5 <- ggplot(data, aes(x = Attendance_Pct, y = Percentage)) +
  geom_point(aes(color = Grade), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(title = "Attendance vs Percentage",
       x = "Attendance (%)", y = "Percentage (%)") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()
ggsave("plot_05_attendance_vs_percentage.png", p5, width = 8, height = 5, dpi = 150)
cat("Saved: plot_05_attendance_vs_percentage.png\n")

# 10f. Gender-wise Comparison Boxplot
p6 <- ggplot(data, aes(x = Gender, y = Percentage, fill = Gender)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(title = "Gender-wise Performance Comparison",
       x = "Gender", y = "Percentage (%)") +
  scale_fill_manual(values = c("Female" = "#E91E63", "Male" = "#2196F3")) +
  theme_minimal()
ggsave("plot_06_gender_comparison.png", p6, width = 8, height = 5, dpi = 150)
cat("Saved: plot_06_gender_comparison.png\n")

# 10g. Correlation Heatmap
png("plot_07_correlation_heatmap.png", width = 800, height = 700)
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.8,
         tl.col = "black", tl.cex = 0.9,
         title = "Correlation Heatmap",
         mar = c(0, 0, 2, 0))
dev.off()
cat("Saved: plot_07_correlation_heatmap.png\n")

# 10h. Pie Chart: Grade Distribution
grade_counts <- table(data$Grade)
grade_df <- data.frame(Grade = names(grade_counts), Count = as.integer(grade_counts))
grade_df$Pct <- round(grade_df$Count / sum(grade_df$Count) * 100, 1)
grade_df$Label <- paste0(grade_df$Grade, "\n(", grade_df$Count, " - ", grade_df$Pct, "%)")

p8 <- ggplot(grade_df, aes(x = "", y = Count, fill = Grade)) +
  geom_bar(stat = "identity", width = 1, alpha = 0.8) +
  coord_polar("y") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 3.5) +
  labs(title = "Grade Distribution (Pie Chart)") +
  scale_fill_brewer(palette = "Set2") +
  theme_void()
ggsave("plot_08_grade_pie_chart.png", p8, width = 7, height = 7, dpi = 150)
cat("Saved: plot_08_grade_pie_chart.png\n")

# 10i. Bar Chart: Average Subject Marks
avg_marks <- data.frame(
  Subject = subjects,
  Average = sapply(data[subjects], mean)
)
p9 <- ggplot(avg_marks, aes(x = reorder(Subject, -Average), y = Average, fill = Subject)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(Average, 1)), vjust = -0.5) +
  labs(title = "Average Marks by Subject",
       x = "Subject", y = "Average Marks") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("plot_09_avg_subject_marks.png", p9, width = 8, height = 5, dpi = 150)
cat("Saved: plot_09_avg_subject_marks.png\n")

# 10j. Density Plot: Percentage by Gender
p10 <- ggplot(data, aes(x = Percentage, fill = Gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Percentage Distribution by Gender (Density Plot)",
       x = "Percentage (%)", y = "Density") +
  scale_fill_manual(values = c("Female" = "#E91E63", "Male" = "#2196F3")) +
  theme_minimal()
ggsave("plot_10_gender_density.png", p10, width = 8, height = 5, dpi = 150)
cat("Saved: plot_10_gender_density.png\n")


# --- Final Summary -----------------------------------------------------------

cat("\n=====================================================\n")
cat("   ANALYSIS COMPLETE!\n")
cat("=====================================================\n")
cat("Total Students Analyzed:", nrow(data), "\n")
cat("Overall Class Average  :", round(mean(data$Percentage), 2), "%\n")
cat("Highest Percentage     :", max(data$Percentage), "%\n")
cat("Lowest Percentage      :", min(data$Percentage), "%\n")
cat("Pass Rate (>= 40%)     :", sum(data$Percentage >= 40), "/", nrow(data), "\n")
cat("Fail Rate (< 40%)      :", sum(data$Percentage < 40), "/", nrow(data), "\n")
cat("10 Visualizations saved to working directory.\n")
cat("=====================================================\n")
