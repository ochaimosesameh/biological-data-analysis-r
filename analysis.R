# ============================================================
# PROJECT: Biological Data Analysis and Visualisation Using R
# AUTHOR: Ochai Moses Ameh
# PURPOSE: Analyse illustrative biological data using R
# ============================================================

# 1. Create an illustrative plant growth dataset

plant_data <- data.frame(
  Treatment = rep(c("Control", "Moderate Stress", "Severe Stress"),
                  each = 5),
  Plant_Height_cm = c(
    25, 27, 26, 24, 28,
    20, 22, 21, 19, 23,
    14, 16, 15, 13, 17
  ),
  Chlorophyll = c(
    42, 45, 44, 41, 46,
    35, 38, 36, 34, 37,
    25, 28, 26, 24, 27
  )
)

# 2. Display the dataset

cat("BIOLOGICAL DATA ANALYSIS\n")
cat("========================\n\n")

cat("First six rows of the dataset:\n")
print(head(plant_data))

cat("\nDataset dimensions:\n")
print(dim(plant_data))

cat("\nColumn names:\n")
print(names(plant_data))

# 3. Check for missing values

cat("\nMissing values in each column:\n")
print(colSums(is.na(plant_data)))

# 4. Calculate descriptive statistics

cat("\nPLANT HEIGHT SUMMARY BY TREATMENT\n")
cat("=================================\n")

height_mean <- aggregate(
  Plant_Height_cm ~ Treatment,
  data = plant_data,
  FUN = mean
)

height_sd <- aggregate(
  Plant_Height_cm ~ Treatment,
  data = plant_data,
  FUN = sd
)

print(height_mean)
cat("\nStandard deviation of plant height:\n")
print(height_sd)

# 5. Summarise chlorophyll measurements

cat("\nCHLOROPHYLL SUMMARY BY TREATMENT\n")
cat("================================\n")

chlorophyll_mean <- aggregate(
  Chlorophyll ~ Treatment,
  data = plant_data,
  FUN = mean
)

print(chlorophyll_mean)

# 6. Calculate the correlation between height and chlorophyll

cat("\nCORRELATION ANALYSIS\n")
cat("====================\n")

correlation <- cor(
  plant_data$Plant_Height_cm,
  plant_data$Chlorophyll
)

cat("Correlation coefficient:", correlation, "\n")

# 7. Compare plant height among treatments using ANOVA

cat("\nANALYSIS OF VARIANCE (ANOVA)\n")
cat("============================\n")

anova_result <- aov(
  Plant_Height_cm ~ Treatment,
  data = plant_data
)

print(summary(anova_result))

# 8. Create a boxplot

boxplot(
  Plant_Height_cm ~ Treatment,
  data = plant_data,
  main = "Plant Height Under Different Treatments",
  xlab = "Treatment",
  ylab = "Plant Height (cm)",
  col = c("lightgreen", "lightblue", "salmon")
)

# 9. Print a brief interpretation

cat("\nINTERPRETATION\n")
cat("==============\n")
cat("The dataset contains illustrative plant measurements.\n")
cat("Compare the mean heights to examine treatment differences.\n")
cat("Use the ANOVA p-value to assess statistical evidence\n")
cat("of differences in mean plant height among treatments.\n")
cat("These are demonstration data, not experimental results.\n")