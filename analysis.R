# ============================================================
# PROJECT: Biological Data Analysis and Visualisation Using R
# AUTHOR: Ochai Moses Ameh
# PURPOSE: Analyse biological data using R
# ============================================================

# 1. Load the biological dataset

# The CSV file must be in the current working directory.
data_file <- "plant_growth.csv"

if (!file.exists(data_file)) {
  stop("Error: plant_growth.csv was not found.")
}

plant_data <- read.csv(data_file, stringsAsFactors = FALSE)

# 2. Inspect the dataset

cat("BIOLOGICAL DATA ANALYSIS\n")
cat("========================\n\n")

cat("First six rows:\n")
print(head(plant_data))

cat("\nDataset dimensions:\n")
print(dim(plant_data))

cat("\nColumn names:\n")
print(names(plant_data))

# 3. Validate the dataset

required_columns <- c(
  "Treatment",
  "Plant_Height_cm",
  "Chlorophyll"
)

if (!all(required_columns %in% names(plant_data))) {
  stop("Error: One or more required columns are missing.")
}

if (anyNA(plant_data[required_columns])) {
  stop("Error: Missing values were found in the dataset.")
}

if (!is.numeric(plant_data$Plant_Height_cm) ||
    !is.numeric(plant_data$Chlorophyll)) {
  stop("Error: Measurement columns must contain numbers.")
}

if (length(unique(plant_data$Treatment)) < 2) {
  stop("Error: At least two treatment groups are required.")
}

cat("\nData validation completed successfully.\n")

# 4. Calculate descriptive statistics

cat("\nPLANT HEIGHT SUMMARY\n")
cat("====================\n")

height_summary <- aggregate(
  Plant_Height_cm ~ Treatment,
  data = plant_data,
  FUN = function(x) {
    c(
      Mean = mean(x),
      SD = sd(x),
      Minimum = min(x),
      Maximum = max(x)
    )
  }
)

height_summary <- do.call(
  data.frame,
  height_summary
)

names(height_summary) <- c(
  "Treatment",
  "Mean_Height",
  "SD",
  "Minimum",
  "Maximum"
)

print(height_summary, row.names = FALSE)

# 5. Summarise chlorophyll measurements

cat("\nCHLOROPHYLL SUMMARY\n")
cat("===================\n")

chlorophyll_summary <- aggregate(
  Chlorophyll ~ Treatment,
  data = plant_data,
  FUN = mean
)

names(chlorophyll_summary)[2] <- "Mean_Chlorophyll"

print(chlorophyll_summary, row.names = FALSE)

# 6. Examine correlation

cat("\nCORRELATION ANALYSIS\n")
cat("====================\n")

correlation <- cor(
  plant_data$Plant_Height_cm,
  plant_data$Chlorophyll
)

cat(
  "Correlation between plant height and chlorophyll:",
  round(correlation, 3),
  "\n"
)

# 7. Compare plant height using ANOVA

cat("\nANALYSIS OF VARIANCE (ANOVA)\n")
cat("============================\n")

anova_result <- aov(
  Plant_Height_cm ~ Treatment,
  data = plant_data
)

anova_table <- summary(anova_result)
print(anova_table)

p_value <- anova_table[[1]][["Pr(>F)"]][1]

# 8. Interpret the ANOVA result

cat("\nINTERPRETATION\n")
cat("==============\n")

if (is.na(p_value)) {
  cat("The ANOVA p-value could not be calculated.\n")
} else if (p_value < 0.05) {
  cat(
    "There is statistical evidence of a difference",
    "in mean plant height among treatments (p < 0.05).\n"
  )
} else {
  cat(
    "There is insufficient statistical evidence of",
    "a difference in mean plant height among treatments",
    "at the 0.05 significance level.\n"
  )
}

# 9. Visualise plant height

boxplot(
  Plant_Height_cm ~ Treatment,
  data = plant_data,
  main = "Plant Height Under Different Treatments",
  xlab = "Treatment",
  ylab = "Plant Height (cm)",
  col = c("lightgreen", "lightblue", "salmon")
)

# 10. Final reminder

cat("\nNOTE\n")
cat("====\n")
cat("These are illustrative data for learning purposes.\n")
cat("They are not actual experimental measurements.\n")