source("adaptive_learning.R") # Load predefined functions
set.seed(123)          # Fix random seed for reproducibility

library(readxl)

# Read Excel file directly
df <- read_excel("Data_Gilles.xlsx")

alpha <- df[["alpha"]]
dalpha_dt <- df[["dalpha/dt"]]

# Train models for positive and negative beta separately
metamodel_pos <- run_analysis_adaptive("positive_beta.csv", 15, 35)
metamodel_neg <- run_analysis_adaptive("negative_beta.csv", 15, 65)

# Filter rows where 'betta=dT/dt' > 0
beta_pos <- subset(df, `betta=dT/dt` > 0)
beta_pos <- beta_pos[, c("alpha", "betta=dT/dt", "dbetta/dt", "dalpha/dt")]
beta_pos <- na.omit(beta_pos)
input_pos <- beta_pos[, c("alpha", "betta=dT/dt", "dbetta/dt")]
# Use positive beta model
prediction_pos <- predict(metamodel_pos, input_pos, type = 'UK', checkNames = FALSE)$mean
alpha_pos <- beta_pos[["alpha"]]

# Filter rows where 'betta=dT/dt' < 0
beta_neg <- subset(df, `betta=dT/dt` < 0)
beta_neg <- beta_neg[, c("alpha", "betta=dT/dt", "dbetta/dt", "dalpha/dt")]
beta_neg <- na.omit(beta_neg)
input_neg <- beta_neg[, c("alpha", "betta=dT/dt", "dbetta/dt")]
# Use negative beta model
prediction_neg <- predict(metamodel_neg, input_neg, type = 'UK', checkNames = FALSE)$mean
alpha_neg <- beta_neg[["alpha"]]

# Plot original data points in blue
plot(alpha, dalpha_dt, 
     pch = 16, col = "blue", cex = 0.5,
     xlab = "Alpha", ylab = "dAlpha/dt",
     main = "Comparison of results using Adaptive Learning")

# Add prediction points for positive beta in orange
points(alpha_pos, prediction_pos, 
       pch = 16, col = "orange", cex = 0.5)

# Add prediction points for negative beta in green
points(alpha_neg, prediction_neg, 
       pch = 16, col = "green", cex = 0.5)

grid(nx = 10, ny = 10, col = "gray", lty = 2, lwd = 1)

# Add legend
legend("topright", legend = c("Original data", "Prediction beta > 0", "Prediction beta < 0"),
       col = c("blue", "orange", "green"), pch = 16, cex = 0.8)