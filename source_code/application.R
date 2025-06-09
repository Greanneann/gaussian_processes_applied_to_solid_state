source("functions.R")
source("data_preprocessing.R")
source("adaptive_learning.R")
source("twinning_sampling.R")
set.seed(123)

# Part with material RT58
process_excel_file("Data_Gilles.xlsx")

df <- read_excel("Data_Gilles.xlsx")

alpha <- df[["alpha"]]
beta <- df[["beta"]]
dalpha_dt <- df[["dalpha/dt"]]

# Train models for positive and negative beta separately
metamodel_pos <- run_analysis_adaptive("positive_beta.csv", 15, 35)
metamodel_neg <- run_analysis_adaptive("negative_beta.csv", 15, 65)

# Filter rows where 'betta=dT/dt' > 0
beta_pos <- subset(df, `beta` > 0)
beta_pos <- beta_pos[, c("alpha", "beta", "dbeta/dt", "dalpha/dt")]
beta_pos <- na.omit(beta_pos)
input_pos <- beta_pos[, c("alpha", "beta", "dbeta/dt")]
# Use positive beta model
prediction_pos <- predict(metamodel_pos, input_pos, type = 'UK', checkNames = FALSE)$mean
alpha_pos <- beta_pos[["alpha"]]

# Filter rows where 'betta=dT/dt' < 0
beta_neg <- subset(df, `beta` < 0)
beta_neg <- beta_neg[, c("alpha", "beta", "dbeta/dt", "dalpha/dt")]
beta_neg <- na.omit(beta_neg)
input_neg <- beta_neg[, c("alpha", "beta", "dbeta/dt")]
# Use negative beta model
prediction_neg <- predict(metamodel_neg, input_neg, type = 'UK', checkNames = FALSE)$mean
alpha_neg <- beta_neg[["alpha"]]

########### Comparing predictions made by models with original data
# Plot original data points in blue
plot(alpha, dalpha_dt, 
     pch = 16, col = "blue", cex = 0.5,
     xlab = "Alpha", ylab = "dAlpha/dt",
     main = "Comparison of results using adaptive learning")
grid(nx = 10, ny = 10, col = "gray", lty = 2, lwd = 1)

# Add prediction points for positive beta in orange
points(alpha_pos, prediction_pos, 
       pch = 16, col = "orange", cex = 0.5)

# Add prediction points for negative beta in green
points(alpha_neg, prediction_neg, 
       pch = 16, col = "green", cex = 0.5)

# Add legend
legend("topright", legend = c("Original data", "Prediction beta > 0", "Prediction beta < 0"),
       col = c("blue", "orange", "green"), pch = 16, cex = 0.8)

######### Comparing predictions by Gilles's formula with original data

dalpha_dt_Gilles <- calculate_dalpha_dt_Gilles(alpha,beta)

# Plot original data points in blue
plot(alpha, dalpha_dt, 
     pch = 16, col = "blue", cex = 0.5,
     xlab = "Alpha", ylab = "dAlpha/dt",
     main = "Comparison of results using Gilles's formula")
grid(nx = 10, ny = 10, col = "gray", lty = 2, lwd = 1)

points(alpha, dalpha_dt_Gilles, 
       pch = 16, col = "orange", cex = 0.5)

legend("topright", legend = c("Original data", "Gilles's formula"),
       col = c("blue", "orange"), pch = 16, cex = 0.8)

#########################################################################3
# Application of models to a new material
process_excel_file("partiels_these_pour_Anna.xlsx")

df_pos <- read.csv("positive_beta.csv")
alpha <- df_pos[["alpha"]]
dalpha_dt <- df_pos[["dalpha_dt"]]
input_pos <- df_pos[, c("alpha", "beta", "dbeta_dt")]
obs_range <- seq(100, 400, by = 50)

# Loop over the observation range
for (obs in obs_range) {
  print(paste("Number of observations:", obs))
  # Train the model with the current number of observations
  metamodel <- run_analysis_twinning("positive_beta.csv", obs)
  
  # Predict using the current metamodel
  preds <- predict(metamodel, input_pos, type = 'UK', checkNames = FALSE)$mean
  
  # Create a new plot for this iteration
  plot(alpha, dalpha_dt, 
       pch = 16, col = "blue", cex = 0.5,
       xlab = "Alpha", ylab = "dAlpha/dt",
       main = paste("Twinning Sampling Positive Beta (Obs =", obs, ")"))
  grid(nx = 10, ny = 10, col = "gray", lty = 2, lwd = 1)
  
  # Add prediction points in orange
  points(alpha, preds, 
         pch = 16, col = "orange", cex = 0.5)
}

##############
# Running analysis for negative beta
df_neg <- read.csv("negative_beta.csv")
alpha <- df_neg[["alpha"]]
dalpha_dt <- df_neg[["dalpha_dt"]]
input_neg <- df_neg[, c("alpha", "beta", "dbeta_dt")]

obs_range_neg <- seq(100, 400, by = 50)

# Loop over the observation range
for (obs in obs_range_neg) {
  print(paste("Number of observations:", obs))
  # Train the model with the current number of observations
  metamodel <- run_analysis_twinning("negative_beta.csv", obs)
  
  # Predict using the current metamodel
  preds <- predict(metamodel, input_neg, type = 'UK', checkNames = FALSE)$mean
  
  # Create a new plot for this iteration
  plot(alpha, dalpha_dt, 
       pch = 16, col = "blue", cex = 0.5,
       xlab = "Alpha", ylab = "dAlpha/dt",
       main = paste("Twinning Sampling Negative Beta (Obs =", obs, ")"))
  grid(nx = 10, ny = 10, col = "gray", lty = 2, lwd = 1)
  
  # Add prediction points in orange
  points(alpha, preds, 
         pch = 16, col = "orange", cex = 0.5)
}


