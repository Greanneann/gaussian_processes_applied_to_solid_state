source("functions.R")  # Load predefined functions
set.seed(123)          # Fix random seed for reproducibility

library(DiceKriging)
library(twinning)
library(LVGP)

div <- 50

df <- read.csv("non_zero_beta.csv")

# Preparing the training set
train_twin <- create_twin_LVGP(df, div)  # Create twin dataset for training
train_input <- as.matrix(train_twin$input)  # Convert inputs to matrix
train_output <- as.matrix(train_twin$output)  # Convert outputs to matrix

# Preparing the test set
test_twin <- create_twin_LVGP(df, div)  # Create twin dataset for testing
test_input <- as.matrix(test_twin$input)  # Convert inputs to matrix
test_output <- as.matrix(test_twin$output)  # Convert outputs to matrix

# Train the LVGP model and measure time
time_taken <- system.time({
  model <- LVGP_fit(train_input, train_output, ind_qual = c(4), dim_z = 2)  # Fit the model
})
print(time_taken)  # Print time taken for model training
LVGP_plot(model)  # Visualize the model

# Predict outputs using the test set
result <- LVGP_predict(test_input, model)
prediction <- result$Y_hat  # Extract predicted outputs

Q2_TB <- Q2(test_output, prediction)
print(paste('Q2 on test basis:', Q2_TB))

par(mfrow = c(1, 1))

alpha <- test_input[, "alpha"]
beta <- test_input[, "beta"]
pos_idx <- which(beta > 0)
neg_idx <- which(beta < 0)

plot(alpha, prediction, type = "n",
     main = "Predictions by sign of beta",
     xlab = "alpha", ylab = "Predicted Output"
)

points(alpha[pos_idx], prediction[pos_idx], col = "orange", pch = 16)
points(alpha[neg_idx], prediction[neg_idx], col = "blue", pch = 16)

grid(nx = 10, ny = 15, col = "gray", lty = 2, lwd = 1)

legend("topright", legend = c("beta > 0", "beta < 0"),
       col = c("orange", "blue"), pch = 16)

dalpha_dt_Gilles = calculate_dalpha_dt_Gilles(alpha, beta)

plot(
  alpha, dalpha_dt_Gilles, type = "p",
  main = "LVGP",
  xlab = "alpha", ylab = "dalpha.dt",
  col = "orange", pch = 16,
  ylim = c(-0.0005, 0.0003)
)
grid(nx = 10, ny = 15, col = "gray", lty = 2, lwd = 1)

points(alpha, prediction, col = "deeppink", pch = 16)

legend("topright", legend = c("Reference", "Prediction"),
       col = c("orange", "deeppink"), pch = 16)


# Model evaluation on bigger subset
test <- df[1:1500,]
test <- test[order(test$alpha),]
input <- as.matrix(test[, c("alpha", "beta", "dbeta_dt", "sign_beta")])
output <- as.matrix(test[, c("dalpha_dt")])

result <- LVGP_predict(X_pl, model)
prediction <- result$Y_hat

alpha <- test[["alpha"]]
beta <- test[["beta"]]

dalpha_dt_Gilles = calculate_dalpha_dt_Gilles(alpha, beta)

par(mfrow = c(1, 1))
plot(
  alpha, dalpha_dt_Gilles, type = "p",
  main = "LVGP",
  xlab = "alpha", ylab = "dalpha/dt",
  col = "orange", pch = 16,
  ylim = c(-0.0005, 0.0003)
)

points(alpha, prediction, col = "blue", pch = 16)

grid(nx = 10, ny = 15, col = "gray", lty = 2, lwd = 1)

legend("topright", legend = c("Reference", "Prediction"),
       col = c("orange", "blue"), pch = 16)