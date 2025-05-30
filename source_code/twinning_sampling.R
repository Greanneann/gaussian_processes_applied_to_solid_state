source("functions.R")  # Load predefined functions
set.seed(123)          # Fix random seed for reproducibility

library(DiceKriging)   # Gaussian Process modeling
library(twinning)      # Twin data generation

run_analysis_twinning <- function(filename, num_obser) {
  
  df <- read.csv(filename)  # Load dataset
  df <- df[, c("dalpha_dt", "alpha", "beta", "dbeta_dt")]  # Select columns
  
  div <- round(nrow(df) / num_obser)
  
  # Generate twin data
  train_twin <- create_twin(df, div)
  input <- train_twin$input
  output <- train_twin$output
  
  # plot_projections(input)  # Visualize input projections
  
  # Train Gaussian Process metamodel
  metamodel <- km(formula = ~1,  
                  design = input,
                  response = output,
                  covtype = "matern5_2",
                  nugget.estim = TRUE,
                  control = list(trace = FALSE))
  
  # plot the model
  # print(metamodel)
  # par(mar = c(4, 4, 2, 2))
  # plot(metamodel)
  
  # Generate test twin data for evaluation
  test_twin <- create_twin(df, div)
  test_input <- test_twin$input
  test_output <- test_twin$output
  
  # Predict and calculate Q2
  prediction <- predict(metamodel, test_input, 'UK', checkNames = FALSE)$mean
  Q2_TB <- Q2(test_output, prediction)
  
  # Leave-One-Out validation
  prediction_LOO <- leaveOneOut.km(metamodel, 'UK')$mean
  Q2_LOO <- Q2(output, prediction_LOO)
  
  print(paste('Q2 on test basis:', Q2_TB))
  print(paste('Q2 on test basis using Leave-One-Out predictions:', Q2_LOO))
  
  # Plot predictions on test dataset
  par(mfrow = c(1, 1))
  test_alpha <- test_input[, "alpha"]
  plot(test_alpha, prediction, type = "l",
       main = paste("Predictions using different beta and dbeta_dt\nFile:", filename),
       xlab = "alpha", ylab = "Predicted Output",
       col = "orange", lty = 1, lwd = 2,
       ylim = c(-0.0004, 0.0004))
  grid(nx = 10, ny = 15, col = "gray", lty = 2, lwd = 1)
  
  # Fixed parameters
  alpha <- seq(0, 1, length.out = 1000)
  beta_fixed <- median(test_input$beta)
  dbeta_dt_fixed <- median(test_input$dbeta_dt)
  
  # Calculate reference dalpha/dt
  dalpha_dt_Gilles <- calculate_dalpha_dt_Gilles(alpha, beta_fixed)
  
  # Model prediction on fixed parameters
  new_data <- data.frame(alpha = alpha, beta = beta_fixed, dbeta_dt = dbeta_dt_fixed)
  predictions <- predict(metamodel, new_data, type = "UK", checkNames = FALSE)
  
  # Plot model predictions and reference
  plot(
    alpha, predictions$mean, type = "l",
    main = paste("Twin Sampling\nFile:", filename),
    xlab = "alpha", ylab = "dalpha/dt",
    col = "orange", lty = 1, lwd = 2,
    ylim = c(-0.0004, 0.0004)
  )
  grid(nx = 10, ny = 15, col = "gray", lty = 2, lwd = 1)
  lines(alpha, dalpha_dt_Gilles, col = "blue", lty = 2, lwd = 2)
  
  legend("topright", legend = c("Predicted", "Baseline"),
         col = c("orange", "blue"), lty = c(1, 2), lwd = c(2, 2))
  return(metamodel)
}

# parameters: filename and number of observations for model's training
run_analysis_twinning("positive_beta.csv", 275)
run_analysis_twinning("negative_beta.csv", 417)