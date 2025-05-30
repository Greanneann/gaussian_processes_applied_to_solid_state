source("functions.R")  # Load predefined functions
set.seed(123)          # Fix random seed for reproducibility

library(DiceKriging)   # Gaussian Process modeling
library(twinning)      # Twin data generation

run_analysis_adaptive <- function(filename, num_initial = 15, nIte = 35, div = 40) {
  
  df <- read.csv(filename)  # Load dataset
  df <- df[, c("dalpha_dt", "alpha", "beta", "dbeta_dt")]  # Select columns
  
  # Randomly select initial training subset
  train_index <- sample(nrow(df), num_initial)
  train <- df[train_index, ]
  train_input <- train[, c("alpha", "beta", "dbeta_dt")]
  train_output <- train[, "dalpha_dt"]
  
  # Generate twin data for point selection
  selection_twin <- create_twin(df, div)
  selection_input <- selection_twin$input
  selection_output <- selection_twin$output
  
  # Train initial Kriging metamodel
  metamodel <- km(
    formula = ~1,
    design = train_input,
    response = train_output,
    covtype = "matern5_2",
    nugget.estim = TRUE,
    control = list(trace = FALSE)
  )
  
  # Iteratively add points with highest prediction uncertainty (standard deviation)
  for (i in 1:nIte) {
    # Select the point with the highest predicted standard deviation
    sd_pred <- predict.km(metamodel, selection_input, type = 'UK', checkNames = FALSE)$sd
    maxi <- which.max(sd_pred)
    
    # Add this point to the training set
    train_input <- rbind(train_input, selection_input[maxi, ])
    train_output <- c(train_output, selection_output[maxi])
    
    # Retrain the metamodel with the updated training data
    capture.output(
      metamodel <- km(
        formula = ~1,
        design = train_input,
        response = train_output,
        covtype = "matern5_2",
        nugget.estim = TRUE,
        control = list(trace = FALSE)
      )
    )
  }

  # Generate test twin data for evaluation
  test_twin <- create_twin(df, div)
  test_input <- test_twin$input
  test_output <- test_twin$output
  
  # Predictions on test set
  prediction <- predict(metamodel, test_input, type = 'UK', checkNames = FALSE)$mean
  Q2_TB <- Q2(test_output, prediction)
  
  # Leave-One-Out validation on training set
  prediction_LOO <- leaveOneOut.km(metamodel, type = 'UK')$mean
  Q2_LOO <- Q2(train_output, prediction_LOO)
  
  print(paste('Q2 on test basis :', Q2_TB))
  print(paste('Q2 on test basis using Leave-One-Out predictions:', Q2_LOO))
  
  # Plot predictions on test data
  par(mfrow = c(1, 1))
  alpha <- test_input[, "alpha"]
  plot(
    alpha, prediction, type = "l",
    main = paste("Predictions using different beta and dbeta.dt\nFile:", filename),
    xlab = "alpha", ylab = "Predicted Output",
    col = "orange", lty = 1, lwd = 2,
    ylim = c(-0.0004, 0.0004)
  )
  grid(nx = 10, ny = 15, col = "gray", lty = 2, lwd = 1)
  
  # Fixed parameters for reference curve
  alpha_seq <- seq(0, 1, length.out = 1000)
  beta_fixed <- median(test_input$beta)
  dbeta_dt_fixed <- median(test_input$dbeta_dt)
  
  dalpha_dt_Gilles <- calculate_dalpha_dt_Gilles(alpha_seq, beta_fixed)
  
  new_data <- data.frame(alpha = alpha_seq, beta = beta_fixed, dbeta_dt = dbeta_dt_fixed)
  predictions <- predict(metamodel, new_data, type = "UK", checkNames = FALSE)
  
  plot(
    alpha_seq, predictions$mean, type = "l",
    main = "Adaptive Learning",
    xlab = "alpha", ylab = "dalpha/dt",
    col = "orange", lty = 1, lwd = 2,
    ylim = c(-0.0004, 0.0004)
  )
  grid(nx = 10, ny = 15, col = "gray", lty = 2, lwd = 1)
  lines(alpha_seq, dalpha_dt_Gilles, col = "blue", lty = 2, lwd = 2)
  
  legend("topright", legend = c("Prediction", "Baseline"),
         col = c("orange", "blue"), lty = c(1, 2), lwd = c(2, 2))
  return(metamodel)
}

# parameters: filename, initial number of points in training set, number of iterations
run_analysis_adaptive("positive_beta.csv", 15, 35)
run_analysis_adaptive("negative_beta.csv", 15, 65)
