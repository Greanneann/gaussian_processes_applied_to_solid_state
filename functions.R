# Calculate Q2 metric for model validation
Q2 <- function(y, yhat) {
  1 - mean((y - yhat)^2) / var(y)
}

# Plot with a grid overlay
plot_with_grid <- function(x, y, xlab, ylab, main, col = "black", cex = 0.7, grid_nx = 20, grid_ny = 20) {
  plot(x, y, xlab = xlab, ylab = ylab, main = main, pch = 16, col = col, cex = cex)
  grid(nx = grid_nx, ny = grid_ny, col = "gray", lty = 2, lwd = 1)
}

# Create twin data and sort by alpha
create_twin <- function(data, div) {
  twin_result <- twin(data = data, r = div, format_data = TRUE, leaf_size = 8)
  twin <- data[twin_result, ]
  twin_sorted <- twin[order(twin$alpha), ]
  return(list(input = twin_sorted[, c("alpha", "beta", "dbeta_dt")], 
              output = twin_sorted[, c("dalpha_dt")]))
}

# Compute dalpha/dt using Gilles model
calculate_dalpha_dt_Gilles <- function(alpha, beta, 
                                       n1 = 1.26, n2 = 0.68, n4 = 1.04, 
                                       k1 = 0.42, k2 = 0.08, 
                                       alpha_max = 0.75, alpha_min = 0.38, 
                                       m1 = 0.54, m2 = 1.04) {
  
  dalpha.dt_Gilles <- -1 * abs(beta)^n4 * (
    k1 * (pmax(alpha / alpha_max, 0.00001)^m1) * (pmax((alpha_max - alpha) / alpha_max, 0.00001)^n1) +
      k2 * (pmax((alpha - alpha_min) / (1 - alpha_min), 0)^m2) * (pmax((1 - alpha) / (1 - alpha_min), 0.00001)^n2)
  ) * sign(beta)
  return(dalpha.dt_Gilles)
}

# Function to plot projections
plot_projections <- function(input) {
  par(mfrow = c(3, 1))
  
  # XY projection
  plot(input$alpha, input$beta, 
       xlab = "alpha", ylab = "beta",   
       pch = 16, col = "black", cex = 0.7)  
  grid(nx = 20, ny = 20, col = "gray", lty = 2, lwd = 1)
  
  # XZ projection
  plot(input$alpha, input$dbeta_dt, 
       xlab = "alpha", ylab = "dbeta/dt",  
       pch = 16, col = "black", cex = 0.7)  
  grid(nx = 20, ny = 20, col = "gray", lty = 2, lwd = 1)
  
  # YZ projection
  plot(input$beta, input$dbeta_dt, 
       xlab = "beta", ylab = "dbeta/dt",  
       pch = 16, col = "black", cex = 0.7)  
  grid(nx = 20, ny = 20, col = "gray", lty = 2, lwd = 1)
  
  # Reset layout to default
  par(mfrow = c(1, 1))
}

# Create twin data and sort by alpha
create_twin_LVGP <- function(data, div) {
  twin_result <- twin(data = data, r = div, format_data = TRUE, leaf_size = 8)
  twin <- data[twin_result, ]
  twin_sorted <- twin[order(twin$alpha), ]
  return(list(input = twin_sorted[, c("alpha", "beta", "dbeta_dt", "sign_beta")], 
              output = twin_sorted[, c("dalpha_dt")]))
}