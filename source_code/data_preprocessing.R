# ------------------------------------------------------------------------------
# This script reads experimental data with columns: 
# T (temperature), t (time), and alpha (solid-state fraction).
# It calculates beta (dT/dt), dbeta/dt, dalpha/dt, and sign(beta).
# The data is then split into subsets based on beta's sign and saved as CSV files.
# Input file must be an Excel file with the required columns.
# ------------------------------------------------------------------------------

library(dplyr)
library(readxl)

process_excel_file <- function(file_name) {
  # Read Excel file
  data <- read_excel(file_name)
  
  # Check necessary columns
  if (!all(c("T", "t", "alpha") %in% colnames(data))) {
    stop("File must contain columns: T, t, and alpha")
  }
  
  # Calculate beta, dbeta/dt, dalpha/dt, and sign_beta
  data <- data %>%
    arrange(t) %>%
    mutate(
      beta = (lead(T) - T) / (lead(t) - t),
      dbeta_dt = (lead(beta) - beta) / (lead(t) - t),
      dalpha_dt = (lead(alpha) - alpha) / (lead(t) - t),
      sign_beta = sign(beta)
    ) %>%
    filter(!is.na(beta) & !is.na(dbeta_dt) & !is.na(dalpha_dt))
  
  # Split data by beta sign
  positive_beta <- data %>% filter(beta > 0)
  negative_beta <- data %>% filter(beta < 0)
  non_zero_beta <- data %>% filter(beta != 0)
  
  # Save CSV files
  write.csv(positive_beta, "positive_beta.csv", row.names = FALSE)
  write.csv(negative_beta, "negative_beta.csv", row.names = FALSE)
  write.csv(non_zero_beta, "non_zero_beta.csv", row.names = FALSE)
  
  cat("Done! Files saved: positive_beta.csv, negative_beta.csv, non_zero_beta.csv\n")
}
