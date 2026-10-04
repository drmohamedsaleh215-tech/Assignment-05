# Stochastic RSI (StochRSI) function
stoch_rsi <- function(data, period, k_period, d_period) {
  # Ensure rsi() and sma() functions are available in the environment
  
  # Calculate the RSI
  rsi_values <- rsi(data, period)
  
  # Calculate the StochRSI
  # Using na.rm=TRUE to handle NA values correctly created by the RSI function's leading NAs
  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  
  # Calculate the %K line (StochRSI)
  # Extracting non-NA values to calculate SMA and appending NAs to maintain array length
  valid_k_values <- k_values[!is.na(k_values)]
  k_line_valid <- sma(valid_k_values, k_period)
  
  k_line <- rep(NA, length(data))
  start_idx_k <- length(data) - length(k_line_valid) + 1
  if(start_idx_k > 0) k_line[start_idx_k:length(data)] <- k_line_valid
  
  # Calculate the %D line (3-day simple moving average of %K)
  valid_d_values <- k_line[!is.na(k_line)]
  d_line_valid <- sma(valid_d_values, d_period)
  
  d_line <- rep(NA, length(data))
  start_idx_d <- length(data) - length(d_line_valid) + 1
  if(start_idx_d > 0) d_line[start_idx_d:length(data)] <- d_line_valid
  
  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}