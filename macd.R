macd <- function(data, short_period, long_period, signal_period) {
  # Ensure the ema function is available in the environment before calling this
  
  # Calculate the short-term exponential moving average (EMA)
  short_ema <- ema(data, short_period)
  
  # Calculate the long-term exponential moving average (EMA)
  long_ema <- ema(data, long_period)
  
  # Calculate the MACD line (the difference between short_ema and long_ema)
  macd_line <- short_ema - long_ema
  
  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)
  
  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line
  
  # Return the MACD line, signal line, and histogram as a list
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )
  
  return(result)
}