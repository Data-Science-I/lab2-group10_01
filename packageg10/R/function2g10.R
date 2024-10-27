calculate_drg_statistics <- function(data, stat_type = c("mean", "median", "sd")) {

  # Check that the stat_type is valid
  stat_type <- match.arg(stat_type)

  # Extract the column for Average Medicare Payments
  payment_data <- data$Average.Medicare.Payments

  # Calculate the chosen statistic
  result <- switch(stat_type,
                   "mean" = mean(payment_data, na.rm = TRUE),
                   "median" = median(payment_data, na.rm = TRUE),
                   "sd" = sd(payment_data, na.rm = TRUE))

  return(result)
}
