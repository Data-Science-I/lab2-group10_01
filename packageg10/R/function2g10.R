#' Calculate DRG Payment Statistics
#'
#' This function calculates a specified statistic (mean, median, or standard deviation)
#' for the "Average.Medicare.Payments" column in the given data frame.
#'
#' @param data A data frame containing a column named "Average.Medicare.Payments".
#' @param stat_type A character string specifying the statistic to calculate.
#'   Options are "mean" (default), "median", or "sd" (standard deviation).
#'
#' @return A numeric value representing the calculated statistic for the
#'   "Average.Medicare.Payments" column.
#' @export
#'
#' @examples
#' # Example usage:
#' data <- data.frame(Average.Medicare.Payments = c(100, 200, 300, NA))
#' calculate_drg_statistics(data, "mean")   # Returns the mean, ignoring NAs
#' calculate_drg_statistics(data, "median") # Returns the median, ignoring NAs
#' calculate_drg_statistics(data, "sd")     # Returns the standard deviation, ignoring NAs

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
