#' Create Boxplot for DRG Payment Data
#'
#' This function generates a boxplot for a specified payment type by DRG code.
#' The plot shows the distribution of the chosen payment type across different DRG codes,
#' allowing for comparison of payment data categories such as "Average Medicare Payments",
#' "Average Total Payments", and "Average Covered Charges".
#'
#' @param data A data frame containing DRG payment data. Must include the columns
#'   "DRG.Definition", "Average.Medicare.Payments", "Average.Total.Payments",
#'   and "Average.Covered.Charges".
#' @param payment_type A character string specifying the type of payment data to plot.
#'   Acceptable values are "Average.Medicare.Payments", "Average.Total.Payments",
#'   or "Average.Covered.Charges".
#'
#' @return A boxplot showing the distribution of the specified payment type by DRG code.
#' @export
#'
#' @examples
#' # Example usage:
#' data <- data.frame(
#'   DRG.Definition = c("001 - Heart transplant", "002 - Lung transplant", "003 - Liver transplant"),
#'   Average.Medicare.Payments = c(50000, 30000, 40000),
#'   Average.Total.Payments = c(60000, 35000, 45000),
#'   Average.Covered.Charges = c(80000, 50000, 70000)
#' )
#' create_boxplot(data, "Average.Medicare.Payments")  # Creates a boxplot for Medicare payments
#' create_boxplot(data, "Average.Total.Payments")     # Creates a boxplot for total payments

create_boxplot <- function(data, payment_type) {

  if (payment_type == "Average.Medicare.Payments") {
    column <- "Average.Medicare.Payments"
  } else if (payment_type == "Average.Total.Payments") {
    column <- "Average.Total.Payments"
  } else if (payment_type == "Average.Covered.Charges") {
    column <- "Average.Covered.Charges"
  } else {
    stop("Invalid payment type. Choose from 'Average.Medicare.Payments', 'Average.Total.Payments', or 'Average.Covered.Charges'.")
  }

  data$DRG.Code <- substr(data$DRG.Definition, 1, 3)
  boxplot_formula <- as.formula(paste(column, "~ DRG.Code", sep = " "))


  boxplot(boxplot_formula, data = data,
          main = paste(payment_type, "by DRG Code"),
          xlab = "DRG Codes", ylab = payment_type, las = 2,
          col = "lightblue", outline = FALSE,
          cex.axis = 0.7,
          cex.lab = 1,
          frame.plot = FALSE)
}

