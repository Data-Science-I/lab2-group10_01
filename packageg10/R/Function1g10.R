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

