calculate_drg_statistics <- function(data, stat_type = c("mean", "median", "sd")) {
  # Check that the stat_type is valid
  stat_type <- match.arg(stat_type)

  # Calculate the chosen statistic for each DRG Definition group
  result <- data %>%
    group_by(DRG.Definition) %>%  # Group by the 'DRG Definition' column
    summarize(statistic = switch(stat_type,
                                 "mean" = mean(Average.Medicare.Payments, na.rm = TRUE),
                                 "median" = median(Average.Medicare.Payments, na.rm = TRUE),
                                 "sd" = sd(Average.Medicare.Payments, na.rm = TRUE))) %>%
    rename_with(~ paste(stat_type, "payment", sep = "_"), statistic)  # Rename the column to include the stat_type prefix

  return(result)

}
