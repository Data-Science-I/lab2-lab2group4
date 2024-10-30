#' Title: Create a Boxplot by DRG
#'
#' This function takes one string and returns a boxplot
#'
#' @param col_name A string.Takes in "covered_charges" for Average Covered Charges. Takes in "total_payments" for Average Total Payments. Takes in "medicare_payments" for Average Medicare Payments
#' @return boxplot generated from ggplot2.
#' @examples
#' boxplot_DRG("covered_charges")  # returns boxplot by DRG
#' boxplot_DRG("total_payments")  # returns boxplot by DRG
#' @export

# function where the argument is "covered_charges", "total payments" or "medicare payments"
boxplot_DRG <- function(data,col_name) {
  if (col_name == "covered_charges") {
    # make ggplot for covered_charges
    ggplot(data, aes(x = DRG,
                   y = `Average Covered Charges`)) +
      geom_boxplot() + # boxplot
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) # makes the titles vertical
  } else if (col_name == "total_payments") {
    # make ggplot for total payments
    ggplot(data, aes(x = DRG,
                   y = `Average Total Payments`)) +
      geom_boxplot() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  # makes the titles vertical
  } else if (col_name == "medicare_payments") {
    # make ggplot for medicare payments
    ggplot(data, aes(x = DRG,
                   y = `Average Medicare Payments`)) +
      geom_boxplot() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  # makes the titles vertical

  } else {
    stop("Invalid column name. Use 'covered_charges', 'total_payments', or 'medicare_payments'.")
  }
}

#' Calculate Summary Statistics for Medicare Payments by DRG
#'
#' This function calculates a summary statistic (mean, median, or standard deviation) for average Medicare payments across all DRG codes.
#'
#' @param df A data frame. Expected to contain a column with DRG codes and a column with Medicare payment amounts.
#' @param stat A string. Options are "mean" for the mean Medicare payment, "median" for the median Medicare payment, and "sd" for the standard deviation of Medicare payments.
#' @return A numeric value representing the specified summary statistic for Medicare payments.
#' @examples
#' calculate_drg_stats(drg_data, "mean")  # Returns the mean Medicare payment
#' calculate_drg_stats(drg_data, "median")  # Returns the median Medicare payment
#' calculate_drg_stats(drg_data, "sd")  # Returns the standard deviation of Medicare payments
#' @export

calc_stats_drg <- function(data, stat_type) {
    data$`Average Medicare Payments` <- as.numeric(gsub("[^0-9.]", "", data$`Average Medicare Payments`))# Convert the 'Average Medicare Payments' column to numeric

  if (!stat_type %in% c("mean", "median", "sd")) {
    stop("Invalid statistic type. Choose either 'mean', 'median', or 'sd'.")
  }

  # Select statistic function
  stat_function <- switch(stat_type,
                          "mean" = mean,
                          "median" = median,
                          "sd" = sd)

  # Calculate the chosen statistic for each DRG code
  result <- data %>%
    group_by(`DRG Definition`) %>%  # Group by the 'DRG Definition' column
    summarize(statistic = stat_function(`Average Medicare Payments`, na.rm = TRUE)) %>%
    rename_with(~ paste(stat_type, "payment", sep = "_"), statistic)

  return(result)
}
