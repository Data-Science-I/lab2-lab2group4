# load packages
library(devtools)
library(roxygen2)
library(tidyverse)
library(here)

# read in csv file
df <- read_csv(here("R/DRG_data.csv"))

# make substring of the DRG codes
df <- df %>% mutate(DRG = substr(`DRG Definition`, 1, 3))

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
boxplot_DRG <- function(col_name) {
  if (col_name == "covered_charges") {
    # make ggplot for covered_charges
    ggplot(df, aes(x = DRG,
                   y = `Average Covered Charges`)) +
      geom_boxplot() + # boxplot
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) # makes the titles vertical
  } else if (col_name == "total_payments") {
    # make ggplot for total payments
    ggplot(df, aes(x = DRG,
                   y = `Average Total Payments`)) +
      geom_boxplot() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  # makes the titles vertical
  } else if (col_name == "medicare_payments") {
    # make ggplot for medicare payments
    ggplot(df, aes(x = DRG,
                   y = `Average Medicare Payments`)) +
      geom_boxplot() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  # makes the titles vertical

  } else {
    stop("Invalid column name. Use 'covered_charges', 'total_payments', or 'medicare_payments'.")
  }
}
