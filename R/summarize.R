# Summarize
# This script contains all functions for aggregating and summarizing
# retail data.

#' Date floors
#'
#' Convenience function for creating floor date variables at
#' weekly, monthly, quarterly, and annual levels
salesSeries <- function(sales, date_column) {
  sales <- sales %>%
    select(date = matches(date_column), everything()) %>%
    mutate(day = floor_date(date, unit = "day"),
           week = floor_date(date, unit = "week"),
           month = floor_date(date, unit = "month"),
           quarter = floor_date(date, unit = "quarter"),
           year = floor_date(date, unit = "year")
    )
}

#' Sales Summary
#'
#' Convenience function for summarizing sales data over different time
#' intervals.
salesSummary <- function(sales,
                         unit = c("day","week","month","quarter","year")) {
  sales_sum <- sales %>%
    group_by_(.dots = unit) %>%
    summarize(orders = length(unique(order_number))
              )
}

#' Sales Series
#' Daily, weekly, monthly, quarterly, annual, sales
#'
#' Takes normalized Itemized Sales data and returns a list of the same data
#' where each item in the list aggregates sales at a different level
#'
#' issue_date serves as the "date" of the order
salesSeries <- function(sales) {
  sales_series <- list()
  summary_vars <- c("")
  sales_series[day] <- sales

}
