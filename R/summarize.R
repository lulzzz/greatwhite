# Summarize
# This script contains all functions for aggregating and summarizing
# retail data.

#' Date floors
#'
#' Convenience function for creating floor date variables at
#' weekly, monthly, quarterly, and annual levels
dateFloors <- function(df, date_column = "issue_date") {
  df <- df %>%
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
    summarize(order_volume = length(unique(order_number)),
              unit_volume = sum(item_quantity),
              revenue_potential = sum(item_quantity * price_regular),
              revenue_realized = sum(item_quantity * price_discount)
              )
  return(sales_sum)
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

  for(unit in c("day","week","month","quarter","year")) {
    sales_series[[unit]] <- salesSummary(sales, unit)
  }

  return(sales_series)

}
