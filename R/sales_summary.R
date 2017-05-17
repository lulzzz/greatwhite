# Summarize
# This script contains all functions for producing a variant-level
# monthly sales summary (3-month rolling average sales)

#' Sales Summary
#'
#' Parent function for generating summary data frames
salesSummary <- function(summary_type = c("variant three month")){

  # Load sales data
  sales <- loadNew(keyword = "export.csv")

  sales <- sales %>%
    # Clean up variable names
    select(order_number = order.number,
           issue_date = issue.date,
           sku = variant.sku,
           item = item.name,
           item_quantity = item.quantity,
           price = item.discounted.price,
           currency = currency,
           customer = company.name,
           ship_zip = shipping.address.zip.code,
           ship_country = shipping.address.country) %>%
    # Set issue_date to date class
    mutate(issue_date = parse_date_time(issue_date, "%y-%m-%d")) %>%
    # Remove lines with a blank SKU value
    filter(sku != "") %>%
    # Remove lines for over 24 units
    filter(item_quantity < 24)

  if(summary_type == "variant three month"){
    # Add month and week columns and
    # limit range to variant sales over past three months
    variant_three_month <- sales %>%
      # Add issue_month and issue_week for grouping
      mutate(issue_month = floor_date(issue_date, unit = "month"),
             issue_week = floor_date(issue_date, unit = "week")) %>%
      # Take only last full 3 months of sales
      filter(issue_month >= max(issue_month) - months(3)) %>%
      # Group by SKU and month to calculate total monthly sales
      group_by(sku, issue_month) %>%
      summarise(item_quantity = sum(item_quantity)) %>%
      # Group by SKU only to calculate monthly averages
      group_by(sku) %>%
      summarise(avg_vol3mo = ceiling(mean(item_quantity)),
                med_vol3mo = median(item_quantity),
                max_vol3mo = max(item_quantity),
                min_vol3mo = min(item_quantity))

    return(variant_three_month)
  }

}










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

# COMMENTED OUT

# SALES SUMMARY IS BEING REWRITTEN

#' Sales Summary
#'
#' Convenience function for summarizing sales data over different time
#' intervals.
# salesSummary <- function(sales,
#                          unit = c("day","week","month","quarter","year")) {
#   sales_sum <- sales %>%
#     group_by_(.dots = unit) %>%
#     summarize(order_volume = length(unique(order_number)),
#               unit_volume = sum(item_quantity, na.rm = T),
#               revenue = sum(item_quantity * price, na.rm = T)
#               )
#   return(sales_sum)
# }

# THIS ONE IS FUCKING SHIT UP TOO FOR SOME REASON

#' Sales Series
#' Daily, weekly, monthly, quarterly, annual, sales
#'
#' Takes normalized Itemized Sales data and returns a list of the same data
#' where each item in the list aggregates sales at a different level
#'
#' issue_date serves as the "date" of the order
# salesSeries <- function(sales) {
#
#   sales_series <- list()
#
#   for(unit in c("day","week","month","quarter","year")) {
#     sales_series[[unit]] <- salesSummary(sales, unit)
#   }
#
#   return(sales_series)
#
# }
