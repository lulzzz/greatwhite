# Normalize
# This file contains all of the functions for normalizing retail data

#' Normalize Inventory report
normInv <- function(inv, platform = "TradeGecko") {

  # Everything is easier in lowercase
  names(inv) %<>% tolower

  # Rename variable names according to platform type
  # This is the core "normalization" step that brings
  # different platforms into the mix.

  # But for now we are just dealing with the TradeGecko
  # Inventory Details csv export from the "Intelligence" menu
  if(platform == "TradeGecko"){
    inv <- inv %>%
      select(product, sku,
             on_hand = stock.on.hand,
             committed = committed.stock,
             available = uncommitted.stock,
             incoming = incoming.stock)
  }

  # Create new variable to see what will be in
  # stock after active POs are received
  inv <- inv %>% mutate(available_incoming = available + incoming)

  # Reorder variables for ease of use
  inv <- inv %>% select(sku, product, on_hand, committed,
                        available, incoming, available_incoming)

  return(inv)
}


#' Merge on sku
#'
#' Convenience function for merging two datasets on "sku"
mergeSKU <- function(x, y) {
  return(
    merge(x, y, by = "sku", all.x = T)
  )
}


#' Normalize Variant Sales report
#'
#' This function is designed to normalize a variety of raw sales order exports.
#' It checks which platform the sales data came from and what type of report
#' has been exported (i.e. product sales vs itemized orders)
normSales <- function(sales, platform = c("tradegecko","stitch labs"),
                      type = c("product","itemized orders")) {

  names(sales) %<>% tolower # Everything is easier in lowercase

  if(platform == "tradegecko" & type == "product") {
    sales <- sales %>%
      select(sku = variant.sku,
             sales_qty = quantity)
  }

  if(platform == "stitch labs" & type == "product") {
    # placeholder cuz I ain't writing this shit
  }

  if(platform == "tradegecko" & type == "itemized orders") {
    sales <- sales %>%
      select(order_number = order.number,
             issue_date = issue.date,
             ship_date = shipment.date,
             sku = variant.sku,
             item = item.name,
             item_custom = item.label,   # shipping, discounts, etc
             item_quanity = item.quantity,
             price_regular = item.price,
             discount = item.discount.percentage,
             price_discount = item.discounted.price,
             tax = item.tax.rate,
             currency = currency,
             warehouse = warehouse,
             customer = company.name,
             ship_zip = shipping.address.zip.code,
             ship_country = shipping.address.country
             ) %>%
      mutate(issue_date = parse_date_time(issue_date, "%m/%d/%y"),
             ship_date = parse_date_time(ship_date, "%m/%d/%y"))
  }

  if(platform == "stitch labs" & type == "itemized orders") {

  }

  # COMMENTED OUT SINCE WE'RE NOT DEALING WITH MULTIPLE SALES IMPORTS
  # AND THERE ARE ALSO A TON OF ISSUES WITH THIS TEST IN GENERAL

  # Crude unit test to make sure we don't accidentally change the
  # variables that are supposed to come out of this beezy

  # sales_vars_norm = c("order_number","sku","item","item_custom",
  #                     "price_regular","discount","price_discount",
  #                     "tax","issue_date","ship_date","currency",
  #                     "warehouse","customer","ship_zip","ship_country")
  # if(!identical(sales_vars_norm, names(sales))){
  #   stop("Personal error message: Column names have changed but this change
  #        has not been reflected in the rest of the function, or at least
  #        not in the checker for this error message...")
  # }

  return(sales)
}

#' Sort by style number and size
#'
#' Convenience function for sorting dataset on style number and size
#' Contains size labels unique to Culk Ink
ordSize <- function(inv) {
  size_order <- c("XS","S","M","L","XL","XXL","T2","T4","T6","YS","YM","YL","YXL","NS")
  inv <- inv %>%
    group_by(product) %>%
    mutate(size_id = factor(size_id, levels = size_order)) %>%
    arrange(style_num, size_id) %>%
    ungroup()
  return(inv)
}

