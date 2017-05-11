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


#' Normalize Variant Sales
#'
#' Output: skus and sales quantities
normVariantSales <- function(sales, platform = c("tradegecko", "stitch labs")){
  names(sales) <- tolower(names(sales))
  sales <- sales %>%
    select(sku = variant.sku,
           sales_qty = quantity)
  return(sales)
}

#' Normalize Itemized Sales datasets
#'
#' This function is designed to normalize itemized Sales datasets exported
#' from TradeGecko or Stitch labs
normSales <- function(sales, platform = c("tradegecko","stitch labs")) {

  names(sales) %<>% tolower # Everything is easier in lowercase

  if(platform == "tradegecko") {
    sales <- sales %>%
      select(order_number = order.number,
             issue_date = issue.date,
             sku = variant.sku,
             item = item.name,
             item_quantity = item.quantity,
             price = item.discounted.price,
             currency = currency,
             customer = company.name,
             ship_zip = shipping.address.zip.code,
             ship_country = shipping.address.country
             ) %>%
      mutate(issue_date = parse_date_time(issue_date, "%y-%m-%d"))
  }

  if(platform == "stitch labs") {

    sales <- sales %>%
      select(order_number = ordernumber,
             issue_date = datecreated,
             sku = lineitemsku,
             item = lineitemname,
             item_quantity = lineitemquantity,
             price = lineitemprice,
             currency = currency,
             customer = contactname,
             ship_zip = shippingzip,
             ship_country = shippingcountry
             ) %>%
      filter(!is.na(item_quantity))

      # Extend values that are only assigned to first row of each order
      # to all rows of their respective orders
      order_first_line <- sales %>%
        select(order_number, issue_date, currency, customer, ship_zip, ship_country) %>%
        filter(issue_date != "")
      sales <- sales %>% select(order_number, sku, item, item_quantity, price)
      sales <- sales %>%
        # merge dates to all line items
        merge(order_first_line, by = "order_number") %>%
        mutate(issue_date = as.Date(parse_date_time(issue_date,
                                                   "%y-%m/%d %H:%M:%S")))

  }



  # Final re-ordering and cleanup for itemized data sets
  sales <- sales %>%
    # re-order variables from output of whichever of the above if statements
    # so that this shit is fucking NORMALIZED for real
    select(order_number, issue_date, sku, item,
           item_quantity, price, price,
           currency, customer, ship_zip, ship_country) %>%
    # filter out empty item values (these are for things like tax, shipping, etc)
    filter(item != "") %>%
    # set classes so that this shit is HELLA FUCKING normal
    mutate(order_number = as.character(order_number),
           issue_date = as.Date(issue_date),
           sku = as.character(sku),
           item = as.character(item),
           item_quantity = as.numeric(item_quantity),
           price = as.numeric(price),
           currency = as.character(currency),
           customer = as.character(customer),
           ship_zip = as.character(ship_zip),
           ship_country = as.character(ship_country)
    )

  return(sales)
}




#' Sort by style number and size
#'
#' Convenience function for sorting dataset on style number and size
#' Contains size labels unique to Culk Ink
ordSize <- function(inv) {
  size_order <- c("AXS","ASM","AMD","ALG","AXL","A2L","T02","T04","T06","YSM","YMD","YLG","Y2L","ONE")
  inv <- inv %>%
    group_by(product) %>%
    mutate(size_id = factor(size_id, levels = size_order)) %>%
    arrange(style_num, size_id) %>%
    ungroup()
  return(inv)
}

