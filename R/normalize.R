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
normSales <- function(sales, platform = "TradeGecko") {

  # Everything is easier in lowercase
  names(sales) %<>% tolower

  # Rename variable names according to platform type
  # Only on TradeGecko for now...
  if(platform == "TradeGecko"){
    sales <- sales %>%
      select(sku = variant.sku,
             sales_qty = quantity)
  }

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

