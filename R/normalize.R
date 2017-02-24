# Normalize
# This file contains all of the functions for normalizing data

#' Normalize Inventory Detail columns
normCols <- function(inv) {
  names(inv) %<>% tolower
  inv <- inv %>%
    # Rename variables
    select(product, sku,
           on_hand = stock.on.hand,
           committed = committed.stock,
           available = uncommitted.stock,
           incoming = incoming.stock) %>%
    # New variables
    mutate(available_incoming = available + incoming,  # available_incoming = available + incoming
           style_num = substr(sku, 1, 4)) %>%          # style_num = first 4 digits of sku, "style number")
    # Reorder for ease of use
    select(sku, style_num, product, on_hand, committed, available, incoming, available_incoming)
  return(inv)
}

#' Merge PRODUCT LINE data
#'
#' Keep only the products which were exported from TradeGecko
#' Occasionally, some new ones might only exist in the PRODUCT LINE file
mergeLine <- function(inv, line) {
  inv <- merge(inv, line$PRODUCT, by = "style_num") # Merge PRODUCT data on style number
  inv <- merge(inv, line$VARIANTS, by = "sku")      # Merge VARIANT data on sku
  # Select and rename
  inv <- inv %>% select(
    sku, style_num = style_num.x,                                   # identifiers
    product = product_description_short, size = size_id,            # variant info
    on_hand, committed, available, incoming,                        # stock info
    category, gender, graphic, cut, color,                          # product info
    blank_id = blank_id.y, blank_name, blank_color = blank_color.y, # blank info
    artwork_file_1, p1_size, p1_inkcolor                            # print info
  )
  return(inv)
}
