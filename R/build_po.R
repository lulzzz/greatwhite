# Build PO
# This file contains all functions related to building a PO

#' Target stock
#'
#' Calculate stock targets based on relative sales numbers
#' Base unit = 3 by default, meaning the smallest positive
#' stock target will be 3. However, every stock target
#' greater than 3 will be a multiple of 6
#'
#' Relative sales numbers
#' Products are assigned to one of three sales tiers: 1, 2, 3
#' 1 = 1-20th percentile sales across all products
#' 2 = 21-80th ""
#' 3 = 81-99th ""
#'
#' Stock targets are assigned based on product category and sales tier:
#'
#' Product Category:
#' Kids = 1 (uniform)
#' Adult = XS-2 : S-3 : M-3 : L-2 : XL-1 : XXL-0
#' Accessory = 1 (uniform, one size)
#'
#' Sales tier:
#' 1 = base unit 3
#' 2 = base unit 6
#' 3 = base unit 12
targetStock <- function(inv, sales_tier_base, size_ratios) {

  # Set missing sales values to 0
  # (so that we don't have to repeatedly use na.rm = T)
  inv <- inv %>% mutate(sales_qty = ifelse(is.na(sales_qty), 0, sales_qty))

  # Calculate total product sales
  product_sales <- inv %>%
    select(style_num, sales_qty) %>%
    group_by(style_num) %>%
    summarize(sales_qty_product = sum(sales_qty))

  sales_tier_thresholds <- quantile(product_sales$sales_qty_product,
                                    probs = c(0, 0.5, 0.8, 1))

  product_sales <- product_sales %>%
    mutate(sales_tier = cut(sales_qty_product,
                            breaks=sales_tier_thresholds,
                            include.lowest=T, labels=F))
  rm(sales_tier_thresholds)

  inv <- merge(inv, product_sales, by="style_num")
  rm(product_sales)

  # Merge sales tier base values and inventory data set
  # This is simply for the purpose of breaking up our calculations
  # into trivial steps
  inv <- merge(sales_tier_base, inv, by = "sales_tier", sort = FALSE)

  # Merge size ratios and inventory data set
  # We merge on size_id and gender because the size ratios vary depending
  # on the type of product, which we are currently denoting by gender
  inv <- merge(size_ratio, inv, by = c("gender","size_id"), all.y = T)

  inv <- inv %>%
    mutate(
      target_stock = size_ratio * sales_tier_base
    )

  return(inv)
}

#' Order quantities
#'
#' Calculate units to be ordered based on stock targets
toOrder <- function(inv) {
  return(
    inv %>%
      mutate(
        # What stock will we have *after* current POs are received?
        available_incoming = available + incoming,

        # How much will we need to order to meet those levels?
        to_order = target_stock - available_incoming,

        # set negative to_order values to zero since
        # we can't order negative quantities
        to_order = ifelse(to_order < 0, 0, to_order),

        # Set to_order values between 0 and 3 to 3
        to_order = ifelse(to_order %in% 1:3, 3, to_order),

        # Round all to_order values greater than 3 up
        # to the nearest multiple of 6
        to_order = ifelse(to_order > 0, ceiling(to_order / 6) * 6, to_order)
        )
  )
}

#' Generate detailed csv of inventory and products to order
poDetail <- function(inv, artwork_i) {

  # Figure out which products have this artwork...
  products_to_print <- inv %>%
    dplyr::filter(artwork_file_1 %in% artwork_i) %>%
    # dplyr::filter(to_order > 0) %>%
    # Commented out because, for now, we want to see all products,
    # even those which do not need to be re-ordered
    dplyr::select(product) %>%
    dplyr::distinct() %>%
    .$product

  # ...so we can filter on all of those products and generate a PO
  po_detail <- inv %>%
    dplyr::filter(product %in% products_to_print) %>%
    select(sku, product, gender, size_id, size_ratio, sales_tier_base,
           blank_id, sales_qty, sales_qty_product,
           on_hand, committed, available_incoming, target_stock, to_order)

  # Add summary row to top of po_detail so we know if it meets our printer's minimums
  row_total <- po_detail %>%
    filter(to_order > 0) %>% # remove variants that won't be ordered
    mutate(dummy = 1) %>% group_by(dummy) %>% # dummy variable for grouping
    summarise(
      sku = "TOTAL",
      product = "",
      gender = "",
      size_id = "",
      size_ratio = "",
      sales_tier_base = "",
      blank_id = "",
      sales_qty = "",
      sales_qty_product = "",
      on_hand = sum(on_hand),
      committed = sum(committed),
      available_incoming = sum(available_incoming),
      target_stock = "",
      to_order = sum(to_order)
    ) %>%
    select(-dummy)

  po_detail <- rbind(row_total, po_detail)

  return(po_detail)
}

#' Generate detailed PO for all products, grouped by graphic
#'
#' Save to new folder: output/DDMMYY_PO/po_detail_graphic_name.csv, ...
poExport <- function(inv) {

  artwork <- inv %>% select(artwork_file_1) %>%
    distinct() %>% .$artwork_file_1

  dir <- paste0("output/",format(now(), format = c("%m%d%y_%H%M%S")),"_PO")
  dir.create(dir)

  for(artwork_i in artwork) {
    artwork_name <- strsplit(artwork_i, "\\.")[[1]][1]
    po_detail <- poDetail(inv, artwork = artwork_i)
    write.csv(po_detail, paste0(dir, "/po_detail_", artwork_name, ".csv"), row.names = F)
  }

}



#' Generate detail graphs of inventory and products to order
poVisual <- function(po_detail) {

}


#' Generate sku:qty csv for importing PO to TradeGecko
poImport <- function(po_detail) {
  return(
    po_detail %>% select(sku, quantity = to_order)
  )
}
