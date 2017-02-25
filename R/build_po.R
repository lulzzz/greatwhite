# Build PO
# This file contains all functions related to building a PO

#' Generate detailed csv of inventory and products to order
poDetail <- function(inv, artwork) {

  # Figure out what products need restocking for this artwork...
  products_to_print <- inv %>%
    dplyr::filter(artwork_file_1 %in% artwork & to_order > 0) %>%
    dplyr::select(product) %>%
    dplyr::distinct() %>%
    .$product

  # ...so we can filter on all of those products and generate a PO
  po_detail <- inv %>%
    dplyr::filter(product %in% products_to_print) %>%
    select(sku, product, size, blank_id, on_hand, committed, available_incoming, to_order) %>%
    # set negative to_order values to zero since we can't order negative quantities
    mutate(to_order = ifelse(to_order < 0, 0, to_order)) %>%
    mutate(to_order = ceiling(to_order / 6) * 6)

  # Add summary row to top of po_detail so we know if it meets our printer's minimums
  row_total <- po_detail %>%
    filter(to_order > 0) %>% # remove variants that won't be ordered
    mutate(dummy = 1) %>% group_by(dummy) %>% # dummy variable for grouping
    summarise(
      sku = "TOTAL",
      product = "",
      size = "",
      blank_id = "",
      on_hand = sum(on_hand),
      committed = sum(committed),
      available_incoming = sum(available_incoming),
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

  graphics <- inv %>% select(artwork_file_1) %>%
    distinct() %>% .$artwork_file_1

  dir <- paste0("output/",format(now(), format = "%d%m%y"),"_PO")
  dir.create(dir)

  for(graphic_i in graphics) {
    graphic_name <- strsplit(graphic_i, "\\.")[[1]][1]
    po_detail <- poDetail(inv, artwork = graphic_i)
    write.csv(po_detail, paste0(dir,"/po_detail_",graphic_name,".csv"), row.names = F)
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
