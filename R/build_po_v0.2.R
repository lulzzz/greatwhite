# Build PO
# This file contains functions for building a PO

# Current Inventory is loaded from the most recent
# Stock Levels Report in ~/Downloads

# Target stock is based on 3-month rolling average sell through
# on all orders under $5000 (excluding Sports Basement)

# Order History is loaded from the most recent Sales Orders
# Report in ~/Downloads


# Check Inv takes no arguments and produces an inventory report
checkInv <- function(){

  # Load inventory
  stock_levels <- loadNew(keyword = "Stock Levels Report")
  stock_levels <- stock_levels %>%
    rename(on_hand = stock.on.hand,
           committed = committed.stock,
           uncommitted = uncommitted.stock,
           incoming = incoming.stock)  %>%
    mutate(available = on_hand - committed,
           available_incoming = available + incoming)

  # Compute variant monthly sales metrics from past three months
  variant_three_month <- salesSummary(summary_type = "variant three month")

  # Merge variant monthly sales metrics with inventory
  # Fill in with zeros for non-existent sales metrics
  stock_levels <- merge(stock_levels, variant_three_month, by = "sku", all.x = T) %>%
    mutate_each(funs(replace(., is.na(.), 0)), avg_vol3mo:min_vol3mo)

  # Load product line graphic names
  product_line <- loadLine() %>% select(sku, size_id, style_num, graphic_name)
  # Merge graphic names to stock_levels
  stock_levels <- merge(stock_levels, product_line, by = "sku")

  # Calculate available_invoming / avg_qty
  stock_levels <- stock_levels %>% mutate(available_pct = available / avg_vol3mo,
                                          incoming_pct = incoming / avg_vol3mo,
                                          available_incoming_pct = available_incoming / avg_vol3mo)


  # TEMPORARY STOPPING POINT

  # Sort by style_num and size_id
  size_order <- c("AXS","ASM","AMD","ALG","AXL","A2L","A3L","T02","T04","T06","YSM","YMD","YLG","Y2L","B00","B06","B12","B18","ONE")
  stock_levels <- stock_levels %>%
    mutate(size_id = factor(size_id),
           size_id = factor(size_id, levels = size_order)) %>%
    arrange(graphic_name, style_num, size_id)

  # Trim excess columns from stock level report
  stock_levels <- stock_levels %>%
    select(graphic_name, variant, sku, style_num, size_id,
           on_hand, available, available_incoming,
           avg_vol3mo, max_vol3mo,
           available_incoming_pct)

  dir <- paste0("~/Desktop/stock_levels_",format(now(), format = c("%m%d%y_%H%M%S")),".csv")
  write.csv(stock_levels, dir, row.names = F)
}


# MESSING AROUND WITH VISUALIZATIONS
# data <- stock_levels %>% filter(style_num == "M0018") %>%
#   mutate(size_id = factor(size_id), levels = c("ASM", "AMD", "ALG", "AXL", "A2L")) %>%
#   melt(id = c("sku", "size_id", "product"), measure = c("available_pct","incoming_pct",
#                                                         "available","incoming",
#                                                         "avg_qty"))
# ggplot(data) + geom_col(aes(size_id, ai_pct),
#                         width = 0.98) +
#   coord_cartesian(ylim = c(0,2))



