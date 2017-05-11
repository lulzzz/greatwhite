# Helpers
# This file contains helper functions for use anywhere else
# within the package

#' Load latest download of Stock Levels Report from Downloads folder
loadStockLevels <- function() {

  path <- "~/Downloads"
  keyword <- "Stock Levels Report"

  # Find new file
  new_file <- list.files(path = path, full.names = T) %>%
    file.info() %>%
    tibble::rownames_to_column(var = "fname") %>%
    dplyr::filter(grepl(keyword, fname)) %>%
    dplyr::filter(mtime == max(mtime)) %>%
    dplyr::select(fname) %>%
    as.character()

  message('Loaded: ', new_file)

  stock_levels <- read.csv(new_file, stringsAsFactors = F)

  return(stock_levels)
}


#' Load Product Line and Variant Data
loadLine <- function() {
  return(
    read_excel(path = "Product Line/PRODUCT LINE MASTER.xlsx",
               sheet = "variants")
    )
}
