# Helpers
# This file contains helper functions for use anywhere else
# within the package

#' Load latest download of Stock Levels Report
#' or Sales Order Export from Downloads folder
loadNew <- function(path = "~/Downloads",
                    keyword = c("Stock Levels Report",
                                "export.csv")) {

  # Find new file
  new_file <- list.files(path = path, full.names = T) %>%
    file.info() %>%
    tibble::rownames_to_column(var = "fname") %>%
    dplyr::filter(grepl(keyword, fname)) %>%
    dplyr::filter(mtime == max(mtime)) %>%
    dplyr::select(fname) %>%
    as.character()

  message('Loaded: ', new_file)

  df <- read.csv(new_file, stringsAsFactors = F)
  names(df) <- tolower(names(df))

  return(df)
}


#' Load Product Line and Variant Data
loadLine <- function() {
  return(
    read_excel(path = "~/Dropbox (Culk)/Culk/Product Line/PRODUCT LINE MASTER.xlsx",
               sheet = "variants")
    )
}


