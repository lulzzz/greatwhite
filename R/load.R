# Load
# This file contains all functions for loading datasets

#' Identify newest file added to data/import
findNew <- function(path = "data/import") {
  return(
    list.files(path = path, full.names = T) %>%
      file.info() %>%
      tibble::rownames_to_column(var = "fname") %>%
      dplyr::filter(mtime == max(mtime)) %>%
      dplyr::select(fname) %>%
      as.character()
  )
}

#' Load raw Inventory Details export
loadRaw <- function(file = findNew()) {
  return(
    read.csv(file, stringsAsFactors = F)
    )
}

#' Load Product Line and Variant Data
#'
#' Loads from PRODUCT LINE MASTER (in Culk/Operations as of 02/23/17)
loadLine <- function(file = "~/Dropbox/Culk/Operations/Product line/PRODUCT LINE MASTER.xlsx",
                     sheets = c("PRODUCTS","VARIANTS")) {
  line <- list()
  for(sheet in sheets){
    line[[sheet]] <- read_excel(file, sheet = sheet)
  }
  return(line)
}

