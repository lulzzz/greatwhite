# Load
# This file contains all functions for loading datasets

#' Identify newest file added to a folder
findNew <- function(path) {
  return(
    list.files(path = path, full.names = T) %>%
      file.info() %>%
      tibble::rownames_to_column(var = "fname") %>%
      dplyr::filter(mtime == max(mtime)) %>%
      dplyr::select(fname) %>%
      as.character()
  )
}


#' Load newest file added to a folder
#' This allows the user to simply drop a file into a folder
#' and have it loaded without specifying a new path
loadRaw <- function(path) {
  return(
    read.csv(findNew(path = path), stringsAsFactors = F)
    )
}

#' Load Product Line and Variant Data
#'
#' Loads from PRODUCT LINE MASTER (in Culk/Operations as of 02/23/17)
loadLine <- function(path = "~/Dropbox/Culk/Operations/Product line/PRODUCT LINE MASTER.xlsx",
                     sheet = "VARIANTS") {
  return(read_excel(path = path, sheet = sheet))
}


