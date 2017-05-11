# Load
# This file contains all functions for loading datasets

#' Identify newest file added to a folder
findNew <- function(path) {

  # Find new file
  # Keeping separate from files_loaded to avoid
  # object indexing headaches...
  new_file <- list.files(path = path, full.names = T) %>%
    file.info() %>%
    tibble::rownames_to_column(var = "fname") %>%
    dplyr::filter(mtime == max(mtime)) %>%
    dplyr::select(fname) %>%
    as.character()

  # Global variable to track which files are loaded
  # for diagnostics and posterity.
  if(! "files_loaded" %in% ls(envir=.GlobalEnv)) {
    files_loaded <<- new_file
  } else {
    files_loaded <<- c(files_loaded, new_file)
  }

  return(new_file)
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
loadLine <- function(path,
                     sheet = "variants") {
  return(read_excel(path = path, sheet = sheet))
}


