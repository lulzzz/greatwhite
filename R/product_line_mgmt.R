# Product Line Management


# SKU Builder
# Generate SKUs using imported style numbers
buildSKUs <- function(new_styles_dir = "~/Dropbox (Culk)/Culk/Product Line/! sku builder/",
                      new_styles_file = "new_styles.csv",
                      size_run = c("AXS-A2L","YSM-YXL","T02-T06","B00-B18")){

  new_styles_path <- paste0(new_styles_dir, new_styles_file)
  new_styles <- read.csv(new_styles_path, stringsAsFactors = F)
  new_styles <- new_styles$new_styles

  if(size_run == "AXS-A2L"){
    sizes <- c("AXS","ASM","AMD","ALG","AXL","A2L")
    new_skus <- expand.grid(sizes,new_styles)
    new_skus <- new_skus %>%
      select(style_num = Var2, size_id = Var1) %>%
      mutate(sku = paste0(style_num,'-',size_id))
  }

  write.csv(new_skus,
            paste0(new_styles_dir,"new_skus ",
                   format(now(), format = c("%m%d%y_%H%M%S")),
                   ".csv"),
            row.names = F)

}



# Blank Library Extractor
# Extract (and clean) S&S styles
# Currently we extract all styles for the brands that we carry
# If this becomes cumbersome, we'll filter more precisely, such as
# only extracting the styles currently carried
extractSS <- function(){

  # Load S&S export
  # Path is hardcoded for now (cuz we need to get shit done!)
  ss_library_raw <- read_excel(path = "~/Dropbox (Culk)/Culk/Product Line/! libraries/S&S Export April 2017.xlsx",
                               sheet = "Products - DS")

  # Load vector of brands currently carried
  # Brand vector is hardcoded (getting shit done!)
  blank_brands <- c("Alternative", "Bella + Canvas", "Independent Trading Co.", "Liberty Bags", "Next Level", "Rabbit Skins")

  ss_library_clean <- ss_library_raw %>% filter(brandName %in% blank_brands)

  write.csv(ss_library_clean, paste0("~/Dropbox (Culk)/Culk/Product Line/! libraries/S&S Library (for import) ",
                                     format(now(), format = c("%m%d%y_%H%M%S")),
                                     ".csv"),
            row.names = FALSE)

}
