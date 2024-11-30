
# Function to extract the metadata for a given fratures from the loaded data
extract_feature <- function(feature, metadata) {
  metadata %>%
    str_subset(paste0("^", feature)) %>%
    str_split("\t", simplify = TRUE) %>%
    as.vector() %>%
    .[-1] %>%
    str_remove_all("\"")
}
