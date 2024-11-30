
# Function to extract the metadata for a given fratures from the loaded data
extract_feature <- function(feature, metadata) {
  metadata %>%
    str_subset(str_c("^", feature)) %>%
    str_split("\t", simplify = TRUE) %>%
    as.vector() %>%
    .[-1] %>%
    str_remove_all("\"")
}


find_genes <- function(gene_set, all_genes){
  present_genes <- sapply(gene_set, function(gene){
    grep(gene, all_genes, ignore.case = TRUE, value = TRUE)}) %>%
    unlist()
  return(present_genes)
}
