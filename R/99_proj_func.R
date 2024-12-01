# Functions for 02_clean_qmd
## Function to map probe IDs to gene names
library("annotate")
library("hgu133a2.db")

map_probe_to_gene <- function(gene_probe_ids) {
  gene_names <- gene_probe_ids |>
    sapply(function(probe_id) {
      mapped_genes <- ifelse(is.na(probe_id) || probe_id == "", "Unnamed",
                             ifelse(is.null(getSYMBOL(probe_id, "hgu133a2.db")), probe_id, getSYMBOL(probe_id, "hgu133a2.db")))
      return(mapped_genes)
    })
  ## Ensure no duplicates or missing values in gene names
  return(make.unique(ifelse(is.na(gene_names), "Unnamed", gene_names)))
}

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
