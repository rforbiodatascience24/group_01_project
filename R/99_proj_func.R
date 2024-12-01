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

