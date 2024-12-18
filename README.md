# Project Contributors
s165493 (github: RuneDaucke32)
s233028 (github: LuisaWeisch)
s144523 (github: dalofa)

# group_01_project

A R4BDS exam project

This repository contains the code necessary to download, clean and analyze the data published in the paper "Blood transcriptomics of drug-naïve sporadic Parkinson's disease patients" by Calligaris et al 2015, DOI: 10.1186/s12864-015-2058-3.

The data can be downloaded from the NCBI Gene Expression Omnibus under the accession GSE72267 but programmatic retreival of the data is inceluded in the 01_load.qmd script.

To render all quarto documents run the following in the terminal. Alternatively simply render the R/00_all.qmd document to get the analysis collect in one file.

```
for quarto_file in R/*.qmd; do quarto render $quarto_file --output-dir ../results; done

```

# Presentation

A direct link to the presentation can be found here:
https://raw.githack.com/rforbiodatascience24/group_01_project/main/doc/presentation.html
