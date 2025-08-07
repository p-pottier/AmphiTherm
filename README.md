# *AmphiTherm*: A comprehensive database of amphibian thermal tolerance and preference.

This repository contains the data and code to reproduce the results from Pottier P., Oh R.R.Y, Pollo P., Rivera-Villanueva A.N., Yang Y., Varon S., Longo A.V., Burke S., Lin H-Y., Valdebenito J.O., Amano T., Drobniak S.M., Nakagawa, S., and Claunch N. (2025). *AmphiTherm*: a comprehensive database of amphibian thermal tolerance and preference. *Scientific Data**

You can cite this repository and its contents using this DOI: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14978243.svg)](https://doi.org/10.5281/zenodo.14978243)

You can also access a Shiny App to filter the database at: https://p-pottier.shinyapps.io/AmphiTherm-Explorer/

Below is a description of the different folders and their content.

Please feel free to contact Patrice Pottier (patrice.pottier37@gmail.com) if you need assistance navigating these documents.

## **data/**

We provide different datasets to help users 

* `Cleaned_and_curated_data_Amphitherm.csv`: Curated version of the database. **This is the dataset we recommend using for most research questions in ecophysiology.**  

* `Cleaned_data_Amphitherm`: Processed data on lower thermal limits, upper thermal limits, and thermal preference. Please note that caution should be taken when using this version of the dataset, as it contains estimates with procedural concerns (see Table S5), but can still be useful for some specific questions (see manuscript for details).

* `Raw_CTmax_data.csv`: Raw data on upper thermal limits collected in Pottier et al. 2022 (Scientific Data) https://doi.org/10.1038/s41597-022-01704-9. Please note there are some mistakes in this dataset, which were corrected in R (see R/ folder).

* `Raw_Tpref_CTmin_data.csv`: Raw data on cold tolerance and thermal preference collected in this database update. Please note there are some mistakes in this dataset, which were corrected in R (see R/ folder).

* `Metadata_Jetz_Pyron.csv`: Supplementary data from Jetz & Pyron (2018) https://doi.org/10.1038/s41559-018-0515-5 containing taxonomic information.

* `Consensus_tree_Jetz_Pyron_2018.tre`: Consensus tree sampled from the posterior distribution by Jetz & Pyron (2018) https://doi.org/10.1038/s41559-018-0515-5 

* `Metadata_AmphiTherm.xlsx1`: Metadata files listing the description of each column of the AmphiTherm database.

## **R/**

* `data_curation.Rmd`: Code detailing the different steps of the data curation and calculation of descriptive statistics.

* `figures_manuscript.Rmd`: Code for producing the figures in the manuscript.

* `bibliography_processing.Rmd`: Code detailing the steps used to process the bibliographic files from the literature searches. Note that the bibliographic files are not presented in this repository because they are too large to share through Github. Therefore, this file is not reproducible. However, I would be happy to share these files upon request. 


## **references/**

We feel strongly about the importance of acknowledging the original authors of the studies included in this database. Therefore, we ask users to strongly consider citing the original references in addition to the database.

* `AmphiTherm_database.ris`: Bibliographic file containing all the references included in the database (RIS format)
* `AmphiTherm_database.bib`: Bibliographic file containing all the references included in the database (BibTex format)

## **shiny_app/** 

This folder contains the data and code (`app.R`) to build the shiny app. 

