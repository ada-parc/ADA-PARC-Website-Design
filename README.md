# ADA-PARC Website

This repository contains the code for the ADA-PARC website and research data dashboard. The website uses MongoDB as the back-end database for storing geospatial and tabular data and is programmed in R. The website code is hosted and run using (ShinyApps)[https://www.shinyapps.io].

In order to run the website locally (by sourcing the `ADA_PARC.Rmd` file in R Studio) you must have a `config.yml` file with the credentials to access the MongoDB connection.

## Tools

The data explored in this website is sourced primarily from the U.S. Census. The `tools` folder contains an R markdown script (`census_download.Rmd`) that can be used to download American Community Survey data at the state and census tract levels. This folder also contains scripts written to manage data on the database. The `example_state_vars_upload.csv` and `example_tracts.xlsx` files can be used with the `census_download.Rmd` tool to recreate the city-level data.

## Scripts

The scripts folder contains two R scripts: `functions.R` contains the functions used throughout the project, and `national_import.R` is a script that generates national tables from the raw data stored on the database.

## Dictionaries

The dictionaries folder holds `dict_vars.csv`, which is used to translate the national tables across the various functions used within the project. It also provides all of the front-end text describing, associating, and displaying the variables on maps, charts, and tables.
