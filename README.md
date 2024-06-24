# RMT: Statistical Practice: Online survey

This repository contains data and supplementary materials for Chapter 2 of the thesis Robust Statistical Methods in the Credibility Movement of Psychology Science. The associated OSF page can be accessed here: <https://osf.io/t4ugb/>

The repository is organised as follows:

-   `r_docs`

    -   `data_cleaning` contains the script for processing raw data from the survey. Please note that raw data are not provided to protect participants' annonymity, so this file is for information purposes only and the code will not run.

    -   `analysis_imputation` contains the script used for running multiple imputation

    -   `analysis_models` takes the imputed datasets and runs statistical models reported in the paper.

    -   `analysis_smmaries` works with the models to generate summaries of the results

-   `data`

    -   `processed_data`

        -   `stats_practice_processed_data` - is the original data file for all participants processed by `data_cleaning` script. It contains all the demographic information with de-identified university information.

        -   `imputed_data_0` is the original data file from the imputed list returned by the `mice` R package.

        -   `imputed_data_1` - imputed dataset used for fitting the models reported in the paper.

-   `scripts`

    -   `helpers.R` - a collection of small helper functions.
