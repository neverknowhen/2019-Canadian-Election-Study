# 2019-Canadian-Election-Study

This repo contains the data and R code for estimating the supporting rate of Liberal Party during 2019 Canada Election. It was created by Zhixing Hong. The aiming for this is to forming a report that presents the logistic regression model we created to estimate the supporting rate when all qualified Canadian participated. As some data is not allowed to share in public , we documented how to get them below. The section of the repo are: inputs, outputs, and scripts.

Inputs is the folder where the changed data stored. We use two datasets in the project:
- [survey data: download from  http://www.ces-eec.ca]
- [census data: download from http://www.chass.utoronto.ca/]


Outputs is the folder where the modified data, report and other supporting file stored.
- census_cell_data.csv (cleaned data file used for post-stratification)
- cleaned_gss_data.csv (cleaned data file for census)
- cleaned_online_data.csv (cleaned data file for survey)
- Candian Election 2019.rmd

Scripts contains R scripts that takes inputs and produced the outputs. They are:
-gss_cleaning.R
-sample_cleaning.R
