# DATA 824 Final Project - Fall 2021
This repository contains all applicable files required for the final project, which was to create an R Shiny app with the intent being to visualize data using techniques and principles from the course, *DATA 824 - Data Visualization and Acquisition*. 
## Background
The data used for this project utilized Alzheimer's and MRI data downloaded from [Kaggle](https://www.kaggle.com/jboysen/mri-and-alzheimers). The data is made publicly available as part of the [Open Access Series of Imaging Studies (OASIS)](https://www.oasis-brains.org) project. The original data file specifically used was the [*oasis_cross-sectional.csv*](oasis_cross-sectional.csv). For more information about the project, please review the [project presentation](Alzheimer’s%20Prediction%20App.pdf). This presentation will walk through the the entire project from downloading the file from Kaggle to highlighting the R Shiny app, which predicts whether a patient has dementia based on a set of model features. 
## Running the Application
The app was built using R via RStudio and the R package, *Shiny*. 
The files necessary to run the app are:
- [final_data.RDS](final_data.RDS)
- [model_svmrad.RDS](model_svmrad.RDS)
- [Alzheimer Prediction App - Shiny - Acorda.R](Alzheimer%20Prediction%20App%20-%20Shiny%20-%20Acorda.R)
