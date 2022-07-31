Comparison and Evaluation of Differentmachine Learning Methods at
Predictingcredit Card Default
================
Sidney Harshman-EarleyDenis O’Riordan
31 July 2022

-   [:blue_book: Overview](#blue_book-overview)
-   [:heavy_exclamation_mark:
    Prerequisites](#heavy_exclamation_mark-prerequisites)
    -   [Files](#files)
    -   [R / R-Studio](#r--r-studio)
    -   [Tensorflow](#tensorflow)
    -   [Database download](#database-download)
-   [:triangular_ruler: Structure](#triangular_ruler-structure)
    -   [main.R](#mainr)
    -   [Feature Engineering](#feature-engineering)
    -   [Machine Learning Models](#machine-learning-models)

# :blue_book: Overview

The aim of this project is to apply a range of machine learning
techniques to predict credit card default using the historical data of
credit card customers. The following report describes the process
undertaken to compare a number of models. Beginning with an industrial
size dataset, cleansing and formatting the data before using it to
train, tune and evaluate the chosen models based on the historical data
of credit card customers.

# :heavy_exclamation_mark: Prerequisites

#### Files

[Code from Repository](project-sharshmanucd) ![alt
text](https://img.shields.io/badge/file%20size-151.6%20Mb-green)

[Datasets from Google
Drive](https://drive.google.com/drive/u/0/folders/1C2TYJRsVH681dylc8ZX5A4gP-LsoaW7o)
![alt text](https://img.shields.io/badge/file%20size-23%20Gb-red)

Each of the software packages below must be installed a a prerequisite
to viewing the models

#### R / R-Studio

<https://rstudio-education.github.io/hopr/starting.html>

#### Tensorflow

<https://keras.rstudio.com/install/index.html>

#### Database download

CSV and parquet files are required to be dowloaded from a Google Drive
repository. Files can be found
[here](https://drive.google.com/drive/u/0/folders/1C2TYJRsVH681dylc8ZX5A4gP-LsoaW7o)

# :triangular_ruler: Structure

    project
    │   README.md
    │
    └─── R
    │   │   amex_metric.R
    │   │   cleansing.R
    │   │   config.R
    │   │   database.R
    │   │   DNN.R
    │   │   logisticP2.R
    │   │   main.R
    │   │   NeuralNetwork.R
    │   │   Noise.R
    │   │   rf_logreg.R
    │   │   Noise.R
    │   │   file012.txt
    │   
    └─── notebooks
    │
    └─── cache
    │   │
    │   └─── models
    │   └─── objects 
    │   └─── NN_tuningRuns 
    │ 
    └─── plots
    │ 
    └─── paper
    │ 
    └─── readme

#### main.R

Primary file used to load all scripts.  
2 directories need to be specified on the host machine: PATH_WD and
PATH_DB.

#### Feature Engineering

Feature engineering functions are stored within 3 files:

-   database.R
-   Contains
-   cleansing.R
-   noise.R

![Structure
1](/Users/root1/Documents/DAC_Project/readme/images/structure1.png)

#### Machine Learning Models

4 Models are evaluated as part of the project

-   Logistic Regression
-   Logistic Regression (Subset on a single feature)
-   Random Forest
-   Neural Network
