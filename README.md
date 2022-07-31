COMPARISON AND EVALUATION OF DIFFERENTMACHINE LEARNING METHODS AT
PREDICTINGCREDIT CARD DEFAULT
================
Sidney Harshman-Earley Denis O’Riordan

-   [Introduction](#introduction)
-   [Overview](#overview)
-   [Intalation Prerequisites](#intalation-prerequisites)
    -   [R / R-Studio](#r--r-studio)
    -   [Tensorflow](#tensorflow)
-   [Structure](#structure)
    -   [Feature Engineering](#feature-engineering)
    -   [Machine Learning Models](#machine-learning-models)

# Introduction

The aim of this project is to apply a range of machine learning
techniques to predict credit card default using the historical data of
credit card customers. The following report describes the process
undertaken to compare a number of models. Beginning with an industrial
size dataset, cleansing and formatting the data before using it to
train, tune and evaluate the chosen models based on the historical data
of credit card customers.

# Overview

# Intalation Prerequisites

Each of the software packages below must be installed a a prerequisite
to viewing the models

## R / R-Studio

<https://rstudio-education.github.io/hopr/starting.html>

## Tensorflow

<https://keras.rstudio.com/install/index.html>

# Structure

## Feature Engineering

Feature engineering functions are stored within 3 files:

-   database.R
-   cleansing.R
-   noise.R

![Structure 1](readme/images/structure1.png)

## Machine Learning Models

4 Models are evaluated as part of the project

-   Logistic Regression
-   Logistic Regression (Subset on a single feature)
-   Random Forest
-   Neural Network
