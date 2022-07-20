---
output:
  pdf_document: default
  html_document: default
bibliography: bibliography.bib  
---
\spacing{1.25}
# Review of Literature  

## Taiwan Data

There have been numerous articles and  papers within the scope of using ML methods to predict credit default, this includes the prediction for credit card default.  
\vspace{.5 cm}
Studies examining credit card default have been concentrated, mainly using the data used originally used as part of \citet{YEH20092473} . This data is currently freely available as the Default credit card clients Data Set on the UC Irvine Machine Learning Repository.[^uci] This data, collected in October 2005, is from a cash and credit card issuing bank in Taiwan, the targets were credit card holders of the bank. Among the total 30,000 observations, 22.12% are the cardholders who defaulted on payment. This research employed a binary variable, default payment (Yes = 1, No = 0), as the response variable. This data uses 23 variables as explanatory variables including a mix of personal information(age,gender,marital status and education level), amount of credit given, historical bill statements, and historical payment information.  
This initial study, \citet{YEH20092473}, compared six classification algorithms - K-nearest neighbour, Logistic regression, Discriminant analysis Nave Bayesian classifier, Artificial Neural Networks, Classification Trees. In the classification accuracy, the results show that there are little differences in error amongst the six methods. The generated probability of default by the Artificial Neural Network most closely resembled the actual probability of default. The actual probability of default was estimated using a novel "Sorting Smoothing Method".  
\vspace{.5 cm}
Other research on predicting credit card default is subsequent years utilised this data to train and evaluate model. The following is a sample of articles available, applying a wide variety of Machine Learning methods to this classification problem.  
Another study, \citep{Neema2017TheCO}, took a similar approach in choice of methods but attempted to predict the best possible cost-effective outcome from the risk management perspective. Again, K-Nearest Neighbour Logistic Regression, Classification Trees and Discriminant Analysis were evaluated but also Naive Bayes and Random Forest classifiers were included. This was evaluated using a cost function which gave a higher cost to defaulters classified not correctly as they are the minority in the data. Defaulted payments can prove more costly to a bank rather than potential customers wrongly identified as a potential default case. It was concluded that original data with Random Forest algorithm is the best in terms of a good balance on cost versus accuracy.  
\citet{Yang2018-rt} introduces two new methods used to predict credit card default. Support Vector Machine (SVM) involves using a kernel function to map the predictor data into a high-dimensional feature space where the outcome classes are easily separable. XGBoost and LightGBM, forms of gradient boosted trees algorithm were used, as well as previously tried methods - Logistic Regression and Neural Networks. LightGBM and XGBoost were both deemed to have the best performance in the prediction of categorical response variables.     
While other studies have used Neural Networks in predicting credit card defaults, the models used have been vague and little detail has been given on architecture or tuning of the model. \citet{dnn2}, trialled a range of Networks, experimenting with two to five layers with number of processing units of 64, 32, 16 units. Neural Networks with three layers and 64 units recorded the highest accuracy of all configurations.  
\vspace{.5 cm}
Due to a lack of credit card specific data pertaining to defaulting on payment, all available studies which predict credit card default utilise the Taiwan data which is both region specific, dated over 15 years and obtained at a time when credit card issuers in Taiwan faced a credit card debt crisis.[^debt]

## Other Literature

In the scope of predicting defaults on other forms of credit such as mortgages, research has also been conducted using data collected by the Central Bank of Ireland, comprising four separate portfolios of over 300,000 owner-occupier mortgage loans of Irish lenders, \citet{FITZPATRICK2016427}. It was found that boosted regression trees provided the best classification algorithms for mortgage default prediction.
\vspace{.5 cm}  

A 2019 research thesis, \citet{trap5146}, examined a number of high-performing methods in predicting credit default on a Home Credit dataset[^hc]. Home Credit is an international non-bank financial institution that specializes in lending to people with little or no credit history. This study examined methods that could be deemed explainable - - which consisted of a number of tree-based ensemble methods. The top performing model was deemed to be XGBoost, a form of gradient boosted trees algorithm.   

[^uci]: 
https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients  

[^debt]:
https://sevenpillarsinstitute.org/case-studies/taiwans-credit-card-crisis/

[^hc]: 
https://www.kaggle.com/c/home-credit-default-risk
