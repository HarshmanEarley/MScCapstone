---
output:
  pdf_document: default
  html_document: default
bibliography: bibliography.bib  
---
\spacing{1}
# Review of Literature  

There have been numerous articles and  papers within the scope of using ML methods to predict credit default, this includes the prediction for credit card default.  
\vspace{.3 cm}
Studies examining credit card default have been concentrated, mainly using data  originally used as part of \citet{YEH20092473}. This data is currently freely available as the Default credit card clients Data Set on the UC Irvine Machine Learning Repository.[^uci] This data, collected in October 2005 is from a cash and credit card issuing bank in Taiwan. Among the 30,000 observations, 22.12% are the cardholders who defaulted on payment. This research employed a binary variable, default payment (Yes = 1, No = 0), as the response variable. This data uses 23 explanatory variables, including a mix of personal information(age, gender, marital status and education level), amount of credit given, historical bill statements, and historical payment information.   
\vspace{.3 cm}
\citet{YEH20092473}, compared six classification algorithms - K-nearest neighbour, Logistic regression, Discriminant analysis Na\char239 ve Bayesian classifier, Artificial Neural Networks, Classification Trees. In the classification accuracy, the results show that there were few differences in error amongst the six methods. The generated probability of default by the Artificial Neural Network most closely resembled the actual probability of default which was estimated using a novel "Sorting Smoothing Method".   
\vspace{.3 cm}
Other research on predicting credit card default in subsequent years utilised this data for training and evaluating models. The following is a sample of articles available, applying a wide variety of Machine Learning methods to this classification problem. \citet{Neema2017TheCO}, took a similar approach in the choice of methods but attempted to predict the best possible cost-effective outcome from the risk management perspective. Naive Bayes Estimator and Random Forest classifiers were introduced. This was evaluated using a cost function which gave a higher cost to defaulters classified not correctly as they are the minority in the data. It was concluded that original data with the Random Forest algorithm is the best in terms of a good balance on cost versus accuracy.   
\vspace{.3 cm}
\citet{Yang2018-rt} introduces two new methods used to predict credit card default. Support Vector Machine (SVM) involves using a kernel function to map the predictor data into a high-dimensional feature space where the outcome classes are easily separable. XGBoost and LightGBM, forms of gradient boosted trees algorithm were used. LightGBM and XGBoost were both deemed to have the best performance in the prediction of categorical response variables.     
\vspace{.3 cm}
While other studies have used Neural Networks in predicting credit card defaults, the models used have been vague, and little detail has been given on the architecture or tuning of the model. \citet{dnn2} trialled a range of Networks, experimenting with two to five layers with the number of processing units of 64, 32 and 16 units. Neural Networks with three layers and 64 units recorded the highest accuracy of all configurations.    
\vspace{.3 cm}
Due to a lack of credit card-specific data pertaining to defaulting on payment, all available studies which predict credit card default utilise the Taiwan data, which is both region specific, over 15 years old and obtained at a time when credit card issuers in Taiwan faced a credit card debt crisis.[^debt]


[^uci]: 
https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients  

[^debt]:
https://sevenpillarsinstitute.org/case-studies/taiwans-credit-card-crisis/


