# Statistical-Learning-Theory


This repository Assignment 1.1 from Statistical Learning Theory at Stellenbosch Univeristy. 

I compared the performance of LSTM RNNs, SVR and GBM on predicting the hourly electricity usage of a household. The data are sourced from Kaggle and
are available at https://www.kaggle.com/uciml/electric-power-consumption-data-set. The data was be split into training, validation and test data. The models were tuned using simple grid-search hyperparameter tuning with the validation performance determining the best model for each class. The best models will be retrained on the training and validation sets combined before performance on the test set is used to determine the best class of model for the problem.


The first model is the Support Vector Regression (SVR), implemented using liquidSVM, the second is the Gradient Boosting Machine (GBM), implemented using lightGBM, and thirdly the Long-Short Term Memory Recurrent Neural Network (LSTM), implemented using Keras and Tensorflow. The specific task is to forecast the following 12 hours of electricity usage from the previous 48 hours of electricity usage. The task is a multi-output regression problem.

Included is my final SLT submission. The questions summarises the mathematical underpinnings range in Machine Learning topics from train and test splits to Neural Networks and Support Vector Machines. The majority of answers are summarised from The Elements of Statistical Learning (Hastie et al. 2017) and Deep Learning (Goodfellow et al., 2016).