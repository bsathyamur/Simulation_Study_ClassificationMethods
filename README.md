## PROBLEM STATEMENT
This assignment is related to the simulation study described in Section 2.3.1 (the so-called Scenario 2) of “Elements of Statistical Learning” (ESL).Scenario 2: the two-dimensional data X ∈ R2 in each class is generated from a mixture of 10 different bivariate Gaussian distributions with uncorrelated components and different means, i.e.,
      X|Y = k, Z = l ∼ N(mkl, s^2I2),
where k = 0, 1, l = 1 : 10, P(Y = k) = 1/2, and P(Z = 1) = 1/10. 

You can choose your own values for s and the twenty 2-dim vectors mkl, or you can generate them from some distribution.

Repeat the following simulation 20 times. In each simulation,
  1. follow the data generating process to generate a training sample of size 200 and a test sample of size 10,000, and
  2. calculate the training and test errors (the averaged 0/1 error1) for each the following four procedures:
    • Linear regression with cut-off value2 0.5,
    • quadratic regression with cut-off value 0.5,
    • kNN classification with k chosen by 10-fold cross-validation, and
    • the Bayes rule (assume your know the values of mkl’s and s).
    
Summarize your results on training errors and test errors graphically, e.g., using boxplot or stripchart. Also report the mean and standard error for the chosen k values.

#### Function to simulate training and test dataset
````
simulateData = function(){
..........
  df_train["X1"] = traindata[,1]
  df_train["X2"] = traindata[,2]
  df_train["Y"] = as.numeric(Ytrain)-1
  
  df_test["X1"] = testdata[,1]
  df_test["X2"] = testdata[,2]
  df_test["Y"] = as.numeric(Ytest)-1
  
  retlst = list(train = df_train,test = df_test,center1 = m0,center2 = m1)
  return(retlst)
}
````
#### Run 20 simulations for different models - Linear,Quadratic,BayesRule and KNN
````
mixnorm = function(x){
   ## return the density ratio for a point x, where each 
   ## density is a mixture of normal with 10 components
   sum(exp(-apply((t(m1)-x)^2, 2, sum)*5/2))/sum(exp(-apply((t(m0)-x)^2, 2, sum)*5/2))
}

#Function to select K values from the train dataset
selectbestcvK = function(train){
..............
  minErrorRate = min(cvError$error_rate) 
  selectedK = max(cvError[cvError$error_rate == minErrorRate,]$K)
  return(selectedK) 
}

error_rate = data.frame("method" = rep("",df_rows),"set" = rep("",df_rows),"errorRate" = rep(0,df_rows))
selectedK = data.frame("K" = rep(0,20))

n = 200
N = 10000
row = 1

for(sim in 1:num_sims){
  
  lst = simulateData()
  ................ 
  #Linear Regression Model
  regModel = lm(Y~X1+X2,data = train)
  Ytrain_pred = as.numeric(regModel$fitted>0.5)
  Ytest_pred = as.numeric(predict(regModel,newdata = test) > 0.5)
  ..................
  #Quadratic Regression Model
  quadModel = lm(Y ~ X1 + X2 + I(X1 * X2) + I(X1^2) + I(X2^2),data = train)
  Ytrain_pred = as.numeric(quadModel$fitted>0.5)
  Ytest_pred = as.numeric(predict(quadModel,newdata = test) > 0.5)
  ...................
  #Bayer Error
  traindata = data.matrix(train[1:2])
  Ytrain_pred = apply(traindata, 1, mixnorm)
  Ytrain_pred = as.numeric(Ytrain_pred > 1)

  testdata = data.matrix(test)
  Ytest_pred = apply(testdata, 1, mixnorm)
  Ytest_pred = as.numeric(Ytest_pred > 1)
  ..................
  #Calling the function to select the best K
  K = selectbestcvK(train)
  selectedK[sim,"K"] = K 
  
  # KNN method
  trainX = train[,c("X1", "X2")]
  trainY = as.factor(train$Y)
  testX = test[,c("X1", "X2")]
  testY = as.factor(Ytest)
  
  predictTrnY = knn(trainX,trainX,trainY,K)
  predictTestY = knn(trainX,testX,trainY,K)
}
````
#### Plot the performance of the models in boxplot using ggplot
````
library(ggplot2)
ggplot(error_rate, aes(x = method, y = errorRate, color = set)) + geom_boxplot(width = 0.2) + ggtitle("Comparison of performance of the 4 models") +
  theme(plot.title = element_text(hjust = 0.5))
````
![comparison](https://github.com/bsathyamur/Simulation_Study_ClassificationMethods/blob/master/comparison.png)

#### Calculate the mean and sd for the selected K values
````
meanK = round(mean(selectedK$K),digits = 2)
sdK = round(sd(selectedK$K),digits = 2)
````
