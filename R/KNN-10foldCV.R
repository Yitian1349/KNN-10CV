#' Use K Near Neighbors method to make predictions
#'
#' This function can be used for both regression and classification problems
#' Using 10-fold cross validation to choose the best k for the model
#' @param Xtrain The train data set without the response value, only the predictors
#' @param ytrain The response value in the train data set
#' @param Xtest The test data set
#' @param method The regression or classification
#' @return The prediction results for the test data
#' @examples
#' install.packages("titanic");
#' library(titanic);
#' train<-titanic_train;
#' Xtrain<-train[,-c(2,4,11)];
#' ytrain<-train[,2];
#' test<-titanic_test;
#' test<-test[,-c(3,10)];
#' Xtrain=na.omit(Xtrain);
#' test$Age[is.na(test$Age)]=mean(test$Age,na.rm=T);
#' test$Fare[is.na(test$Fare)]=mean(test$Fare,na.rm=T);
#' KNN_10CV(Xtrain,ytrain,Xtest=test,method="regression")
#' @export
KNN_10CV<-function(Xtrain,ytrain,Xtest,method){
  #2 methods: "regression" and "classification"

  #scale the data
  combined_data<-rbind(Xtrain,Xtest)
  combined_data_matrix<-model.matrix( ~ ., data=combined_data)[,-1]
  scaled_data<-scale(combined_data_matrix)
  X.train<-scaled_data[1:nrow(Xtrain),]
  X.test<-scaled_data[(nrow(Xtrain)+1):nrow(scaled_data),]
  #set seed
  set.seed(123)
  fold.index <- cut(sample(1:nrow(test)), breaks=10, labels=FALSE)
  K.vt <- c(1:nrow(test))
  error.k <- rep(0, length(K.vt))
  counter <- 0
  # knn regression
  if (method=="regression"){
    for(k in K.vt){
      # counter for error.k
      counter <- counter + 1
      # initialize an mse object to record the MSE for each fold
      mse <- rep(0,10)
      # 10 fold cross validation
      for(i in 1:10){
        pred.out <- FNN::knn.reg(X.train[fold.index!=i,], X.train[fold.index==i,],
                            ytrain[fold.index!=i], k=k)
        mse[i] <- mean((pred.out$pred - ytrain[fold.index==i])^2)
      }
      error.k[counter] <- sum(mse)/10
    }
    #find the best k which has the smallest MSE
    real_k<-K.vt[which.min(error.k)]
    final_pred<-FNN::knn.reg(train = X.train, test = X.test, y = ytrain, k = real_k)
    return(final_pred)
  }
  #knn classification
  else{
    for(k in K.vt){
      # counter for error.k
      counter <- counter + 1
      # initialize an mse object to record the MSE for each fold
      mse <- rep(0,10)
      # 10 fold cross validation
      for(i in 1:10){
        pred.out <- FNN::knn(X.train[fold.index!=i,], X.train[fold.index==i,],
                        ytrain[fold.index!=i], k=k)
        mse[i] <- mean((pred.out$pred - ytrain[fold.index==i])^2)
      }
      error.k[counter] <- sum(mse)/10
    }
    #find the best k which has the smallest MSE
    real_k<-K.vt[which.min(error.k)]
    final_pred<-FNN:knn(train = X.train, test = X.test, y = ytrain, k = real_k)
    return(final_pred)
  }

}
