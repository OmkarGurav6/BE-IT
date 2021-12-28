

# Univariate linear regression

    uni_data <- read.csv('C:/Users/DELL/Downloads/Data_Set_for_Univariate_Regression.csv')
    
    summary(uni_data)
    
    # Checking if there are any NA values
    sum(is.na(uni_data))
    
    # Creating random sample
    index <- sample(1:nrow(uni_data),size = 0.7*nrow(uni_data) )
    
    # Spliting dataset into training and testing using index
    train <- uni_data[index,]
    
    test <- uni_data[-index,]
    
    # Linear regression model for univariate data
    univariate_model <- lm(Y~X, train)
    
    summary(univariate_model)
    
    # Predicting on train set
    train_predictions <- predict(univariate_model, data.frame(X = train$X))
    
    # Predicting on test set
    test_predictions <- predict(univariate_model, data.frame(X = test$X))
    
    require("Metrics")
    
    
    Train_MSE <- mse(train$Y, train_predictions)
    Train_MSE
    
    Test_MSE <- mse(test$Y, test_predictions)
    Test_MSE
    
    xla =  c("Train_Error","Test_Error")
    vec = c(Train_MSE,Test_MSE)
    
    par(mfrow=c(1,1))
    # Plotting Train MSE and Test MSE
    barplot(vec, names.arg = xla, xlab = 'Errors', main="Training and Testing Error", ylim = c(0, 2000))
    
    # Plotting regression line for univariate model
    plot(train$X,train$Y,xlab = 'X', ylab = 'Y')
    abline(univariate_model, col = 'red')


# Multivariate linear regression
    
    multi_data <- read.csv('C:/Users/DELL/Downloads/Regression_Data_set_Batch1.csv')
    
    summary(multi_data)
    
    # Checking if there are any NA values
    sum(is.na(multi_data))
    
    # Checking which variables are numerical
    str(multi_data)
    
    # Converting character variables into numeric variables
    multi_data[,c(6:10,12,13)] <- lapply(multi_data[,c(6:10,12,13)], as.factor )
    str(multi_data)
    
    multi_data[,c(6:10,12,13)] <- lapply(multi_data[,c(6:10,12,13)], as.integer )
    str(multi_data)
    
    # Correlation matrix of independent variables and dependent variable
    cor(multi_data)
  
    
    # Creating random sample
    index_1 <- sample(1:nrow(multi_data),size = 0.7*nrow(multi_data) )
    
    # Spliting dataset into training and testing using index
    train_1 <- multi_data[index_1,]
    
    test_1 <- multi_data[-index_1,]

    # Linear regression model for multivariate data
    multivariate_model_1 <- lm(price ~ area + bathrooms + airconditioning, train_1)
    summary(multivariate_model_1)
    
    multivariate_model_2 <- lm(price ~ area + bathrooms + stories, train_1)
    summary(multivariate_model_2)
    
    multivariate_model_3 <- lm(price ~ area + bathrooms + stories + airconditioning, train_1)
    summary(multivariate_model_3)
    
    multivariate_model_4 <- lm(price ~ ., train_1)
    summary(multivariate_model_4)

    
    # Predicting on train set
    train_predictions_1 <- predict(multivariate_model_1, train_1)
    
    train_predictions_2 <- predict(multivariate_model_2, train_1)
    
    train_predictions_3 <- predict(multivariate_model_3, train_1)
    
    train_predictions_4 <- predict(multivariate_model_4, train_1)
    
    # Predicting on test set
    test_predictions_1 <- predict(multivariate_model_1, test_1)    

    test_predictions_2 <- predict(multivariate_model_2, test_1)
    
    test_predictions_3 <- predict(multivariate_model_3, test_1)
    
    test_predictions_4 <- predict(multivariate_model_4, test_1)


    require("Metrics")
    
    Train_MSE_1 <- mse(train_1$price, train_predictions_1)
    
    Train_MSE_2 <- mse(train_1$price, train_predictions_2)
    
    Train_MSE_3 <- mse(train_1$price, train_predictions_3)
    
    Train_MSE_4 <- mse(train_1$price, train_predictions_4)
    
    c(Train_MSE_1, Train_MSE_2, Train_MSE_3, Train_MSE_4)
    
    
    Test_MSE_1 <- mse(test_1$price, test_predictions_1)
    
    Test_MSE_2 <- mse(test_1$price, test_predictions_2)
    
    Test_MSE_3 <- mse(test_1$price, test_predictions_3)
    
    Test_MSE_4 <- mse(test_1$price, test_predictions_4)
    
    c(Test_MSE_1, Test_MSE_2, Test_MSE_3, Test_MSE_4)

    
    xla =  c("Train_Error","Test_Error")
    
    
    vec_1 = c(Train_MSE_1,Test_MSE_1)
    vec_2 = c(Train_MSE_2,Test_MSE_2)
    vec_3 = c(Train_MSE_3,Test_MSE_3)
    vec_4 = c(Train_MSE_4,Test_MSE_4)

    # Setting graphical parameters to divide the plotting area into 2 by 2 
    par(mfrow=c(2,2))
    
    # Plotting Train MSE and Test MSE
    barplot(vec_1, names.arg = xla, xlab = 'Errors', main="Training and Testing Errors of Model 1", ylim = c(0.0e+00,2.0e+12))
    
    barplot(vec_2, names.arg = xla, xlab = 'Errors', main="Training and Testing Errors of Model 2", ylim = c(0.0e+00,2.0e+12))
    
    barplot(vec_3, names.arg = xla, xlab = 'Errors', main="Training and Testing Errors of Model 3", ylim = c(0.0e+00,2.0e+12))
    
    barplot(vec_4, names.arg = xla, xlab = 'Errors', main="Training and Testing Errors of Model 4", ylim = c(0.0e+00,2.0e+12))
    
    
    library(caret)
    
    # K-fold Cross-Validation for model 4
    
    set.seed(125)
    
    train_control <- trainControl(method = "cv", number = 10)
    
    k_fold_model <- train(price ~ ., data = train_1, method = "lm", trControl = train_control)
    
    print(k_fold_model)

    
    
    