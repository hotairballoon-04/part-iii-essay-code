lasso_base <- list(
  name="lasso",
  fit = function(X, y) {
    glmnet::cv.glmnet(x = X, y = y, alpha = 1)
  },
  
  predict = function(model, X_test) {
    as.numeric(predict(model, newx = X_test, s = "lambda.min"))
  }
)

rf_base <- list(
  name = "random_forest",
  
  fit = function(X, y) {
    data <- data.frame(y = y, X)
    randomForest::randomForest(y ~ ., data = data)
  },
  
  predict = function(model, X_test) {
    as.numeric(predict(model, newdata = data.frame(X_test)))
  }
)
