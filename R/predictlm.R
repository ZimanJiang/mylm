#'Predict method for linear model fits
#'
#'Predicted values of a new dataset based on the fitted linear regression model.
#'
#'@import stats, datasets
#'
#'@param op output of mylm function
#'@param newdata new dataset
#'
#'@return
#'Prediction of model
#'
#'@examples
#
#'model1 <- mylm( mpg~wt+gear,inputdata = mtcars, style = "nothing" )
#'newdata <- data.frame( wt = c(2.945, 3.130, 4.280), gear = c( 3,3,4 ) )
#'predictlm( model1, newdata )
#'
#'@export
#'
#'
predictlm <- function( op, newdata ){
  y.name <- op$Call[[2]]
  newdata <- cbind( y= rep(1, nrow(newdata)), newdata)
  names(newdata) <- c( as.character(y.name), names(newdata)[-1] )
  predictor <- model.matrix(op$Call, newdata)
  predict_value <- predictor %*% op$coefficients[,1]
  return( predict_value )
}
