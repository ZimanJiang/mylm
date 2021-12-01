#'mylm
#'
#'Fit the linear regression model
#'
#'@param obj input formula
#'
#'@return the summary of the model
#'
#'@examples
#'mylm(Y~X1+X2)
#'
#'@export
#'
mylm <- function(obj){
  ##define outcome, predictor, dim #input obj is a formula
  y <- obj[[2]]
  x <- model.matrix(obj)
  n <- nrow(x)
  p <- ncol(x)
  ##estimation
  #estimated coef
  beta.hat <- solve( t(x) %*% x ) %*% t(x) %*% y
  #fitted value
  y.hat <- x %*% beta.hat
  #residuals
  e.hat <- y - y.hat
  #estimated sigma^2
  SSE <- as.numeric( t( e.hat ) %*% e.hat )
  sigma.2.hat <- SSE /( n-p )
  SE <- sqrt( sigma.2.hat )
  #variance of est coef
  var.beta.hat <- diag( solve( t(x) %*% x ) )* c( sigma.2.hat )
  se.beta.hat <- sqrt( var.beta.hat )
  ##inference
  t.stat <- c( beta.hat/se.beta.hat )
  ttest.p.value <- c( 2*( 1-pt( q = abs( t.stat ), df = n-p ) ) )
  SSR <- sum((y.hat - mean(y))^2)
  F.stat <- SSR/(p-1)/sigma.2.hat
  Ftest.p.value <- pf( q = F.stat, df1 = p-1, df2 = n-p, lower.tail = F)
  R.square <- 1- SSE /( SSE + SSR )
  adj.R.square <- 1- sigma.2.hat * ( n-1 )/( SSE + SSR )
  ##output
  coef <- cbind( Estimate = c(beta.hat), Std_Err = se.beta.hat, t_statistic = t.stat, p_value = ttest.p.value )
  infer <- c( F.stat, R.squre )
  output <- list(obj,coef,infer)
  print(output)
  return(output)
}
