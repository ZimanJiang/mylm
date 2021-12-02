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
#'library(NHANES)
#'data(NHANES)
#'mylm( BMI~Age+Gender,data = NHANES, style = "summary" )
#'
#'@export
#'
mylm <- function(obj, inputdata=NULL, style="simple"){
  ##define outcome, predictor, dim #obj is a formula
  if(is.null(inputdata)){
    y <- model.frame(obj)[,1]
    x <- model.matrix(obj)
  }else{
    y <- model.frame(obj,data=inputdata)[,1]
    x <- model.matrix(obj,data=inputdata)
  }
  n <- nrow(x)
  p <- ncol(x)
  ##estimation
  #estimated coef
  beta.hat <- solve( t(x) %*% x ) %*% t(x) %*% y
  #fitted value
  y.hat <- x %*% beta.hat
  #residuals
  e.hat <- y - y.hat
  sum.e.hat <- cbind(Min = min(e.hat), first_Q = quantile(e.hat, 0.25), Median = median(e.hat), third_Q = quantile(e.hat, 0.75), Max = max(e.hat))#stat features of residuals
  row.names(sum.e.hat) <- NULL
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
  #ttest.p.value[ttest.p.value==0] <- "< 2e-16"
  SSR <- sum((y.hat - mean(y))^2)
  F.stat <- SSR/(p-1)/sigma.2.hat
  Ftest.p.value <- pf( q = F.stat, df1 = p-1, df2 = n-p, lower.tail = F)
  R.square <- 1- SSE /( SSE + SSR )
  adj.R.square <- 1- sigma.2.hat * ( n-1 )/( SSE + SSR )
  ##output
  df <- n-p
  coef <- cbind( Estimate = c(beta.hat), Std_Err = se.beta.hat, t_statistic = t.stat, p_value = ttest.p.value )
  infer <- c( F.stat, R.square, adj.R.square)
  output <- list(Call = obj, Residuals= sum.e.hat, coefficients=coef,infer)
  #print output
  if(style=="summary"){
    cat("Call:\n",
        as.character(obj)[2],as.character(obj)[1],as.character(obj)[3],"\n","\n")
    cat("Coefficients:\n",colnames(coef),"\n")
  }
  return(output)
}
