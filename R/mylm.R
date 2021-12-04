#'Fitting linear Models
#'
#'Fit the linear regression model.
#'
#'@import stats datasets
#'
#'
#'@param obj input formula
#'@param inputdata dataset
#'@param style Choose different kind of output. Nothing prints out if style = "nothing". Get only the model and coefficients if style = "simple". Get a summary of linear regresion if style = "summary"
#'
#'
#'
#'@return a list containing the following components:
#'@return \item{Call}{the formula of the model}
#'@return \item{fitted_value}{the fitted mean value}
#'@return \item{Residuals}{the usual residuals of the model}
#'@return \item{coefficients}{a p*4 matrix with columns for the estimated coefficients, their standard error, t-statistics and p-value of the t-test}
#'@return \item{RSE}{the square root of estimated variance (mean square error)}
#'@return \item{R_squared}{the fraction of variance explained by the model}
#'@return \item{adjusted_R_squared}{the fraction of variance explained by the model penalized by p}
#'@return \item{f_value}{the statistic of F test}
#'@return \item{p_f}{the p value of F test}
#'@return \item{d_f}{degrees of freedom, a 2-vector (p,n-p)}
#'
#'
#'@examples
#
#'mylm( mpg~wt+drat,inputdata = mtcars, style = "summary" )
#'mylm( mtcars$mpg~mtcars$wt+mtcars$drat )
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
  #t test
  t.stat <- c( beta.hat/se.beta.hat )
  ttest.p.value <- c( 2*( 1-pt( q = abs( t.stat ), df = n-p ) ) )
  #F test
  SSR <- sum((y.hat - mean(y))^2)
  F.stat <- SSR/(p-1)/sigma.2.hat
  Ftest.p.value <- pf( q = F.stat, df1 = p-1, df2 = n-p, lower.tail = F)
  #R square
  R.square <- 1- SSE /( SSE + SSR )
  adj.R.square <- 1- sigma.2.hat * ( n-1 )/( SSE + SSR )
  ##output
  #coefficient matrix
  coef.mat <- cbind( Estimate = c(beta.hat),
                     Std_Err = se.beta.hat,
                     t_statistic = t.stat,
                     p_value = ttest.p.value )
  coef.df <- signif(coef.mat, digits=7)
  #coef.df <- data.frame( coef.mat )
  #coef.df$p_value[coef.df$p_value==0] <- "< 2e-16" #printing format
  output <- list( Call = obj,
                  fitted_value = y.hat,
                  Residuals= e.hat,
                  coefficients=coef.mat,
                  RSE = SE, R_squared = R.square,
                  adjusted_R_squared = adj.R.square,
                  f_value = F.stat,
                  p_f = Ftest.p.value,
                  d_f = c( p, n-p ) )
  #print output
  if(style=="summary"){
    cat( "Call:\n",
        as.character(obj)[2],as.character(obj)[1],as.character(obj)[3],"\n","\n" )
    cat( "Coefficients:\n" )
    print(coef.mat)
    cat( "\n" )
    cat( "Residual standard error:", signif(SE,4), "on", n-p, "degrees of freedom", "\n" )
    cat( "Multiple R-squared:", signif(R.square,4), "Adjusted R-squared", signif(adj.R.square,4), "\n" )
    cat( "F statistic:", F.stat, "on", n-p, "degrees of freedom", "\n")
  }else if( style == "simple" ){
    colnames(beta.hat) <- "Estimate"
    cat( "Call:\n",
        as.character(obj)[2],as.character(obj)[1],as.character(obj)[3],"\n","\n" )
    cat( "Coefficients:\n" )
    print( t(beta.hat) )
  }else if( style == "nothing"){
    #print nothing
  }else{
    warning( "Invalid stlye" )
  }
  invisible(output)
}
