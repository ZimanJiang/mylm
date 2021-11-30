# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#hello <- function() {
#  print("Hello, world!")
#  print("Hello, Kimmy!")
#}

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
}
