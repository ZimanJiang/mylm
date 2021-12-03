#test function
test_mylm <- function(mymodel, lmmodel){
  diff <- 1e-6
  #coefficients
  expect_equal( mymodel$coefficients[,1],
                lmmodel$coefficients[,1],
                tolerance=diff )
  #residuals
  expect_equal( mymodel$fitted_value,
                lmmodel$residuals,
                tolerance=diff )
}
#SLR
test_that("mylm works for the simple linear regression",{
  mylm1 <- mylm(mpg~wt,mtcars,"nothing")
  lm1 <- summary(lm(mpg~wt,mtcars))
  test_mylm (mylm1, lm1)
})
#MLR
mylm2 <- mylm(mpg~wt+drat,mtcars,"nothing")
lm2 <- summary(lm(mpg~wt+drat,mtcars))
#MLR with categorical covariate
mylm3 <- mylm(mpg~wt+drat+factor(gear),mtcars,"nothing")
lm3 <- summary(lm(mpg~wt+drat+factor(gear),mtcars))
#MLR with interaction term
mylm4 <- mylm(mpg~wt+drat+I(wt*drat),mtcars,"nothing")
lm4 <- summary(lm(mpg~wt+drat+I(wt*drat),mtcars))
#MLR with transformed covariate
mylm5 <- mylm(mpg~wt+drat+I(wt^2),mtcars,"nothing")
lm5 <- summary(lm(mpg~wt+drat+I(wt^2),mtcars))
