##test function
test_mylm <- function(mymodel, lmmodel){
  diff <- 1e-5
  #coefficients
  expect_equal( mymodel$coefficients[,1],
                lmmodel$coefficients[,1],
                tolerance=diff )
  #residuals
    #Since the residual is equal to the outcome minus the fitted value, it guarantees the equality of fitted values
  expect_equal( sum(mymodel$Residuals-lmmodel$residuals),0,
                tolerance=diff )
  #standard error
  expect_equal( mymodel$coefficients[,2],
                lmmodel$coefficients[,2],
                tolerance=diff )
  #standard error
  expect_equal( mymodel$coefficients[,2],
                lmmodel$coefficients[,2],
                tolerance=diff )
  #t statistic
  expect_equal( mymodel$coefficients[,3],
                lmmodel$coefficients[,3],
                tolerance=diff )
  #p-value of t test
  expect_equal( mymodel$coefficients[,4],
                lmmodel$coefficients[,4],
                tolerance=diff )
  #residual standard error
  expect_equal( mymodel$RSE,
                lmmodel$sigma,
                tolerance=diff )
  #R squared
  expect_equal( mymodel$R_squared,
                lmmodel$r.squared,
                tolerance=diff )
  #adjusted R squared
  expect_equal( mymodel$adjusted_R_squared,
                lmmodel$adj.r.squared,
                tolerance=diff )
  #F statistic
  expect_equal( mymodel$f_value,
                unname(lmmodel$fstatistic[1]),
                tolerance=diff )
  #degree of freedom
  expect_equal( mymodel$d_f[1],
                lmmodel$df[1],
                tolerance=diff )
  expect_equal( mymodel$d_f[2],
                lmmodel$df[2],
                tolerance=diff )
}

##tests
#SLR
test_that("mylm works for the simple linear regression",{
  mylm1 <- mylm(mpg~wt,mtcars,"nothing")
  lm1 <- summary(lm(mpg~wt,mtcars))
  test_mylm (mylm1, lm1)
})

#MLR
test_that("mylm works for the multiple linear regression",{
  mylm2 <- mylm(mpg~wt+drat,mtcars,"nothing")
  lm2 <- summary(lm(mpg~wt+drat,mtcars))
  test_mylm (mylm2, lm2)
})

#MLR with categorical covariate
test_that("mylm works for the multiple linear regression with categorical covariate",{
  mylm3 <- mylm(mpg~wt+drat+factor(cyl),mtcars,"nothing")
  lm3 <- summary(lm(mpg~wt+drat+factor(cyl),mtcars))
  test_mylm (mylm3, lm3)
})

#MLR with interaction term
test_that("mylm works for the multiple linear regression with interaction term",{
  mylm4 <- mylm(mpg~wt+drat+I(wt*drat),mtcars,"nothing")
  lm4 <- summary(lm(mpg~wt+drat+I(wt*drat),mtcars))
  test_mylm (mylm4, lm4)
})

#MLR with transformed covariate
test_that("mylm works for the multiple linear regression with transformed covariate",{
  mylm5 <- mylm(mpg~wt+drat+I(wt^2),mtcars,"nothing")
  lm5 <- summary(lm(mpg~wt+drat+I(wt^2),mtcars))
  test_mylm (mylm5, lm5)
})
