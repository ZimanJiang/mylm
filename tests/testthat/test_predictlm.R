##tests
diff = 1e-6
n <- nrow(mtcars)
train_n <- floor(n*0.7)
traindata <- mtcars[1:train_n, ]
testdata <- mtcars[(train_n+1):n, ]
#SLR
test_that("predictlm works for the simple linear regression",{
  mylm1 <- mylm(mpg~wt,traindata,"nothing")
  lm1 <- lm(mpg~wt,traindata)
  expect_equal (sum(predictlm(mylm1,testdata)-predict(lm1,testdata)), 0, tolerance = diff)
})

#MLR
test_that("predictlm works for the multiple linear regression",{
  mylm2 <- mylm(mpg~wt+drat,traindata,"nothing")
  lm2 <- lm(mpg~wt+drat,traindata)
  expect_equal (sum(predictlm(mylm2,testdata)-predict(lm2,testdata)), 0, tolerance = diff)
})

#MLR with categorical covariate
test_that("mylm works for the multiple linear regression with categorical covariate",{
  mylm3 <- mylm(mpg~wt+drat+factor(cyl),traindata,"nothing")
  lm3 <- lm(mpg~wt+drat+factor(cyl),traindata)
  expect_equal (sum(predictlm(mylm3,testdata)-predict(lm3,testdata)), 0, tolerance = diff)
})

#MLR with interaction term
test_that("mylm works for the multiple linear regression with interaction term",{
  mylm4 <- mylm(mpg~wt+drat+I(wt*drat),traindata,"nothing")
  lm4 <- lm(mpg~wt+drat+I(wt*drat),traindata)
  expect_equal (sum(predictlm(mylm4,testdata)-predict(lm4,testdata)), 0, tolerance = diff)
})

#MLR with transformed covariate
test_that("mylm works for the multiple linear regression with transformed covariate",{
  mylm5 <- mylm(mpg~wt+drat+I(wt^2),traindata,"nothing")
  lm5 <- lm(mpg~wt+drat+I(wt^2),traindata)
  expect_equal (sum(predictlm(mylm5,testdata)-predict(lm5,testdata)), 0, tolerance = diff)
})
