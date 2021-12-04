# zmjianglm

<!-- badges: start -->
[![R-CMD-check](https://github.com/ZimanJiang/mylm/workflows/R-CMD-check/badge.svg)](https://github.com/ZimanJiang/mylm/actions)
[![codecov](https://codecov.io/gh/ZimanJiang/zmjianglm/branch/main/graph/badge.svg?token=WU8AUP0NCF)](https://codecov.io/gh/ZimanJiang/zmjianglm)
<!-- badges: end -->


# Overview
This is a package used to fit linear regression model. It contains two functions:

- `mylm()` to fit a linear regression model and choose the printing style
- `predictlm()` to get predicted value of the new data set based on the fitted model

`mylm()` allows you to get important items in linear regression fitting. You can learn more about the usage in ` vignette("TutorialandComparison")`

# Installation
To install the package:

```{r,eval=FALSE}
devtools::install_github("ZimanJiang/zmjianglm", build_vignettes = TRUE)
```
# Usage
```{r}
library(zmjianglm)

n <- nrow(mtcars)
train_n <- floor(n*0.7)
traindata <- mtcars[1:train_n, ]#the original data set
testdata <- mtcars[(train_n+1):n, ]#the new data set

model <- mylm(mpg~wt,traindata,"summary")
predicted_value <- predictlm(model,testdata)
```


