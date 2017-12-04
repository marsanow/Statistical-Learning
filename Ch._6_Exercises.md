# Ch. 6 Exercises



8. In this exercise, we will generate simulated data, and will then use this data to perform best subset selection.  

(a) Use the `rnorm()` function to generate a predictor X of length n = 100, as well as a noise vector ε of length n = 100.


```r
set.seed(1)
X <- rnorm(100)
e <- rnorm(100)
```
(b) Generate a response vector Y of length n = 100 according to the model
Y = β0 +β1X +β2X2 +β3X3 +ε, where β0, β1, β2, and β3 are constants of your choice.

```r
B0 <- -4
B1 <- 3
B2 <- -2
B3 <- 1
Y <- B0 + B1*X + B2*X^2 + B3*X^3 + e
```

(c) Use the `regsubsets()` function to perform best subset selection in order to choose the best model containing the predictors X,X2,...,X10. What is the best model obtained according to Cp, BIC, and adjusted R2? Show some plots to provide evidence for your answer, and report the coefficients of the best model obtained. Note you will need to use the `data.frame()` function to create a single data set containing both X and Y .


```r
require(leaps)
```

```
## Loading required package: leaps
```

```r
Xy <- data.frame(y=Y, x=X)
reg.fit.full <- regsubsets(y ~ I(X) + I(X^2)+ I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data=Xy, nvmax=10)
reg.fit.full.summary <- summary(reg.fit.full)
```

```r
names(reg.fit.full.summary)
```

```
## [1] "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"
```

```r
which.min(reg.fit.full.summary$cp)
```

```
## [1] 4
```

```r
reg.fit.full.summary$cp[4]
```

```
## [1] 0.6067483
```


```r
which.min(reg.fit.full.summary$bic)
```

```
## [1] 3
```

```r
reg.fit.full.summary$bic[3]
```

```
## [1] -327.9111
```


```r
which.max(reg.fit.full.summary$adjr2)
```

```
## [1] 4
```

```r
reg.fit.full.summary$adjr2[4]
```

```
## [1] 0.9685959
```



```r
par(mfrow=c(2,2))
plot(reg.fit.full.summary$cp ,xlab="Number of Variables ", ylab="Cp",
type="l")
points(which.min(reg.fit.full.summary$cp), reg.fit.full.summary$cp[which.min(reg.fit.full.summary$cp)], col="red", cex=2, pch=20)

plot(reg.fit.full.summary$bic ,xlab="Number of Variables ", 
ylab="BIC",type="l")
points(which.min(reg.fit.full.summary$bic), reg.fit.full.summary$bic[which.min(reg.fit.full.summary$bic)], col="red", cex=2, pch=20)

plot(reg.fit.full.summary$adjr2 ,xlab="Number of Variables ", 
ylab="Adjusted R^2^",type="l")
points(which.max(reg.fit.full.summary$adjr2), reg.fit.full.summary$adjr2[which.max(reg.fit.full.summary$adjr2)], col="red", cex=2, pch=20)
```

![](Ch._6_Exercises_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

The best model according to Cp and Adjusted R^2^ has 4 variables, and according to BIC 3 variables (BIC penalizes large models and therefore tends to choose smaller models).  


```r
coef(reg.fit.full, 4)
```

```
## (Intercept)        I(X)      I(X^2)      I(X^3)      I(X^5) 
## -3.92799225  3.38745596 -2.15424359  0.55797426  0.08072292
```

(d) Repeat (c), using forward stepwise selection and also using back- wards stepwise selection. How does your answer compare to the results in (c)?  

#### Forward Step Selection

```r
reg.fit.fwd <- regsubsets(y ~ I(X) + I(X^2)+ I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data=Xy, nvmax=10, method ="forward")
reg.fit.fwd.summary <- summary(reg.fit.fwd)
```

```r
which.min(reg.fit.fwd.summary$cp)
```

```
## [1] 4
```

```r
which.min(reg.fit.fwd.summary$bic)
```

```
## [1] 3
```

```r
which.max(reg.fit.fwd.summary$adjr2)
```

```
## [1] 4
```

```r
coef(reg.fit.fwd, 4)
```

```
## (Intercept)        I(X)      I(X^2)      I(X^3)      I(X^5) 
## -3.92799225  3.38745596 -2.15424359  0.55797426  0.08072292
```

#### Backward Step Selection

```r
reg.fit.bwd <- regsubsets(y ~ I(X) + I(X^2)+ I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data=Xy, nvmax=10, method ="backward")
reg.fit.bwd.summary <- summary(reg.fit.bwd)
```

```r
which.min(reg.fit.bwd.summary$cp)
```

```
## [1] 4
```

```r
which.min(reg.fit.bwd.summary$bic)
```

```
## [1] 3
```

```r
which.max(reg.fit.bwd.summary$adjr2)
```

```
## [1] 4
```

```r
coef(reg.fit.bwd, 4)
```

```
## (Intercept)        I(X)      I(X^2)      I(X^5)      I(X^7) 
## -3.93986035  3.70201369 -2.13582615  0.30330191 -0.02419389
```

All 3 methods selected a 4 variable model as the best model. Both best subset selection and forward selection chose the same variables - X, X^2^, X^3^ and X^5^. On the other hand, the backward selection method chose X, X^2^, X^5^ and X^7^ predictors.  

(e) Now fit a lasso model to the simulated data, again using X,X^2^, . . . , X^10^ as predictors. Use cross-validation to select the optimal value of λ. Create plots of the cross-validation error as a function of λ. Report the resulting coefficient estimates, and discuss the results obtained.


```r
library(glmnet)
```

```
## Warning: package 'glmnet' was built under R version 3.4.2
```

```
## Loading required package: Matrix
```

```
## Loading required package: foreach
```

```
## Loaded glmnet 2.0-13
```

```r
X <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data=Xy)[, -1]
Y <- Xy$y

set.seed(1)
cv.lasso.mod <- cv.glmnet(X,Y,alpha=1)
plot(cv.lasso.mod)
```

![](Ch._6_Exercises_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
min.lambda <- cv.lasso.mod$lambda.min
min.lambda
```

```
## [1] 0.02737278
```
Refit the model with the optimal lambda value = 0.02737278.

```r
predict(cv.lasso.mod, s = min.lambda, type = "coefficients")[1:11, ]
```

```
##   (Intercept)             x        I(x^2)        I(x^3)        I(x^4) 
## -3.950369e+00  3.301907e+00 -2.119422e+00  6.280779e-01  0.000000e+00 
##        I(x^5)        I(x^6)        I(x^7)        I(x^8)        I(x^9) 
##  6.675650e-02  0.000000e+00  3.351028e-05  0.000000e+00  0.000000e+00 
##       I(x^10) 
##  0.000000e+00
```
The model chosen by the lambda selected with cross-validation contains 5 variables out of 10 present in the model. The method chose to include the following variables: X, X^2^, X^3^, X^5^, and X^7^. 
The lasso method has a substantial advantage, because it zeros out the variables that were not selected in the model, so we're able to see clearly which variables were actually used in the model.  

(f) Now generate a response vector Y according to the model Y = β0 + β7X7 + ε, and perform best subset selection and the lasso. Discuss the results obtained.

```r
set.seed(1)
X <- rnorm(100)
e <- rnorm(100)
B0 <- -4
B7 <- 2
Y <- B0 + B7*X^7 + e
```

#### Best Subset Selection

```r
require(leaps)

Xy <- data.frame(y=Y, x=X)
reg.fit.full <- regsubsets(y ~ I(X) + I(X^2)+ I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data=Xy, nvmax=10)
reg.fit.full.summary <- summary(reg.fit.full)
```

```r
which.min(reg.fit.full.summary$cp)
```

```
## [1] 2
```

```r
reg.fit.full.summary$cp[2]
```

```
## [1] -0.5745758
```

```r
which.min(reg.fit.full.summary$bic)
```

```
## [1] 1
```

```r
reg.fit.full.summary$bic[1]
```

```
## [1] -960.2429
```

```r
which.max(reg.fit.full.summary$adjr2)
```

```
## [1] 4
```

```r
reg.fit.full.summary$adjr2[4]
```

```
## [1] 0.9999393
```

```r
par(mfrow=c(2,2))
plot(reg.fit.full.summary$cp ,xlab="Number of Variables ", ylab="Cp",
type="l")
points(which.min(reg.fit.full.summary$cp), reg.fit.full.summary$cp[which.min(reg.fit.full.summary$cp)], col="red", cex=2, pch=20)

plot(reg.fit.full.summary$bic ,xlab="Number of Variables ", 
ylab="BIC",type="l")
points(which.min(reg.fit.full.summary$bic), reg.fit.full.summary$bic[which.min(reg.fit.full.summary$bic)], col="red", cex=2, pch=20)

plot(reg.fit.full.summary$adjr2 ,xlab="Number of Variables ", 
ylab="Adjusted R^2^",type="l")
points(which.max(reg.fit.full.summary$adjr2), reg.fit.full.summary$adjr2[which.max(reg.fit.full.summary$adjr2)], col="red", cex=2, pch=20)
```

![](Ch._6_Exercises_files/figure-html/unnamed-chunk-24-1.png)<!-- -->
We pick the 2 variable model with Cp, the 1 variable model with BIC, and the 4 variable model with Adjusted R^2^.


```r
coef(reg.fit.full, 1)
```

```
## (Intercept)      I(X^7) 
##    -4.04106     2.00077
```

```r
coef(reg.fit.full, 2)
```

```
## (Intercept)      I(X^2)      I(X^7) 
##  -3.9295096  -0.1417084   2.0015552
```

```r
coef(reg.fit.full, 4)
```

```
## (Intercept)        I(X)      I(X^2)      I(X^3)      I(X^7) 
##  -3.9237476   0.2914016  -0.1617671  -0.2526527   2.0091338
```

#### Lasso Regression

```r
require(glmnet)

X <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data=Xy)[, -1]
Y <- Xy$y

set.seed(2)
cv.lasso.mod <- cv.glmnet(X,Y,alpha=1)
plot(cv.lasso.mod)
```

![](Ch._6_Exercises_files/figure-html/unnamed-chunk-26-1.png)<!-- -->


```r
min.lambda <- cv.lasso.mod$lambda.min
min.lambda
```

```
## [1] 3.879577
```


```r
predict(cv.lasso.mod, s = min.lambda, type = "coefficients")[1:11, ]
```

```
## (Intercept)           x      I(x^2)      I(x^3)      I(x^4)      I(x^5) 
##   -3.770915    0.000000    0.000000    0.000000    0.000000    0.000000 
##      I(x^6)      I(x^7)      I(x^8)      I(x^9)     I(x^10) 
##    0.000000    1.936760    0.000000    0.000000    0.000000
```

The lasso method chose the 1 variable model using only X^7^ variable.


tbc








