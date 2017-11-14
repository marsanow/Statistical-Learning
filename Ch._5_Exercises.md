# Ch. 5 Exercises


5. In Chapter 4, we used logistic regression to predict the probability of default using income and balance on the Default data set. We will now estimate the test error of this logistic regression model using the validation set approach. Do not forget to set a random seed before beginning your analysis.

```r
require(ISLR)
```

```
## Loading required package: ISLR
```

```r
data("Default")
attach(Default)
```
(a) Fit a logistic regression model that uses income and balance to predict default.

```r
glm.fit <- glm(default ~ income + balance, data=Default, family = binomial)
summary(glm.fit)
```

```
## 
## Call:
## glm(formula = default ~ income + balance, family = binomial, 
##     data = Default)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4725  -0.1444  -0.0574  -0.0211   3.7245  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.154e+01  4.348e-01 -26.545  < 2e-16 ***
## income       2.081e-05  4.985e-06   4.174 2.99e-05 ***
## balance      5.647e-03  2.274e-04  24.836  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2920.6  on 9999  degrees of freedom
## Residual deviance: 1579.0  on 9997  degrees of freedom
## AIC: 1585
## 
## Number of Fisher Scoring iterations: 8
```

(b) Using the validation set approach, estimate the test error of this model. In order to do this, you must perform the following steps:
i. Split the sample set into a training set and a validation set.  

```r
set.seed(33)
train_id <- sample(10000, 5000)

train <- Default[train_id, ]
test <- Default[-train_id, ]
test.Default <- test$default
```

ii. Fit a multiple logistic regression model using only the training observations.  

```r
glm.fit.train <- glm(default ~ income + balance, data=train, family = binomial)
summary(glm.fit.train)
```

```
## 
## Call:
## glm(formula = default ~ income + balance, family = binomial, 
##     data = train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.3612  -0.1411  -0.0579  -0.0216   3.7160  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.124e+01  6.049e-01 -18.588   <2e-16 ***
## income       1.523e-05  7.125e-06   2.137   0.0326 *  
## balance      5.522e-03  3.151e-04  17.524   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1436.67  on 4999  degrees of freedom
## Residual deviance:  769.94  on 4997  degrees of freedom
## AIC: 775.94
## 
## Number of Fisher Scoring iterations: 8
```

iii. Obtain a prediction of default status for each individual in the validation set by computing the posterior probability of default for that individual, and classifying the individual to the default category if the posterior probability is greater than 0.5.  

```r
glm.fit.prob <- predict(glm.fit.train, test, type = "response")
glm.fit.pred <- ifelse(glm.fit.prob>0.5, "Yes", "No")
table(glm.fit.pred, test.Default)
```

```
##             test.Default
## glm.fit.pred   No  Yes
##          No  4813  125
##          Yes   17   45
```

iv. Compute the validation set error, which is the fraction of the observations in the validation set that are misclassified.  

```r
mean(glm.fit.pred != test.Default)
```

```
## [1] 0.0284
```
The validation set error is 2.88% (misclassified observations).  

(c) Repeat the process in (b) three times, using three different splits of the observations into a training set and a validation set. Comment on the results obtained.  
Sample 1  

```r
set.seed(3)
train_id <- sample(10000, 5000)

train <- Default[train_id, ]
test <- Default[-train_id, ]
test.Default <- test$default
```


```r
glm.fit.1 <- glm(default ~ income + balance, data=train, family = binomial)
```


```r
glm.fit.prob <- predict(glm.fit.1, test, type = "response")
glm.fit.pred <- ifelse(glm.fit.prob>0.5, "Yes", "No")
table(glm.fit.pred, test.Default)
```

```
##             test.Default
## glm.fit.pred   No  Yes
##          No  4828  108
##          Yes   16   48
```

```r
mean(glm.fit.pred != test.Default)
```

```
## [1] 0.0248
```
The validation set error for the first sample is 2.48%.  

Sample 2

```r
set.seed(2)
train_id <- sample(10000, 5000)

train <- Default[train_id, ]
test <- Default[-train_id, ]
test.Default <- test$default
```

```r
glm.fit.2 <- glm(default ~ income + balance, data=train, family = binomial)
```

```r
glm.fit.prob <- predict(glm.fit.2, test, type = "response")
glm.fit.pred <- ifelse(glm.fit.prob>0.5, "Yes", "No")
table(glm.fit.pred, test.Default)
```

```
##             test.Default
## glm.fit.pred   No  Yes
##          No  4811  118
##          Yes   20   51
```

```r
mean(glm.fit.pred != test.Default)
```

```
## [1] 0.0276
```
The validation set error for the second sample is 2.76%.  

Sample 3 

```r
set.seed(1)
train_id <- sample(10000, 5000)

train <- Default[train_id, ]
test <- Default[-train_id, ]
test.Default <- test$default
```

```r
glm.fit.3 <- glm(default ~ income + balance, data=train, family = binomial)
```

```r
glm.fit.prob <- predict(glm.fit.3, test, type = "response")
glm.fit.pred <- ifelse(glm.fit.prob>0.5, "Yes", "No")
table(glm.fit.pred, test.Default)
```

```
##             test.Default
## glm.fit.pred   No  Yes
##          No  4805  115
##          Yes   28   52
```

```r
mean(glm.fit.pred != test.Default)
```

```
## [1] 0.0286
```
The validation set error for the third sample is 2.86%.   

(d) Now consider a logistic regression model that predicts the probability of default using income, balance, and a dummy variable for student. Estimate the test error for this model using the validation set approach. Comment on whether or not including a dummy variable for student leads to a reduction in the test error rate.


```r
set.seed(6)

train_id <- sample(10000, 5000)

train <- Default[train_id, ]
test <- Default[-train_id, ]
test.Default <- test$default
```


```r
glm.fit.4 <- glm(default ~ income + balance + student, data = train, family = binomial)
```


```r
glm.fit.prob <- predict(glm.fit.4, test, type = "response")
glm.fit.pred <- ifelse(glm.fit.prob>0.5, "Yes", "No")
table(glm.fit.pred, test.Default)
```

```
##             test.Default
## glm.fit.pred   No  Yes
##          No  4813  122
##          Yes   16   49
```

```r
mean(glm.fit.pred != test.Default)
```

```
## [1] 0.0276
```
The validation test error is 2.76%, therefore including the student variable did not reduce the test error.

tbc












