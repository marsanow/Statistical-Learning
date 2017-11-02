# Ch. 4 Exercises



10. This question should be answered using the Weekly data set, which is part of the ISLR package. This data is similar in nature to the Smarket data from this chapter’s lab, except that it contains 1,089 weekly returns for 21 years, from the beginning of 1990 to the end of 2010.

```r
library(ISLR)
data("Weekly")
attach(Weekly)
```
(a) Produce some numerical and graphical summaries of the Weekly data. Do there appear to be any patterns?

```r
dim(Weekly)
```

```
## [1] 1089    9
```

```r
names(Weekly)
```

```
## [1] "Year"      "Lag1"      "Lag2"      "Lag3"      "Lag4"      "Lag5"     
## [7] "Volume"    "Today"     "Direction"
```

```r
summary(Weekly)
```

```
##       Year           Lag1               Lag2               Lag3         
##  Min.   :1990   Min.   :-18.1950   Min.   :-18.1950   Min.   :-18.1950  
##  1st Qu.:1995   1st Qu.: -1.1540   1st Qu.: -1.1540   1st Qu.: -1.1580  
##  Median :2000   Median :  0.2410   Median :  0.2410   Median :  0.2410  
##  Mean   :2000   Mean   :  0.1506   Mean   :  0.1511   Mean   :  0.1472  
##  3rd Qu.:2005   3rd Qu.:  1.4050   3rd Qu.:  1.4090   3rd Qu.:  1.4090  
##  Max.   :2010   Max.   : 12.0260   Max.   : 12.0260   Max.   : 12.0260  
##       Lag4               Lag5              Volume       
##  Min.   :-18.1950   Min.   :-18.1950   Min.   :0.08747  
##  1st Qu.: -1.1580   1st Qu.: -1.1660   1st Qu.:0.33202  
##  Median :  0.2380   Median :  0.2340   Median :1.00268  
##  Mean   :  0.1458   Mean   :  0.1399   Mean   :1.57462  
##  3rd Qu.:  1.4090   3rd Qu.:  1.4050   3rd Qu.:2.05373  
##  Max.   : 12.0260   Max.   : 12.0260   Max.   :9.32821  
##      Today          Direction 
##  Min.   :-18.1950   Down:484  
##  1st Qu.: -1.1540   Up  :605  
##  Median :  0.2410             
##  Mean   :  0.1499             
##  3rd Qu.:  1.4050             
##  Max.   : 12.0260
```

```r
pairs(Weekly[, -9])
```

![](Ch._4_Exercises_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
round(cor(Weekly[, -9]), 4)
```

```
##           Year    Lag1    Lag2    Lag3    Lag4    Lag5  Volume   Today
## Year    1.0000 -0.0323 -0.0334 -0.0300 -0.0311 -0.0305  0.8419 -0.0325
## Lag1   -0.0323  1.0000 -0.0749  0.0586 -0.0713 -0.0082 -0.0650 -0.0750
## Lag2   -0.0334 -0.0749  1.0000 -0.0757  0.0584 -0.0725 -0.0855  0.0592
## Lag3   -0.0300  0.0586 -0.0757  1.0000 -0.0754  0.0607 -0.0693 -0.0712
## Lag4   -0.0311 -0.0713  0.0584 -0.0754  1.0000 -0.0757 -0.0611 -0.0078
## Lag5   -0.0305 -0.0082 -0.0725  0.0607 -0.0757  1.0000 -0.0585  0.0110
## Volume  0.8419 -0.0650 -0.0855 -0.0693 -0.0611 -0.0585  1.0000 -0.0331
## Today  -0.0325 -0.0750  0.0592 -0.0712 -0.0078  0.0110 -0.0331  1.0000
```

There seems to be a positive relationship between Volume and Year. The correlation index is 0.8419, which is pretty high, and we can also see the relationship from the paired scatterplots (although the relationship doesn't seem to be linear).

(b) Use the full data set to perform a logistic regression with Direction as the response and the five lag variables plus Volume as predictors. Use the summary function to print the results. Do any of the predictors appear to be statistically significant? If so, which ones?

```r
glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)
summary(glm.fit)
```

```
## 
## Call:
## glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
##     Volume, family = binomial, data = Weekly)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.6949  -1.2565   0.9913   1.0849   1.4579  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept)  0.26686    0.08593   3.106   0.0019 **
## Lag1        -0.04127    0.02641  -1.563   0.1181   
## Lag2         0.05844    0.02686   2.175   0.0296 * 
## Lag3        -0.01606    0.02666  -0.602   0.5469   
## Lag4        -0.02779    0.02646  -1.050   0.2937   
## Lag5        -0.01447    0.02638  -0.549   0.5833   
## Volume      -0.02274    0.03690  -0.616   0.5377   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1496.2  on 1088  degrees of freedom
## Residual deviance: 1486.4  on 1082  degrees of freedom
## AIC: 1500.4
## 
## Number of Fisher Scoring iterations: 4
```
The Lag2 variable seems to be statistically significant on a 0.05 significance level with a p-value of 0.0296.

(c) Compute the confusion matrix and overall fraction of correct predictions. Explain what the confusion matrix is telling you about the types of mistakes made by logistic regression.

```r
glm.probs <- predict(glm.fit, type="response")
glm.pred <- ifelse(glm.probs>0.5, "Up", "Down")
table(glm.pred, Direction)
```

```
##         Direction
## glm.pred Down  Up
##     Down   54  48
##     Up    430 557
```

```r
# Overall fraction of correct predictions
(54+557)/1089
```

```
## [1] 0.5610652
```

```r
# Fraction of false positives
430/(430+557)
```

```
## [1] 0.4356636
```

```r
# Fraction of false negatives
48/(54+48)
```

```
## [1] 0.4705882
```

```r
# When market is "Up"
557/(557+48)
```

```
## [1] 0.9206612
```

```r
# When market is "Down"
54/(54+430)
```

```
## [1] 0.1115702
```
When market is "Up", logistic regression predicts correctly 92% of the time, while when the market is "Down", it predicts correctly only 11% of the time.

(d) Now fit the logistic regression model using a training data period from 1990 to 2008, with Lag2 as the only predictor. Compute the confusion matrix and the overall fraction of correct predictions for the held out data (that is, the data from 2009 and 2010).

```r
train <- (Year<=2008)
length(train)
```

```
## [1] 1089
```

```r
test <- Weekly[!train, ]
test.Direction <- Direction[!train]

glm.fit_1 <- glm(Direction ~ Lag2, data=Weekly, family=binomial, subset = train)
glm.probs_1 <- predict(glm.fit_1, test, type="response")
glm.pred_1 <- ifelse(glm.probs_1>0.5, "Up", "Down")
table(glm.pred_1, test.Direction)
```

```
##           test.Direction
## glm.pred_1 Down Up
##       Down    9  5
##       Up     34 56
```

```r
# Overall fraction of correct predicitons
(9+56)/104
```

```
## [1] 0.625
```
The results are much better than before. The % of correct predictions increased to 62.5%, while using train and test datasets to account for overfitting.  

```r
# When market is up
56/(56+5)
```

```
## [1] 0.9180328
```

```r
# When market is down
9/(9+34)
```

```
## [1] 0.2093023
```

When the market is up, the algorithm predicts correctly 91.8% of the time.
When the market is down, that number goes down to 20.9% of the time.

(e) Repeat (d) using LDA.

```r
library(MASS)
lda.fit <- lda(Direction ~ Lag2, data=Weekly, subset=train)
lda.fit
```

```
## Call:
## lda(Direction ~ Lag2, data = Weekly, subset = train)
## 
## Prior probabilities of groups:
##      Down        Up 
## 0.4477157 0.5522843 
## 
## Group means:
##             Lag2
## Down -0.03568254
## Up    0.26036581
## 
## Coefficients of linear discriminants:
##            LD1
## Lag2 0.4414162
```

```r
lda.probs <- predict(lda.fit, test)
lda.class <- lda.probs$class
table(lda.class, test.Direction)
```

```
##          test.Direction
## lda.class Down Up
##      Down    9  5
##      Up     34 56
```
The results are the same as with logistic regression.

(f) Repeat (d) using QDA.  

```r
qda.fit <- qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.fit
```

```
## Call:
## qda(Direction ~ Lag2, data = Weekly, subset = train)
## 
## Prior probabilities of groups:
##      Down        Up 
## 0.4477157 0.5522843 
## 
## Group means:
##             Lag2
## Down -0.03568254
## Up    0.26036581
```

```r
qda.class <- predict(qda.fit, test)$class
table(qda.class, test.Direction)
```

```
##          test.Direction
## qda.class Down Up
##      Down    0  0
##      Up     43 61
```

```r
# Overall fraction of correct predicitons
61/(61+43)
```

```
## [1] 0.5865385
```
Quadratic Discriminant Analysis model is correct 58.7% of the time, by predicting "Up" 100% of the time, and "Down" 0% of the time. 

(g) Repeat (d) using KNN with K = 1.


```r
library(class)
train.X <- as.matrix(Weekly[Weekly$Year <= '2008', 'Lag2'])
test.X <- as.matrix(Weekly[Weekly$Year > '2008', 'Lag2'])
train.Direction <- Direction[train]
```


```r
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, test.Direction)
```

```
##         test.Direction
## knn.pred Down Up
##     Down   21 30
##     Up     22 31
```

```r
# Overall fraction of correct predicitons
(31+21)/104
```

```
## [1] 0.5
```
KNN is exactly as good as a random guess when k=1.

```r
# Fraction of false positives
22/(31+22)
```

```
## [1] 0.4150943
```

```r
# Fraction of false negatives
30/(30+21)
```

```
## [1] 0.5882353
```

```r
# When market is "Up"
31/(31+30)
```

```
## [1] 0.5081967
```

```r
# When market is "Down"
21/(21+22)
```

```
## [1] 0.4883721
```
Based on these percentages, KNN predicts about the same for when market is "Up" or "Down", which is as good as a random guess.  

(h) Which of these methods appears to provide the best results on
this data?
The best overall model is the logistic regression model, which was correct 62.5% of the time.  

(i) Experiment with different combinations of predictors, including possible transformations and interactions, for each of the methods. Report the variables, method, and associated confusion matrix that appears to provide the best results on the held out data. Note that you should also experiment with values for K in the KNN classifier.

KNN with k=3

```r
train.X <- as.matrix(Weekly[Weekly$Year <= '2008', 'Lag2'])
test.X <- as.matrix(Weekly[Weekly$Year > '2008', 'Lag2'])
train.Direction <- Direction[train]
```


```r
set.seed(2)
knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, test.Direction)
```

```
##         test.Direction
## knn.pred Down Up
##     Down   15 19
##     Up     28 42
```

```r
# Overall fraction of correct predicitons
(42+15)/104
```

```
## [1] 0.5480769
```

```r
# When market is "Up"
42/(19+42)
```

```
## [1] 0.6885246
```

```r
# When market is "Down"
15/(15+28)
```

```
## [1] 0.3488372
```
When market is up, the algorithm predicts correctly 68.9% of the time.
When the market is down, 34.9% of the time.

```r
set.seed(3)
knn.pred <- knn(train.X, test.X, train.Direction, k=2)
table(knn.pred, test.Direction)
```

```
##         test.Direction
## knn.pred Down Up
##     Down   21 24
##     Up     22 37
```

```r
# Overall fraction of correct predicitons
(37+21)/104
```

```
## [1] 0.5576923
```


```r
# When market is "Up"
37/(24+37)
```

```
## [1] 0.6065574
```

```r
# When market is "Down"
21/(21+22)
```

```
## [1] 0.4883721
```
The model is correct 55.8% of the time, classifying the variable "Up" correctly 60.7% of the time, and the variable "Down" 48.8% of the time.

Increasing the k value will increase the gap between the two variables - the percentage of correctly classified "Up" values will increase, and the percentage of "Down" values will decrease.

tbc






