# Ch. 8 Exercises



7. In the lab, we applied random forests to the `Boston` data using `mtry=6` and using `ntree=25` and `ntree=500`. Create a plot displaying the test error resulting from random forests on this data set for a more comprehensive range of values for mtry and ntree. You can model your plot after Figure 8.10. Describe the results obtained.

```r
library(randomForest)
```

```
## randomForest 4.6-12
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```r
library(MASS)
data(Boston)
attach(Boston)
dim(Boston)
```

```
## [1] 506  14
```


```r
set.seed(134)
#Create train and test samples
train <- sample(1:nrow(Boston), nrow(Boston)/3)

x.train <- Boston[train, -14]
y.train <- Boston[train, 14]
x.test <- Boston[-train, -14]
y.test <- Boston[-train, 14]
```


```r
rf.1 <- randomForest(x=x.train, y=y.train, xtest=x.test, ytest=y.test, mtry=ncol(Boston)-1, ntree=1000)
rf.2 <- randomForest(x=x.train, y=y.train, xtest=x.test, ytest=y.test, mtry=floor((ncol(Boston)-1)/2), ntree=1000)
rf.3 <- randomForest(x=x.train, y=y.train, xtest=x.test, ytest=y.test, mtry=floor(sqrt(ncol(Boston)-1)), ntree=1000)
```


```r
plot(1:1000, rf.1$test$mse, type='l', col='orange', xlab="Number of trees", ylab = "Test MSE", ylim=c(11, 16))
lines(1:1000, rf.2$test$mse, type='l', col="blue")
lines(1:1000, rf.3$test$mse, type='l', col="darkgreen")
legend("topright", legend=c("mtry=p", "mtry=p/2", "mtry=sqrt(p)"), col=c("orange", "blue", "darkgreen"), lty=1, cex=1)
which.min(rf.1$test$mse)
```

```
## [1] 6
```

```r
which.min(rf.2$test$mse)
```

```
## [1] 213
```

```r
which.min(rf.3$test$mse)
```

```
## [1] 52
```

```r
points(which.min(rf.1$test$mse), rf.1$test$mse[6], col="red", cex=2)
points(which.min(rf.2$test$mse), rf.2$test$mse[213], col="red", cex=2)
points(which.min(rf.3$test$mse), rf.3$test$mse[52], col="red", cex=2)
```

![](Ch._8_Exercises_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

We see that the Test MSE is minimized when the number of variables sampled at each node is sqrt(p), and the number of trees is 52. After that, the test MSE increases. We see that the test MSE for mtry=p/2 decreases and stays low up until around 200 trees, before slightly increasing again. Test MSE is highest when mtry=p, and it reaches its lowest test MSE when ntrees=6.

8. In the lab, a classification tree was applied to the `Carseats` data set after converting `Sales` into a qualitative response variable. Now we will seek to predict `Sales` using regression trees and related approaches, treating the response as a quantitative variable.  

```r
library(ISLR)
library(tree)
attach(Carseats)
```

(a) Split the data set into a training set and a test set.

```r
set.seed(456)
train <- sample(1:nrow(Carseats), nrow(Carseats)/3)
Carseats.train <- Carseats[train, ]
Carseats.test <- Carseats[-train, ]
y.test <- Carseats.test$Sales
```

(b) Fit a regression tree to the training set. Plot the tree, and interpret the results. What test error rate do you obtain?  

```r
car.tree <- tree(Sales ~ ., data=Carseats.train)
summary(car.tree)
```

```
## 
## Regression tree:
## tree(formula = Sales ~ ., data = Carseats.train)
## Variables actually used in tree construction:
## [1] "ShelveLoc"   "Age"         "CompPrice"   "Price"       "Advertising"
## [6] "Population" 
## Number of terminal nodes:  16 
## Residual mean deviance:  1.738 = 203.3 / 117 
## Distribution of residuals:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  -3.212  -0.780   0.141   0.000   0.894   3.751
```

```r
plot(car.tree)
text(car.tree, pretty=0)
```

![](Ch._8_Exercises_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


```r
yhat <- predict(car.tree, newdata=Carseats.test)
mean((yhat - y.test)^2)
```

```
## [1] 5.941745
```

The test error is 5.94. 

(c) Use cross-validation in order to determine the optimal level of tree complexity. Does pruning the tree improve the test error rate?  


```r
cv.Carseats <- cv.tree(car.tree)
plot(cv.Carseats$size, cv.Carseats$dev, type='b')
```

![](Ch._8_Exercises_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
prune.Carseats <- prune.tree(car.tree, best=11)
plot(prune.Carseats)
text(prune.Carseats, pretty=0)
```

![](Ch._8_Exercises_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
yhat <- predict(prune.Carseats, Carseats.test)
mean((yhat-y.test)^2)
```

```
## [1] 6.033721
```

In this case, pruning resulted in higher test MSE.

(d) Use the bagging approach in order to analyze this data. What test error rate do you obtain? Use the `importance()` function to determine which variables are most important.  

```r
require(randomForest)
bag.Carseats <- randomForest(Sales ~., data=Carseats.train, mtry=10, importance=TRUE)
```


```r
yhat.bag <- predict(bag.Carseats, newdata=Carseats.test)
mean((yhat.bag-y.test)^2)
```

```
## [1] 3.701408
```

Bagging decreases the test MSE to 3.7.

```r
importance(bag.Carseats)
```

```
##               %IncMSE IncNodePurity
## CompPrice   16.899526    104.453513
## Income       4.293574     72.011911
## Advertising 15.612781    135.328747
## Population   1.521859     40.866301
## Price       28.183233    187.087965
## ShelveLoc   34.393343    252.462028
## Age         10.471261     87.497267
## Education    1.044461     24.177085
## Urban        3.689382     10.681987
## US           1.027006      5.914018
```

```r
varImpPlot(bag.Carseats)
```

![](Ch._8_Exercises_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

The two most important variables are ShelveLoc and Price.  


(e) Use random forests to analyze this data. What test error rate do you obtain? Use the `importance()` function to determine which variables are most important. Describe the effect of m, the number of variables considered at each split, on the error rate obtained.


```r
rf.Carseats <- randomForest(Sales~., data=Carseats.train, mtry=floor((ncol(Carseats)-1)/3),importance=TRUE)
```

```r
yhat.rf <- predict(rf.Carseats, newdata = Carseats.test)
mean((yhat.rf-y.test)^2)
```

```
## [1] 4.567363
```

When using p/3 variables at each node in random forest, we obtain a higher test MSE (4.43) than by bagging.

```r
importance(rf.Carseats)
```

```
##                %IncMSE IncNodePurity
## CompPrice   11.7871290     108.93962
## Income       3.2014315      83.21027
## Advertising 15.3653181     134.79871
## Population  -0.2806728      67.89261
## Price       20.3176311     143.12869
## ShelveLoc   27.7860892     175.70573
## Age          8.9600635      96.17402
## Education   -0.2472758      38.57338
## Urban        0.1395236      11.52172
## US           5.6386770      19.75942
```

```r
varImpPlot(rf.Carseats)
```

![](Ch._8_Exercises_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

The two most important variables are ShelveLoc and Price - the same variables that were most important in bagging.


tbc.











