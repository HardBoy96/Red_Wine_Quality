## Reading the data and displaying the summary.

wine <- read.csv("wine.csv", header=TRUE)
install.packages("psych")
library(psych)

## The min, max, median, mean , standard deviation(spread of the data)

summary(wine)
describe(wine)


## Creating a new column for defining which wine is good and bad
## according to market understanding.This is beneficial for classification
## Here, the wines that have aquality greater that 6 are considered as good and 
## the rest are considered to be bad.

wine$good <- ifelse(wine$quality > 6,1,0)
table(wine$quality)

## here good=1 and bad=0

myTable=with(wine,table(wine$good))
myTable
xtabs(~wine$quality+wine$good)

## Fitting Multiple linear regression to the data set

regressor = lm(formula = quality ~ .,
               data = wine)

## Backward elimination of Multiple linear regression

backward <- step(regressor, direction = "backward")

## Mean of acidity among various types/class of wines

aggregate(wine[, c("fixed.acidity"), drop=FALSE], by=list(Quality=wine$quality), mean)

## Standard deviation or spread of acidity data in terms of quality of wine

aggregate(wine[, c("fixed.acidity"), drop=FALSE], by=list(Quality=wine$quality), sd)

## Mean of acidity among Good and bad wines

aggregate(wine[, c("fixed.acidity"), drop=FALSE], by=list(Quality=wine$good), mean)

## Mean citric acid among various qualities of wine

aggregate(wine[, c("citric.acid"), drop=FALSE], by=list(Quality=wine$quality), mean)

## Standard deviation or spread of citric acid data in terms of quality of wine

aggregate(wine[, c("citric.acid"), drop=FALSE], by=list(Quality=wine$quality), sd)

## Mean of citric acid among Good and bad wines

aggregate(wine[, c("citric.acid"), drop=FALSE], by=list(Quality=wine$good), mean)

## Mean of residual sugar among various tpyes/class of wines

aggregate(wine[, c("residual.sugar"), drop=FALSE], by=list(Quality=wine$quality), mean)

## Standard deviation or spread of residual sugar in terms of quality of wine

aggregate(wine[, c("residual.sugar"), drop=FALSE], by=list(Quality=wine$quality), sd)

## Mean of residual sugar among Good and bad wines

aggregate(wine[, c("residual.sugar"), drop=FALSE], by=list(Quality=wine$good), mean)

## Mean of chlorides among various tpyes/class of wines

aggregate(wine[, c("chlorides"), drop=FALSE], by=list(Quality=wine$quality), mean)

## Standard deviation or spread of data in terms of quality of wine

aggregate(wine[, c("chlorides"), drop=FALSE], by=list(Quality=wine$quality), sd)

## Mean of chloride among Good and bad wines

aggregate(wine[, c("chlorides"), drop=FALSE], by=list(Quality=wine$good), mean)

## Mean of alcohol among various tpyes/class of wines

aggregate(wine[, c("alcohol"), drop=FALSE], by=list(Quality=wine$quality), mean)

## Mean of alcohol among Good and bad wines

aggregate(wine[, c("alcohol"), drop=FALSE], by=list(Quality=wine$good), mean)

## Histogram for understanding the distribution

hist(wine$quality, breaks=6, col="red", xlab="Quality Rating,low=1 high=7", main="Quality  Distribution")

hist(wine$good, main = "Good vs Bad wine, Good = 1,Bad = 0", col=c("red","yellow"), xlab="Wine quality")

## Heatmap to better understand correlation

install.packages("ggplot2")
library(ggplot2)
install.packages("reshape2")
library(reshape2)
qplot(x=Var1, y=Var2, data=melt(cor(wine)), fill=value, geom="tile")

## Boxplots of each variables Vs quality

boxplot(wine$alcohol~wine$quality, main="Alcohol vs Quality", xlab="Quality",ylab="Alcohol" ,horizontal = FALSE,col="red")

boxplot(wine$sulphates~wine$quality, main="Sulphates vs Quality", xlab="Quality",ylab="Sulphates" ,horizontal = FALSE,col="pink")

boxplot(wine$pH~wine$quality, main="PH vs Quality", xlab="Quality",ylab="PH" ,horizontal = FALSE,col="blue")

boxplot(wine$density~wine$quality, main="Density vs Quality", xlab="Quality",ylab="Density" ,horizontal = FALSE,col="green")

boxplot(wine$total.sulfur.dioxide~wine$quality, main="Total_sulfur_dioxide vs Quality", xlab="Quality",ylab="Total_sulfur_dioxide" ,horizontal = FALSE,col="cyan")

boxplot(wine$free.sulfur.dioxide~wine$quality, main="Free_sulfur_dioxide vs Quality", xlab="Quality",ylab="Free_sulfur_dioxide" ,horizontal = FALSE,col="brown")

boxplot(wine$chlorides~wine$quality, main="Chlorides vs Quality", xlab="Quality",ylab="Chlorides" ,horizontal = FALSE,col="yellow")

boxplot(wine$residual.sugar~wine$quality, main="Resuidal_sugar vs Quality", xlab="Quality",ylab="Resuidal_sugar" ,horizontal = FALSE,col="gray")

boxplot(wine$citric.acid~wine$quality, main="Citric_acid vs Quality", xlab="Quality",ylab="Citric_acid" ,horizontal = FALSE,col="darkgreen")

boxplot(wine$volatile.acidity~wine$quality, main="Volatile_acidity vs Quality", xlab="Quality",ylab="Volatile_acidity" ,horizontal = FALSE,col="burlywood")

boxplot(wine$fixed.acidity~wine$quality, main="Fixed_acidity vs Quality", xlab="Quality",ylab="Fixed_acidity" ,horizontal = FALSE,col="chocolate")

## Correlation

install.packages("corrplot")
library(corrplot)
corrplot(cor(wine), method = "square")

## Correlation tests

cor.test(wine$alcohol,wine$quality)
cor.test(wine$sulphates,wine$quality)
cor.test(wine$pH,wine$quality)
cor.test(wine$chlorides,wine$quality)
cor.test(wine$volatile.acidity,wine$quality)
cor.test(wine$citric.acid,wine$quality)
cor.test(wine$free.sulfur.dioxide,wine$quality)
cor.test(wine$total.sulfur.dioxide,wine$quality)

## Correlarion T-tests

t.test(wine$alcohol~wine$good)
t.test(wine$sulphates~wine$good)
t.test(wine$pH~wine$good)
t.test(wine$chlorides~wine$good)
t.test(wine$volatile.acidity~wine$good)
t.test(wine$citric.acid,wine$quality)
t.test(wine$free.sulfur.dioxide,wine$quality)
t.test(wine$total.sulfur.dioxide,wine$quality)