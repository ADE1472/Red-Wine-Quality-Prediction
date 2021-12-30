#Importing the dataset
r_wine<- read.table('winequality-red.csv', header = TRUE, sep = ';')
head(r_wine)
str(r_wine)
summary(r_wine)

#Library needed function
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(reshape2)
library(GGally)
library(gridExtra)

#Let's check if the data has null values
is.na(r_wine)
sum(is.na(r_wine))

#Engineering the dataset:
#Quality variable has to be transformed from integer to a factor in order for me to 
#create a new factored variable feature named RATING

r_wine$quality<- factor(r_wine$quality, ordered = T)
r_wine$rating<- ifelse(r_wine$quality <= 4, 'Bad', 
                       ifelse(r_wine$quality <=6, 'Normal', 'Good'))

r_wine$rating<- ordered(r_wine$rating, levels= c('Bad', 'Normal', 'Good'))
glimpse(r_wine)
summary(r_wine)

#visualization

#Univarate and Multivate plots for some selected features

#PH
pH <- c(rep(65, times=5), rep(25, times=5), rep(35, times=10), rep(45, times=4))
hist(pH, prob=TRUE, col="grey")
lines(density(pH), col="blue", lwd=2)
lines(density(pH, adjust=2), lty="dotted", col="red", lwd=2)

#PH density plot
r_wine$pH <- c(rep(65, times=5), rep(25, times=5), rep(35, times=10), rep(45, times=4))
hist(r_wine$pH, prob=TRUE, col="grey")
lines(density(r_wine$pH), col="blue", lwd=2)
lines(density(r_wine$pH, adjust=2), lty="dotted", col="red", lwd=2)


#Fixed.acidity density plot
r_wine$fixed.acidity <- c(rep(65, times=5), rep(25, times=5), rep(35, times=10), rep(45, times=4))
hist(r_wine$fixed.acidity, prob=TRUE, col="grey")
lines(density(r_wine$fixed.acidity), col="blue", lwd=2)
lines(density(r_wine$fixed.acidity, adjust=2), lty="dotted", col="red", lwd=2)

#Alcohol density plot
r_wine$alcohol <- c(rep(65, times=5), rep(25, times=5), rep(35, times=10), rep(45, times=4))
hist(r_wine$alcohol, prob=TRUE, col="grey")
lines(density(r_wine$alcohol), col="blue", lwd=2)
lines(density(r_wine$alcohol, adjust=2), lty="dotted", col="red", lwd=2)

#Chlorides density plot
r_wine$chlorides <- c(rep(65, times=5), rep(25, times=5), rep(35, times=10), rep(45, times=4))
hist(r_wine$chlorides, prob=TRUE, col="grey")
lines(density(r_wine$chlorides), col="blue", lwd=2)
lines(density(r_wine$chlorides, adjust=2), lty="dotted", col="red", lwd=2)

#Density density plot
r_wine$density <- c(rep(65, times=5), rep(25, times=5), rep(35, times=10), rep(45, times=4))
hist(r_wine$density, prob=TRUE, col="grey")
lines(density(r_wine$density), col="blue", lwd=2)
lines(density(r_wine$density, adjust=2), lty="dotted", col="red", lwd=2)

#Total sulfur dioxide
r_wine$total.sulfur.dioxide <- c(rep(65, times=5), rep(25, times=5), rep(35, times=10), rep(45, times=4))
hist(r_wine$total.sulfur.dioxide, prob=TRUE, col="grey")
lines(density(r_wine$total.sulfur.dioxide), col="blue", lwd=2)
lines(density(r_wine$total.sulfur.dioxide, adjust=2), lty="dotted", col="red", lwd=2)

#Residual sugar density plot
residual.sugar <- c(rep(65, times=5), rep(25, times=5), rep(35, times=10), rep(45, times=4))
hist(residual.sugar, prob=TRUE, col="grey")
lines(density(residual.sugar), col="blue", lwd=2)
lines(density(residual.sugar, adjust=2), lty="dotted", col="red", lwd=2)


#Multivariate plot for some selected features
ggplot(data= r_wine)+geom_point(mapping = aes(x=sulphates,y=quality, colour= rating))
ggplot(data= r_wine)+geom_point(mapping = aes(x=free.sulfur.dioxide, y=quality, colour= rating))

ggplot(data=r_wine)+geom_smooth(mapping = aes(x=residual.sugar,y=quality),colour="green") + 
  geom_abline() + facet_wrap( ~rating)

#bivarate plot
#Fixed Acidity boxplot
boxplot(fixed.acidity~quality, data = r_wine, 
        col= (c('blue', 'red', 'green', 'gold', 'grey', 'pink')),
       main= 'Boxplot of Quality against fixed.acidity')

#Residual Sugar
boxplot(residual.sugar~quality, data = r_wine, 
        col= (c('blue', 'red', 'green', 'gold', 'grey', 'pink')),
        main= 'Boxplot of Quality against Residual Sugar')

#chlorides
boxplot(chlorides~quality, data = r_wine, 
        col= (c('blue', 'red', 'green', 'gold', 'grey', 'pink')),
        main= 'Boxplot of Quality against Chlorides')

#Total Sulfur
boxplot(total.sulfur.dioxide~quality, data = r_wine, 
        col= (c('blue', 'red', 'green', 'gold', 'grey', 'pink')),
        main= 'Boxplot of Quality against total S02')

#PH
boxplot(pH~quality, data = r_wine, 
        col= (c('blue', 'red', 'green', 'gold', 'grey', 'pink')),
        main= 'Boxplot of Quality against pH')

#Alcohol
boxplot(alcohol~quality, data = r_wine, 
        col= (c('blue', 'red', 'green', 'gold', 'grey', 'pink')),
        main= 'Boxplot of Quality against Alcohol')


#Correlation plots
install.packages("corrplot")
library(corrplot)
library(ggcorrplot)
library(ggplot2)

correlation<-cor(r_wine[, 1:11])
corrplot(correlation, order = "hclust", tl.cex = 0.7)
ggcorrplot(correlation)
ggcorrplot(correlation, hc.order = TRUE, type = "lower", lab = TRUE)


redWine<- read.table('winequality-red.csv', header = TRUE, sep = ';')
rwCorr<-cor(redWine[, 1:12])
corrplot(rwCorr, order = "hclust", tl.cex = 0.7)
ggcorrplot(rwCorr)
ggcorrplot(rwCorr, hc.order = TRUE, lab = TRUE)





wine<- dplyr::select_if(r_wine, is.numeric)
C<- cor(wine, use = "complete.obs")
round(C,2)
corrplot(C)


install.packages("polycor")
library(polycor)
corr<- hetcor(r_wine[, 1:12])
corrplot(corr)


#Modeling dataset
#Separating data into testing and training set
set.seed(123)
sep<- sample(nrow(redWine), 0.8*nrow(redWine))
train<- redWine[sep, ]
test<- redWine[-sep, ]
#This separate the dataset into 80% for train and 20% for test

#Building the model
#Multiple linear regression
Rmodel=lm(formula = quality~., data = train)
summary(Rmodel)
par(mfrow= c(2,2))
plot(Rmodel)


#test for multicollinearity
library(mctest)
independent<- redWine[, 1:11]
imcdiag(Rmodel)

#Since fixed.acidity and density has the highest MC, let's remove them 
Rmodel2=lm(formula = quality~volatile.acidity+ citric.acid+ residual.sugar+
             chlorides+ free.sulfur.dioxide+ total.sulfur.dioxide+ pH+ 
             sulphates+ alcohol, data = train)
summary(Rmodel2)
par(mfrow= c(2,2))
plot(Rmodel2)
#Conclusion: Model 2 is not statistically significant. 

#Removing variables (residual sugar, chlorides &total SO2) with low correlation against quality 
Rmodel3=lm(formula = quality~fixed.acidity+ volatile.acidity+ citric.acid+ 
             free.sulfur.dioxide+ density+ pH+ sulphates+ alcohol, data = train)
summary(Rmodel3)
par(mfrow= c(2,2))
plot(Rmodel3)
#Conclusion: model 3 is not statically significant because some var. p-values> 0.05 


#Removing all variables that has presences of MC
Rmodel4=lm(formula = quality~volatile.acidity+ chlorides+ total.sulfur.dioxide+ 
             pH+ sulphates+ alcohol, data = train)
summary(Rmodel4)
par(mfrow= c(2,2))
plot(Rmodel4)
#Conclusion: finally i got the best regression model  

#using model4 to predict test data
pred= predict(Rmodel4, newdata = test[-12], interval = 'confidence')
head(pred)
summary(pred)


#Metric performance
#Mean absolute error
mae <- function(error) { mean(abs(error)) }
mae(Rmodel4$residuals)

#Root mean square error
RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(Rmodel4$residuals)



#Decision Tree
library('rpart')
DTmodel<- rpart(quality~., data= train, method = 'class')
summary(DTmodel)
rpart.plot::rpart.plot(DTmodel, extra = 'auto')
printcp(DTmodel)

#Using Decision tree model to predict test data
pred2= predict(DTmodel, newdata = test[-12], type = 'class')
summary(pred2)
cMatrix= table(test[, 12], pred2)
cMatrix


#Random forest tree
library(randomForest)
library(mlbench)
RFmodel= randomForest(x=train[-12], y = train$quality)
print(RFmodel)
plot(RFmodel)
varImpPlot(RFmodel,type = 2)

#Obtain MSE and RMSE
RFmodel$mse[length(RFmodel$mse)]
sqrt(RFmodel$mse[length(RFmodel$mse)])

#Using Random forest tree model errors 
#on train data to predict test data
pred3<- predict(RFmodel, newdata = test[-12], 
                type = 'class')
mean(abs(test$quality -pred3)) #MAE
sqrt(mean((test$quality -pred3)^2)) #RMSE


cMatrix= table(test[, 12], pred3)
cMatrix

table(pred3, test$quality)




