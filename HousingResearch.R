library(pastecs)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggpubr)
library(moments)
library(MASS)
library(tidyverse)
library(caret)
library(car)
library(olsrr)
library(leaps)
library(e1071)
library(ModelMetrics)
library(fBasics)

#setwd("C:/Rutgers/Courses/Fall_2020/Regression_Time_Series/Project")

# Load the data
housingdata = read.csv(file = 'kc_house_data.csv', stringsAsFactors = TRUE)
summary(housingdata)
housingdata$yr_renovated[housingdata$yr_renovated==0]= NA

my.summary <- function(x, na.rm=TRUE){
  result <- c(Mean=mean(x, na.rm=na.rm),
              SD=sd(x, na.rm=na.rm),
              Median=median(x, na.rm=na.rm),
              Min=min(x, na.rm=na.rm),
              Max=max(x, na.rm=na.rm), 
              N=length(x))
}
ind <- sapply(housingdata, is.numeric)
tab=data.frame(sapply(housingdata[, ind], my.summary))
write.csv(tab,"Statistics.csv", row.names = TRUE)

#Univariate analysis

plot(density(housingdata$bedrooms, na.rm = TRUE))
plot(density(housingdata$price, na.rm = TRUE))
plot(density(housingdata$bedrooms, na.rm = TRUE))
plot(density(housingdata$bathrooms, na.rm = TRUE))
plot(density(housingdata$sqft_living, na.rm = TRUE))
plot(density(housingdata$sqft_lot, na.rm = TRUE))
plot(density(housingdata$floors, na.rm = TRUE))
plot(density(housingdata$waterfront, na.rm = TRUE))
plot(density(housingdata$view, na.rm = TRUE))
plot(density(housingdata$condition, na.rm = TRUE))
plot(density(housingdata$grade, na.rm = TRUE))
plot(density(housingdata$sqft_above, na.rm = TRUE))
plot(density(housingdata$sqft_basement, na.rm = TRUE))
plot(density(housingdata$yr_built, na.rm = TRUE))
plot(density(housingdata$yr_renovated, na.rm = TRUE))


# Bivariate Analysis
ggpairs(data=housingdata, columns=3:7,mapping = aes(color = "dark green"), axisLabels="show")


## Checking Relationship between price, floors, waterfront, view, condition and grade
ggpairs(data=Training, columns=c(3,8:12),mapping = aes(color = "dark green"),axisLabels="show")


## Checking Relationship between price, yr built, lat and long
ggpairs(data=Training, columns=c(3,15,18,19),mapping = aes(color = "dark green"),axisLabels="show")


#Correlation plot to identify highly correlated predictors.
correlationhouse= cor(housingdata, use="pairwise.complete.obs")
corrplot(correlationhouse, method="circle")

# Skewness test is performed to check for the normal distribution. Observed skewness is greater than 1 for few predictors, 
# suggesting data might not be normally distributed
skewness(housingdata$price)
skewness(housingdata$bedrooms)
skewness(housingdata$bathrooms)
skewness(housingdata$sqft_living)
skewness(housingdata$sqft_lot)
skewness(housingdata$floors)
skewness(housingdata$waterfront)
skewness(housingdata$view)
skewness(housingdata$condition)


#create a linear model. Plot QQ-plot and boxcox test

housingmodel= lm(price ~ bedrooms + bathrooms + sqft_living + 
                   sqft_lot + floors + waterfront + view + condition + grade + 
                   yr_built + yr_renovated + zipcode +
                   lat + long , data = housingdata)
summary(housingmodel)
plot(housingmodel$fitted.values, housingmodel$residuals, xlab = 'fitted', ylab = 'residuals',
     main = 'Heart residual plot')
abline(h=0, col = 'red')
qqnorm(residuals(housingmodel), ylab="Residuals")
qqline(residuals(housingmodel), col = 'blue')
histogram(housingmodel$residuals)


boxcox(housingmodel, lambda = seq(-5,5,0.1))


# Create a log model based on lambda =0 observed in BOxcox test

model_1= lm(log(price) ~bedrooms + bathrooms + sqft_living + 
              sqft_lot + floors + waterfront + view + condition + grade + 
              sqft_above + yr_built + yr_renovated + zipcode + 
              lat + long , data = housingdata)
m1=summary(model_1)

plot(model_1$fitted.values, model_1$residuals, xlab = 'fitted', ylab = 'residuals',
     main = 'Housing residual plot')
abline(h=0, col = 'red')
qqnorm(residuals(model_1), ylab="Residuals")
qqline(residuals(model_1), col = 'blue')
ggplot(data.frame(residuals=model_1$residuals), aes(x=residuals)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2,  fill="grey", )
vif_table=vif(model_1)
write.csv(vif_table,"VIF_Table.csv", row.names = TRUE)


model1= lm(price ~bedrooms,data = housingdata)
summary(model1)

model2= lm(price ~bedrooms+bathrooms,data = housingdata)
summary(model2)

model3= lm(price ~bedrooms+bathrooms+sqft_living,data = housingdata)
summary(model3)

model4= lm(price ~bedrooms+bathrooms+sqft_living+sqft_lot,data = housingdata)
summary(model4)

model5= lm(price ~bedrooms+bathrooms+sqft_living+waterfront,data = housingdata)
summary(model5)

model6= lm(price ~bedrooms+bathrooms+sqft_living+waterfront+view,data = housingdata)
summary(model6)

model7= lm(price ~bedrooms+bathrooms+sqft_living+waterfront+view+condition,data = housingdata)
summary(model7)

model8= lm(price ~bedrooms+bathrooms+sqft_living+waterfront+view+condition+grade,data = housingdata)
summary(model8)

model9= lm(price ~bedrooms+bathrooms+sqft_living+waterfront+view+condition+grade+sqft_above,data = housingdata)
summary(model9)

model10= lm(price ~bedrooms+bathrooms+sqft_living+waterfront+view+condition+grade+yr_built,data = housingdata)
summary(model10)

model11= lm(price ~bedrooms+bathrooms+sqft_living+waterfront+view+condition+grade+yr_built+yr_renovated,data = housingdata)
summary(model11)

model12= lm(price ~bedrooms+bathrooms+sqft_living+waterfront+view+condition+grade+yr_built+zipcode,data = housingdata)
summary(model12)

model13= lm(price ~bedrooms+bathrooms+sqft_living+waterfront+view+condition+grade+yr_built+lat,data = housingdata)
summary(model13)

model14= lm(price ~bedrooms+bathrooms+sqft_living+waterfront+view+condition+grade+yr_built+lat+long,data = housingdata)
summary(model14)

model15= lm(price ~bedrooms+bathrooms+sqft_living+waterfront+view+condition+grade+yr_built+lat,data = housingdata)
summary(model15)

model16= lm(price ~bedrooms+bathrooms+sqft_living+waterfront+grade+yr_built+lat,data = housingdata)
summary(model16)

boxcox(model16,lambda = seq(-5,5,0.1))


bicdata= as.data.frame(subset(housingdata, select=-c(price,date,id,sqft_above,yr_renovated,sqft_living15,sqft_lot15)))
bicdata=data.frame(bicdata)
m1=regsubsets(bicdata, housingdata$price, method = "forward", nvmax=15)
m2=summary(m1)
names(sort(colSums(m2$which[, -1]*1), decreasing=T))
plot(m2$bic, main='forward search: BIC')
which.min(m2$bic)
names(which(m2$which[which.min(m2$bic), ]))
which(diff(m2$bic) > 0)[1]

m11=regsubsets(bicdata, housingdata$price, method = "backward", nvmax=15)
m12=summary(m11)
names(sort(colSums(m12$which[, -1]*1), decreasing=T))
plot(m12$bic, main='forward search: BIC')
which.min(m12$bic)
names(which(m12$which[which.min(m12$bic), ]))
which(diff(rev(m12$bic)) > 0)[1]
names(which(m12$which[(11-which(diff(rev(m12$bic)) > 0)[1]), ]))

model_17 = lm(log(price)~sqft_living+waterfront+view+grade+yr_built+zipcode+lat+long, data = housingdata)
summary(model_17)
# Model Sensitivity

dt = sort(sample(nrow(housingdata), nrow(housingdata)*.7))
train=housingdata[dt,]
test=housingdata[-dt,]
dim(train)
dim(test)

finalmodel= lm(log(price) ~sqft_living+waterfront+view+grade+yr_built+lat+long+zipcode,data = train)
summary(finalmodel)
plot(finalmodel$fitted.values, finalmodel$residuals, xlab = 'fitted', ylab = 'residuals',
     main = 'Housing residual plot')
abline(h=0, col = 'red')

imp= data.frame(varImp(finalmodel))
barplot(t(imp),col ="grey")

predicted=predict.lm(finalmodel,newdata = test,type = "response")
summary(predicted)

actualvalues= data.frame(predicted=predicted, actual=log(test$price))
actualvalues
write.csv(actualvalues,"Validation.csv", row.names = TRUE)

rmse(log(test$price), predicted)