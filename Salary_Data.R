library(readr)
Salary_Data <- read_csv("C:/Users/Factory/Desktop/Salary_Data.csv")
View(Salary_Data)
summary(Salary_Data)
plot(Salary_Data$YearsExperience, Salary_Data$Salary)  # plot(X,Y)
#From plot we can say that data is linearity is there,strength is moderate (sub to check r value),
#& Direction is positive
attach(Salary_Data)
#Correlation between output to input
cor(YearsExperience , Salary)   
#from value of correlation coe.(r) we can say that very good correlation between o/p & i/p
# Simple Linear Regression model
reg <- lm(Salary~ YearsExperience) # lm(Y ~ X)
#Summary of regression model
summary(reg)
reg$fitted.values
reg$residuals
pred <- predict(reg)
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
hist(reg$residuals) 
sqrt(sum(reg$residuals^2)/nrow(Salary_Data))  #RMSE
sqrt(mean(reg$residuals^2))
#interval for 5% of confidence
confint(reg,level=0.95)
predict(reg,interval="predict")
library(ggplot2)
ggplot(data = Salary_Data, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Salary_Data, aes(x=YearsExperience, y=pred))

