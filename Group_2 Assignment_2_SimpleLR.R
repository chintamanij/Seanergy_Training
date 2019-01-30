##  Assumptions of linear regression

data(airquality)
names(airquality)
model2 = lm(Ozone~Solar.R+Wind+Temp,data=airquality)
model2
summary(model2)


# 1. The regression model is linear in parameters

# i.e ozone = -64.34+0.059(Solar.R)-3.33(Wind)+1.65(Temp)

# So Assumption Assumptions acceptable

# 2. The mean of residuals is zero

mean(model2$residuals)

#output : 7.417701e-17 it is almost zero Assemption is acceptable.

#3. Homoscedasticity of residuals or equal variance

par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(model2)

#In this case, there is a definite pattern noticed.
#So, there is heteroscedasticity. Lets check this on a different model.

mod <- lm(Ozone~Solar.R+Wind+Temp,data=airquality[1:20, ])  #  linear model
plot(mod)


# Assumption 4: The X variables and residuals are uncorrelated

##Do a correlation test on the X variable and the residuals.

cor.test(subset(airquality, select = c(-Month,-Day,-Ozone)), model2$residuals)  # do correlation test 

cor.test((airquality[2:4]),model2$residuals) #issue with x and y values


# Assumption 5: The number of observations must be greater than number of Xs

##This can be directly observed by looking at the data.


# Assumption 6: The variability in X values is positive

var(airquality$Solar.R+airquality$Wind+airquality$Temp,na.rm=T)  

# The variance in the X variable above is much larger than 0. So, this assumption is satisfied.



# Assumption 7: The regression model is correctly specified

# This means that if the Y and X variable has an inverse relationship,S
# the model equation should be specified appropriately: Y=ß1+ß2*(1/X)


#Assumption 8: No perfect multicollinearity

# Using Variance Inflation factor (VIF).
# VIF=1/(1-Rsq) 

# Output :  Solar.R     Wind     Temp 
#           1.095253 1.329070 1.431367 

#The convention is, the VIF should not go more than 4 for any of the X variables. That means we are not letting the RSq of any of the Xs (the model that was built with that X as a response variable and the remaining Xs are predictors) to go more than 75%. => 1/(1-0.75) => 1/0.25 => 4.

# to check all Assumption
#install.packages('gvlma')
library(gvlma)
par(mfrow=c(2,2))  # draw 4 plots in same window
model2=lm(Ozone~Solar.R+Wind+Temp,data=airquality)
gvlma::gvlma(model2)

## Making the changes
mod <- lm(Ozone~Solar.R+Wind+Temp,data=airquality[-c(30, 62, 117), ]) # removing outliers
gvlma::gvlma(mod)



