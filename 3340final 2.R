getwd()
setwd("c:")
Realestate<-read.csv("C:\\Realestate.csv")
head(Realestate)
## NOTE: The author has changed the unit of house price into dollar. Rather than ten thousand
## NOTE: The author has changed the unit of house age into days. Rather than year
## All of the changes of units are for the seek of a good understanding of the linear regression model created.
## The first thing we want to do is to be clear which factor is reasonable to be a predictor.
## We can easily find that "latitude" and "longitude" these two contributors do not vary much over 
## these different house the prove of the statement is at below.
mean_of_latitude=mean(Realestate$X5.latitude)
mean_of_latitude
mean_of_longtitude=mean(Realestate$X6.longitude)
mean_of_longtitude
## we can say that over all the houses they having roughly same latitude and longittude.So, we
## will ignore these two factors. And using other remaining factors as predictors.
lm_Realestate=lm(Y~X1.transaction.date+X2.house.age+X3.distance.to.the.nearest.MRT.station+
                   X4.number.of.convenience.stores,data=Realestate)
summary(lm_Realestate)
## Seeing from the summary of the simple regression model that we created. The most essential
## thing we need to check is the correlation coefficient. We find that with 4predictors, The R^2
## is 0.5553. Now we can see what happening if we put the remaining two predictors into model
lm_Realestate2=lm(Y~X1.transaction.date+X2.house.age+X3.distance.to.the.nearest.MRT.station+
                   X4.number.of.convenience.stores+X5.latitude+X6.longitude,data=Realestate)
summary(lm_Realestate2)
## We surprisingly find that after put these two predictors, the R^2 increased. So it states that
## the conclusion we made before is wrong. As a result we should take all factors into model!
## From the R^2, we know that about 58% of of variation in these predictors can be explained 
## by our model.

## The we can check the regression diagnostic plots for this model
plot(lm_Realestate2)
## The model above provided overall plot among six predictors. And now we can check the fit-level
## of each predictors by using plot and abline command.
##1
price_related_to_date=lm(Y~X1.transaction.date,Realestate)
plot(Y~X1.transaction.date,ylab="price",data=Realestate)
abline(a=coef(price_related_to_date)[1],b=coef(price_related_to_date)[2])
##2
price_related_to_age=lm(Y~X2.house.age,Realestate)
plot(Y~X2.house.age,ylab="price",data=Realestate)
abline(a=coef(price_related_to_age)[1],b=coef(price_related_to_age)[2])
##3
price_related_to_station=lm(Y~X3.distance.to.the.nearest.MRT.station,Realestate)
plot(Y~X3.distance.to.the.nearest.MRT.station,ylab="price",data=Realestate)
abline(a=coef(price_related_to_station)[1],b=coef(price_related_to_station)[2])

##4
price_related_to_stores=lm(Y~X4.number.of.convenience.stores,Realestate)
plot(Y~X4.number.of.convenience.stores,ylab="price",data=Realestate)
abline(a=coef(price_related_to_stores)[1],b=coef(price_related_to_stores)[2])
##5
price_related_to_lat=lm(Y~X5.latitude,Realestate)
plot(Y~X5.latitude,ylab="price",data=Realestate)
abline(a=coef(price_related_to_lat)[1],b=coef(price_related_to_lat)[2])
##6
price_related_to_long=lm(Y~X6.longitude,Realestate)
plot(Y~X6.longitude,ylab="price",data=Realestate)
abline(a=coef(price_related_to_long)[1],b=coef(price_related_to_long)[2])

## The above are the process of prove the correlation coefficient.


## Now we came to the model selection, In statistic we want to detach those unnecessary elements.
## Due to the lack of time we can just analyze four models.

## First one contains all predictors, named full model.
## Y= beta1*X1+...+beta6*X6+ERROR
Full_model=lm(Y~X1.transaction.date+X2.house.age+X3.distance.to.the.nearest.MRT.station+
               X4.number.of.convenience.stores+X5.latitude+X6.longitude,data=Realestate)
summary(Full_model)
## Second one just have an ERROR, called empty model.
Empty_model=lm(Y~1,data=Realestate)
summary(Empty_model)
## Third one would be the model has predictors from odd position.
Odd_model=lm(Y~X1.transaction.date+X3.distance.to.the.nearest.MRT.station+X5.latitude,data=Realestate)
summary(Odd_model)
## Fourth one is the model that just included predictors from even position.
Even_model=lm(Y~X2.house.age+X4.number.of.convenience.stores+X6.longitude,data=Realestate)
summary(Even_model)
## Here we choosing to use the anova test to see the F test and find out which model is non significant
## compared to Full model.
## IF the model is non significant, then we can not rejct the NULL hypothesis 
## and embrace with this model.

anova(Full_model,Empty_model)
anova(Full_model,Odd_model)
anova(Full_model,Even_model)
## Seeing from the f test we find that these three model's are all fail to reject the null hypothesis.
## So, we are continue to use the full model.

## So far, we have already provided the coefficient correlation relationship between factors and dependent
## variable(house price). Besides we are fully demonstrated the relationship of each factors 
## contribution and effects
## To conclude the final model that we created is 
lm_Realestate_final=lm(Y~X1.transaction.date+X2.house.age+X3.distance.to.the.nearest.MRT.station+
                    X4.number.of.convenience.stores+X5.latitude+X6.longitude,data=Realestate)
summary(lm_Realestate_final)
## For the seek of a intuitive understanding we extract the BETA0 and BETA1 to BETA6.
## Now, we can assign any value to the factors and it would return a satisfied result
lm_of_house_price=-1.454e+08+(((5.085e+04)*X1)+((-7.446e+00)*X2)+((-4.409e+01)*X3)+((1.119e+04)*X4)+((2.309e+06)*X5)+((-1.171e+05)*X6))
## where X1=transaction.date,X2=house.age,X3=distance.to.the.nearest.MRT.station,X4=number.of.convenience.stores
## X5=latitude, X6=longitude. And the intercept(BETA) is -1.454e+08.
lm_of_house_price=-1.454e+08+(((5.085e+04)*2012)+((-7.446e+00)*6180)+((-4.409e+01)*368)+((1.119e+04)*3)+((2.309e+06)*25)+((-1.171e+05)*121))
lm_of_house_price
## Above is a example of estimation by our model by taking a real data of house description.
## Overall, that is our model!



