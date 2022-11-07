setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#1 
mData = read.csv('./AmesHousing.csv')
str(mData)
# 2 
# getting rows and cols count
length(mData) # columns
nrow(mData) # rows

library(dplyr)
nData = mData %>% select(where(is.numeric))

for(i in 1:ncol(nData)){
  nData[is.na(nData[,i]), i] <- mean(nData[,i], na.rm = TRUE)
}

psych::describe(nData[c('SalePrice','X1st.Flr.SF','Garage.Cars','Garage.Area','Gr.Liv.Area','Overall.Qual','Overall.Cond')])
options(scipen=999)
hist(nData$SalePrice,xlab='House Sale Price',main='Ames Iowa House Prices',col='red',border='white')
hist(nData$Overall.Qual,xlab='Overall Quality',main='Quality of Houses of Ames',col='cyan4',border='white')

cor(nData['SalePrice'],nData[1:38])

library(ggcorrplot) 
ggcorrplot(cor(nData),  insig = "blank", lab_size=2,type='lower',tl.cex = 8)
ggcorrplot(cor(nData['SalePrice'],nData[1:39]), lab=TRUE,lab_size=1.5, insig = "blank", tl.cex=8)

library(ggplot2)
options(scipen=999)
# highest
ggplot(nData, aes(x=Gr.Liv.Area, y=SalePrice)) +
  geom_point(size=2, shape=1)
# lowest
ggplot(nData, aes(x=Yr.Sold, y=SalePrice)) +
  geom_point(size=2, shape=1)
# 0.5
ggplot(nData, aes(x=Year.Built, y=SalePrice)) +
  geom_point(size=2, shape=1)

# 7
par(mfrow=c(1,2))
# a
SL_lmodel = lm(nData$SalePrice ~ nData$Gr.Liv.Area)
plot(nData$SalePrice ~ nData$Gr.Liv.Area,xlab="Above Ground Level (Sq.Ft)",ylab="Sales Price",main='House Price ~ Sq.ft Above Ground Level')
abline(a= SL_lmodel$coefficients[1],b=SL_lmodel$coefficients[2])
summary(SL_lmodel)

# b 
ML_lmodel = lm(nData$SalePrice ~ nData$Gr.Liv.Area + nData$X1st.Flr.SF + nData$Total.Bsmt.SF)
plot(nData$SalePrice ~ nData$Gr.Liv.Area,xlab="Above Ground Level (Sq.Ft)",ylab="Sales Price",main='Price ~ Sq.ft Above Ground + Basement + 1st Floor')
abline(a= ML_lmodel$coefficients[1], b=ML_lmodel$coefficients[2]+ML_lmodel$coefficients[3]+ML_lmodel$coefficients[4])
summary(ML_lmodel)

par(mfrow=c(1,1))
plot(SL_lmodel)
plot(ML_lmodel)


library(car)
vif(ML_lmodel)

# 11
outlierTest(ML_lmodel)
par(mfrow=c(1,1))
hat.plot <- function(ML_lmodel) {
  p <- length(coefficients(ML_lmodel))
  n <- length(fitted(ML_lmodel))
  plot(hatvalues(ML_lmodel), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(ML_lmodel), names(hatvalues(ML_lmodel)))
}
hat.plot(ML_lmodel)

# 12
cooksValue = cooks.distance(ML_lmodel)
influential_points = cooksValue[(cooksValue>3*mean(cooksValue,na.rm=T))]
names = names(influential_points)
outliers = nData[names,]
cleanedData = nData%>%anti_join(outliers)

#qqnorm()
par(mfrow=c(1,1))
WO_ML_lmodel<- lm(cleanedData$SalePrice~cleanedData$Gr.Liv.Area+cleanedData$X1st.Flr.SF +cleanedData$Total.Bsmt.SF)
plot(cleanedData$SalePrice ~ cleanedData$Gr.Liv.Area)
abline(a=WO_ML_lmodel$coefficients[1],b=WO_ML_lmodel$coefficients[1]+WO_ML_lmodel$coefficients[2])
summary(WO_ML_lmodel)
plot(WO_ML_lmodel)

#13
library(MASS)
stepAIC(WO_ML_lmodel,direction = "both")
par(mfrow=c(1,1))
library(leaps)
leaps <- regsubsets(SalePrice~Gr.Liv.Area+X1st.Flr.SF +Total.Bsmt.SF + Garage.Area + Garage.Cars,
                    data=withoutOutliers,nbest=4)
plot(leaps,scale="adjr2",ylab='Adjusted R-Square Value')

